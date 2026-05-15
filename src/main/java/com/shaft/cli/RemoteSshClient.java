package com.shaft.cli;

import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.shaft.cli.internal.JschSessionFactory;
import com.shaft.cli.internal.RemoteCommandBundler;
import com.shaft.cli.internal.SshConnectionOptions;
import com.shaft.cli.internal.SshOutputRedactor;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * One JSch {@link Session} (default) with multiple {@code exec} channels. Day-to-day tests usually call
 * {@link com.shaft.driver.SHAFT.CLI.Terminal} instead; use this when you need the builder, SFTP, or port forwards.
 *
 * @see com.shaft.driver.SHAFT.CLI#remoteSsh(String, int, String, String, String)
 */
public final class RemoteSshClient implements AutoCloseable {

    private final String sshHostName;
    private final int sshPortNumber;
    private final String sshUsername;
    private final String sshKeyFileFolderName;
    private final String sshKeyFileName;
    private final String dockerName;
    private final String dockerUsername;
    private final SshSessionPolicy sessionPolicy;
    private final String password;
    private final String knownHostsPathOverride;
    private final String keyPassphrase;

    private final List<Integer> activeLocalForwardPorts = new ArrayList<>();
    private final List<Integer> activeRemoteForwardPorts = new ArrayList<>();

    private Session session;

    public RemoteSshClient(String sshHostName, int sshPortNumber, String sshUsername,
                           String sshKeyFileFolderName, String sshKeyFileName) {
        this(sshHostName, sshPortNumber, sshUsername, sshKeyFileFolderName, sshKeyFileName, "", "", SshSessionPolicy.REUSE_SESSION,
                null, null, null);
    }

    public RemoteSshClient(String sshHostName, int sshPortNumber, String sshUsername,
                           String sshKeyFileFolderName, String sshKeyFileName, SshSessionPolicy sessionPolicy) {
        this(sshHostName, sshPortNumber, sshUsername, sshKeyFileFolderName, sshKeyFileName, "", "", sessionPolicy, null, null, null);
    }

    public RemoteSshClient(String sshHostName, int sshPortNumber, String sshUsername,
                           String sshKeyFileFolderName, String sshKeyFileName,
                           String dockerName, String dockerUsername) {
        this(sshHostName, sshPortNumber, sshUsername, sshKeyFileFolderName, sshKeyFileName, dockerName, dockerUsername,
                SshSessionPolicy.REUSE_SESSION, null, null, null);
    }

    public RemoteSshClient(String sshHostName, int sshPortNumber, String sshUsername,
                           String sshKeyFileFolderName, String sshKeyFileName,
                           String dockerName, String dockerUsername, SshSessionPolicy sessionPolicy) {
        this(sshHostName, sshPortNumber, sshUsername, sshKeyFileFolderName, sshKeyFileName, dockerName, dockerUsername,
                sessionPolicy, null, null, null);
    }

    private RemoteSshClient(String sshHostName, int sshPortNumber, String sshUsername,
                            String sshKeyFileFolderName, String sshKeyFileName,
                            String dockerName, String dockerUsername, SshSessionPolicy sessionPolicy,
                            String password, String knownHostsPathOverride, String keyPassphrase) {
        this.sshHostName = sshHostName == null ? "" : sshHostName;
        this.sshPortNumber = sshPortNumber;
        this.sshUsername = sshUsername;
        this.sshKeyFileFolderName = sshKeyFileFolderName;
        this.sshKeyFileName = sshKeyFileName;
        this.dockerName = dockerName == null ? "" : dockerName;
        this.dockerUsername = dockerUsername == null ? "" : dockerUsername;
        this.sessionPolicy = sessionPolicy == null ? SshSessionPolicy.REUSE_SESSION : sessionPolicy;
        this.password = password;
        this.knownHostsPathOverride = knownHostsPathOverride;
        this.keyPassphrase = keyPassphrase;
    }

    public static Builder builder(String host, int port, String username) {
        return new Builder(host, port, username);
    }

    public void connect() throws JSchException {
        if (session != null && session.isConnected()) {
            return;
        }
        disconnectQuietly();
        session = JschSessionFactory.connect(buildOptions());
        ReportManager.logDiscrete("Successfully created SSH Session.");
    }

    public Session getJschSession() throws JSchException {
        connect();
        return session;
    }

    public SshSftp openSftp() throws JSchException {
        connect();
        ChannelSftp ch = (ChannelSftp) session.openChannel("sftp");
        ch.connect();
        return new SshSftp(ch);
    }

    public SshLocalPortForward forwardLocalPort(int localPort, String remoteHost, int remotePort) throws JSchException {
        connect();
        int assigned = session.setPortForwardingL(localPort, remoteHost, remotePort);
        activeLocalForwardPorts.add(assigned);
        return new SshLocalPortForward(session, assigned, () -> activeLocalForwardPorts.remove(Integer.valueOf(assigned)));
    }

    public SshRemotePortForward forwardRemotePort(int remotePort, String localHost, int localPort) throws JSchException {
        connect();
        session.setPortForwardingR(remotePort, localHost, localPort);
        activeRemoteForwardPorts.add(remotePort);
        return new SshRemotePortForward(session, remotePort,
                () -> activeRemoteForwardPorts.remove(Integer.valueOf(remotePort)));
    }

    public SshCommandResult performCommand(String command) throws JSchException, IOException {
        return performCommands(Collections.singletonList(command), null, null);
    }

    public SshCommandResult performCommand(String command, Map<String, String> environment)
            throws JSchException, IOException {
        return performCommands(Collections.singletonList(command), environment, null);
    }

    public SshCommandResult performCommandStreaming(String command, Consumer<String> stdoutLineConsumer)
            throws JSchException, IOException {
        return performCommands(Collections.singletonList(command), null,
                Objects.requireNonNull(stdoutLineConsumer, "stdoutLineConsumer"));
    }

    public SshCommandResult performCommands(List<String> commands) throws JSchException, IOException {
        return performCommands(commands, null, null);
    }

    public SshCommandResult performCommands(List<String> commands, Map<String, String> environment)
            throws JSchException, IOException {
        return performCommands(commands, environment, null);
    }

    public SshCommandResult performCommandsStreaming(List<String> commands, Consumer<String> stdoutLineConsumer)
            throws JSchException, IOException {
        return performCommands(commands, null, Objects.requireNonNull(stdoutLineConsumer, "stdoutLineConsumer"));
    }

    private SshCommandResult performCommands(List<String> commands, Map<String, String> environment,
                                             Consumer<String> stdoutStreamConsumer) throws JSchException, IOException {
        if (sshHostName.isEmpty()) {
            throw new IllegalStateException("RemoteSshClient requires a non-empty SSH host.");
        }
        List<String> internalCommands = new ArrayList<>(commands);
        String longCommand = RemoteCommandBundler.buildLongCommand(internalCommands, isDockerized(),
                dockerName, dockerUsername, SHAFT.Properties.timeouts.dockerCommandTimeout());

        if (sessionPolicy == SshSessionPolicy.NEW_SESSION_PER_COMMAND) {
            disconnectQuietly();
        }
        connect();
        int sessionTimeoutMs = Integer.parseInt(String.valueOf(SHAFT.Properties.timeouts.shellSessionTimeout() * 1000L));
        session.setTimeout(sessionTimeoutMs);

        ReportManager.logDiscrete("Attempting to perform the following command remotely. Command: \"" + longCommand + "\"");
        ChannelExec channel = (ChannelExec) session.openChannel("exec");
        channel.setCommand(longCommand);
        if (environment != null) {
            for (var entry : environment.entrySet()) {
                if (entry.getKey() != null && entry.getValue() != null) {
                    channel.setEnv(entry.getKey(), entry.getValue());
                }
            }
        }
        channel.connect();
        String stdout;
        String stderr;
        try (BufferedReader outReader = new BufferedReader(
                new InputStreamReader(channel.getInputStream(), StandardCharsets.UTF_8));
             BufferedReader errReader = new BufferedReader(
                     new InputStreamReader(channel.getErrStream(), StandardCharsets.UTF_8))) {
            if (stdoutStreamConsumer != null) {
                StringBuilder stdoutAcc = new StringBuilder();
                String line;
                while ((line = outReader.readLine()) != null) {
                    stdoutStreamConsumer.accept(line);
                    appendLine(stdoutAcc, line);
                }
                stdout = stdoutAcc.toString();
                stderr = readAllLines(errReader);
            } else {
                stdout = readAllLines(outReader);
                stderr = readAllLines(errReader);
            }
        }
        int exit = channel.getExitStatus();
        channel.disconnect();
        if (sessionPolicy == SshSessionPolicy.NEW_SESSION_PER_COMMAND) {
            disconnectQuietly();
        }
        SshCommandResult result = new SshCommandResult(stdout, stderr, exit);
        maybeAttachCommandReport(longCommand, result);
        return result;
    }

    @Override
    public void close() {
        disconnectQuietly();
    }

    public boolean isConnected() {
        return session != null && session.isConnected();
    }

    public SshSessionPolicy getSessionPolicy() {
        return sessionPolicy;
    }

    private void maybeAttachCommandReport(String executedCommand, SshCommandResult result) {
        if (!SHAFT.Properties.ssh.attachCommandOutputToReport()) {
            return;
        }
        String merged = result.mergedOutput();
        merged = SshOutputRedactor.apply(merged, SHAFT.Properties.ssh.commandOutputReportRedactRegex());
        String attachment = "Command: " + executedCommand + System.lineSeparator() + merged;
        SHAFT.Report.attach("text/plain", "ssh-exec-output.txt", attachment);
    }

    private boolean isDockerized() {
        return dockerName != null && !dockerName.isEmpty();
    }

    private SshConnectionOptions buildOptions() {
        String identity = null;
        if (sshKeyFileName != null && !sshKeyFileName.isEmpty()) {
            identity = FileActions.getInstance(true).getAbsolutePath(sshKeyFileFolderName, sshKeyFileName);
        }
        String knownHosts = resolveKnownHostsPath();
        return new SshConnectionOptions(
                sshUsername,
                sshHostName,
                sshPortNumber,
                identity,
                SHAFT.Properties.ssh.strictHostKeyChecking(),
                SHAFT.Properties.ssh.connectTimeout(),
                SHAFT.Properties.ssh.serverAliveInterval(),
                knownHosts,
                password,
                keyPassphrase);
    }

    private String resolveKnownHostsPath() {
        if (knownHostsPathOverride != null && !knownHostsPathOverride.isBlank()) {
            return knownHostsPathOverride;
        }
        String fromProps = SHAFT.Properties.ssh.knownHostsFile();
        if (fromProps != null && !fromProps.isBlank()) {
            return fromProps;
        }
        return null;
    }

    private void disconnectQuietly() {
        if (session != null && session.isConnected()) {
            for (Integer p : new ArrayList<>(activeLocalForwardPorts)) {
                try {
                    session.delPortForwardingL(p);
                } catch (JSchException ignored) {
                }
            }
            activeLocalForwardPorts.clear();
            for (Integer r : new ArrayList<>(activeRemoteForwardPorts)) {
                try {
                    session.delPortForwardingR(r);
                } catch (JSchException ignored) {
                }
            }
            activeRemoteForwardPorts.clear();
            session.disconnect();
        }
        session = null;
    }

    private static void appendLine(StringBuilder logBuilder, String line) {
        if (logBuilder.isEmpty()) {
            logBuilder.append(line);
        } else {
            logBuilder.append(System.lineSeparator()).append(line);
        }
    }

    private static String readAllLines(BufferedReader reader) throws IOException {
        StringBuilder logBuilder = new StringBuilder();
        String line;
        while ((line = reader.readLine()) != null) {
            appendLine(logBuilder, line);
        }
        return logBuilder.toString();
    }

    public static final class Builder {
        private final String host;
        private final int port;
        private final String username;
        private String keyFolder = "";
        private String keyFile = "";
        private String dockerName = "";
        private String dockerUser = "";
        private SshSessionPolicy sessionPolicy = SshSessionPolicy.REUSE_SESSION;
        private String password;
        private String knownHostsPath;
        private String keyPassphrase;

        private Builder(String host, int port, String username) {
            this.host = host == null ? "" : host;
            this.port = port;
            this.username = username;
        }

        public Builder identity(String folder, String file) {
            this.keyFolder = folder == null ? "" : folder;
            this.keyFile = file == null ? "" : file;
            return this;
        }

        public Builder dockerExec(String name, String user) {
            this.dockerName = name == null ? "" : name;
            this.dockerUser = user == null ? "" : user;
            return this;
        }

        public Builder sessionPolicy(SshSessionPolicy policy) {
            this.sessionPolicy = policy == null ? SshSessionPolicy.REUSE_SESSION : policy;
            return this;
        }

        public Builder password(String value) {
            this.password = value;
            return this;
        }

        public Builder knownHostsPath(String path) {
            this.knownHostsPath = path;
            return this;
        }

        public Builder keyPassphrase(String passphrase) {
            this.keyPassphrase = passphrase;
            return this;
        }

        public RemoteSshClient build() {
            return new RemoteSshClient(host, port, username, keyFolder, keyFile, dockerName, dockerUser, sessionPolicy,
                    password, knownHostsPath, keyPassphrase);
        }
    }
}
