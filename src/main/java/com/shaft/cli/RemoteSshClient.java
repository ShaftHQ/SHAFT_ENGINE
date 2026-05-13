package com.shaft.cli;

import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.shaft.cli.internal.JschSessionFactory;
import com.shaft.cli.internal.RemoteCommandBundler;
import com.shaft.cli.internal.SshConnectionOptions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Reusable SSH client: one {@link Session} (by default) and multiple {@code exec} channels.
 * Use {@link SshSessionPolicy#NEW_SESSION_PER_COMMAND} to mimic legacy per-call {@link TerminalActions} isolation.
 *
 * <p><b>Example:</b>
 * <pre>{@code
 * try (var ssh = SHAFT.CLI.remoteSsh(host, 22, user, keyFolder, keyFile)) {
 *     ssh.connect();
 *     var r1 = ssh.performCommand("pwd");
 *     var r2 = ssh.performCommand("whoami");
 * }
 * }</pre>
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

    private Session session;

    public RemoteSshClient(String sshHostName, int sshPortNumber, String sshUsername,
                           String sshKeyFileFolderName, String sshKeyFileName) {
        this(sshHostName, sshPortNumber, sshUsername, sshKeyFileFolderName, sshKeyFileName, "", "", SshSessionPolicy.REUSE_SESSION);
    }

    public RemoteSshClient(String sshHostName, int sshPortNumber, String sshUsername,
                           String sshKeyFileFolderName, String sshKeyFileName, SshSessionPolicy sessionPolicy) {
        this(sshHostName, sshPortNumber, sshUsername, sshKeyFileFolderName, sshKeyFileName, "", "", sessionPolicy);
    }

    public RemoteSshClient(String sshHostName, int sshPortNumber, String sshUsername,
                           String sshKeyFileFolderName, String sshKeyFileName,
                           String dockerName, String dockerUsername) {
        this(sshHostName, sshPortNumber, sshUsername, sshKeyFileFolderName, sshKeyFileName, dockerName, dockerUsername, SshSessionPolicy.REUSE_SESSION);
    }

    public RemoteSshClient(String sshHostName, int sshPortNumber, String sshUsername,
                           String sshKeyFileFolderName, String sshKeyFileName,
                           String dockerName, String dockerUsername, SshSessionPolicy sessionPolicy) {
        this.sshHostName = sshHostName == null ? "" : sshHostName;
        this.sshPortNumber = sshPortNumber;
        this.sshUsername = sshUsername;
        this.sshKeyFileFolderName = sshKeyFileFolderName;
        this.sshKeyFileName = sshKeyFileName;
        this.dockerName = dockerName == null ? "" : dockerName;
        this.dockerUsername = dockerUsername == null ? "" : dockerUsername;
        this.sessionPolicy = sessionPolicy == null ? SshSessionPolicy.REUSE_SESSION : sessionPolicy;
    }

    public void connect() throws JSchException {
        if (session != null && session.isConnected()) {
            return;
        }
        disconnectQuietly();
        SshConnectionOptions options = buildOptions();
        session = JschSessionFactory.connect(options);
        ReportManager.logDiscrete("Successfully created SSH Session.");
    }

    public SshCommandResult performCommand(String command) throws JSchException, IOException {
        return performCommands(Collections.singletonList(command));
    }

    public SshCommandResult performCommands(List<String> commands) throws JSchException, IOException {
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
        int sessionTimeoutMs = Integer.parseInt(String.valueOf(SHAFT.Properties.timeouts.shellSessionTimeout() * 1000));
        session.setTimeout(sessionTimeoutMs);

        ReportManager.logDiscrete("Attempting to perform the following command remotely. Command: \"" + longCommand + "\"");
        ChannelExec channel = (ChannelExec) session.openChannel("exec");
        channel.setCommand(longCommand);
        channel.connect();
        String stdout;
        String stderr;
        try (BufferedReader outReader = new BufferedReader(new InputStreamReader(channel.getInputStream(), StandardCharsets.UTF_8));
             BufferedReader errReader = new BufferedReader(new InputStreamReader(channel.getErrStream(), StandardCharsets.UTF_8))) {
            stdout = readAllLines(outReader);
            stderr = readAllLines(errReader);
        }
        int exit = channel.getExitStatus();
        channel.disconnect();
        if (sessionPolicy == SshSessionPolicy.NEW_SESSION_PER_COMMAND) {
            disconnectQuietly();
        }
        return new SshCommandResult(stdout, stderr, exit);
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

    private boolean isDockerized() {
        return dockerName != null && !dockerName.isEmpty();
    }

    private SshConnectionOptions buildOptions() {
        String identity = null;
        if (sshKeyFileName != null && !sshKeyFileName.isEmpty()) {
            identity = FileActions.getInstance(true).getAbsolutePath(sshKeyFileFolderName, sshKeyFileName);
        }
        return new SshConnectionOptions(
                sshUsername,
                sshHostName,
                sshPortNumber,
                identity,
                SHAFT.Properties.ssh.strictHostKeyChecking(),
                SHAFT.Properties.ssh.connectTimeout(),
                SHAFT.Properties.ssh.serverAliveInterval());
    }

    private void disconnectQuietly() {
        if (session != null) {
            session.disconnect();
            session = null;
        }
    }

    private static String readAllLines(BufferedReader reader) throws IOException {
        StringBuilder logBuilder = new StringBuilder();
        String line;
        while ((line = reader.readLine()) != null) {
            if (logBuilder.isEmpty()) {
                logBuilder.append(line);
            } else {
                logBuilder.append(System.lineSeparator()).append(line);
            }
        }
        return logBuilder.toString();
    }
}
