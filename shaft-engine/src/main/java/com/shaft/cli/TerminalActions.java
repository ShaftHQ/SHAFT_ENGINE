package com.shaft.cli;

import com.jcraft.jsch.*;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import lombok.Getter;
import org.apache.commons.lang3.SystemUtils;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.*;

/**
 * Executes shell commands on local or remote terminals.
 *
 * <p>Supports local OS command execution via {@link ProcessBuilder} and
 * remote command execution over SSH using JSch. Commands can be run
 * synchronously or asynchronously with configurable timeouts.
 *
 * <p><b>Usage example:</b>
 * <pre>{@code
 * SHAFT.CLI.terminal().performTerminalCommand("ls -la");
 * }</pre>
 *
 * @see com.shaft.driver.SHAFT.CLI
 */
@SuppressWarnings("unused")
public class TerminalActions {
    @Getter
    private String sshHostName = "";
    @Getter
    private String sshUsername;
    @Getter
    private String sshKeyFileFolderName;
    @Getter
    private String sshKeyFileName;
    @Getter
    private int sshPortNumber = 22;

    @Getter
    private String dockerName = "";
    @Getter
    private String dockerUsername;

    private boolean asynchronous = false;
    private boolean verbose = false;
    private boolean isInternal = false;
    private boolean reuseRemoteSession = false;
    private Session reusableRemoteSession;
    private ScheduledExecutorService reusableSessionTimeoutScheduler;
    private ScheduledFuture<?> reusableSessionTimeoutTask;
    private final List<Integer> activeLocalPortForwards = new ArrayList<>();
    private final List<Integer> activeRemotePortForwards = new ArrayList<>();


    /**
     * This constructor is used for local terminal actions.
     */
    public TerminalActions() {
    }

    /**
     * This constructor is used for local terminal actions.
     *
     * @param asynchronous true for asynchronous execution of commands in a separate thread
     */
    public TerminalActions(boolean asynchronous) {
        this.asynchronous = asynchronous;
    }

    private TerminalActions(boolean asynchronous, boolean verbose, boolean isInternal) {
        this.asynchronous = asynchronous;
        this.verbose = verbose;
        this.isInternal = isInternal;
    }

    /**
     * This constructor is used for local terminal actions inside a docker.
     *
     * @param dockerName     the name of the docker instance that you want to
     *                       execute the terminal command inside
     * @param dockerUsername the username which will be used to access the docker
     *                       instance. Must have the access/privilege to execute the
     *                       terminal command
     */
    public TerminalActions(String dockerName, String dockerUsername) {
        this.dockerName = dockerName;
        this.dockerUsername = dockerUsername;
    }

    /**
     * This constructor is used for remote terminal actions.
     *
     * @param sshHostName          the IP address or host name for the remote
     *                             machine you want to execute the terminal command
     *                             on.
     * @param sshPortNumber        the port that's used for the SSH service on the
     *                             target machine. Default is 22.
     * @param sshUsername          the username which will be used to access the
     *                             target machine via ssh. Must have the
     *                             access/privilege to execute the terminal command
     * @param sshKeyFileFolderName the directory that holds the ssh key file
     *                             (usually it's somewhere in the test data of the
     *                             current project)
     * @param sshKeyFileName       the name of the ssh key file
     */
    public TerminalActions(String sshHostName, int sshPortNumber, String sshUsername, String sshKeyFileFolderName,
                           String sshKeyFileName) {
        this.sshHostName = sshHostName;
        this.sshPortNumber = sshPortNumber;
        this.sshUsername = sshUsername;
        this.sshKeyFileFolderName = sshKeyFileFolderName;
        this.sshKeyFileName = sshKeyFileName;
    }

    /**
     * This constructor is used for remote terminal actions inside a docker.
     *
     * @param sshHostName          the IP address or host name for the remote
     *                             machine you want to execute the terminal command
     *                             on.
     * @param sshPortNumber        the port that's used for the SSH service on the
     *                             target machine. Default is 22.
     * @param sshUsername          the username which will be used to access the
     *                             target machine via ssh. Must have the
     *                             access/privilege to execute the terminal command
     * @param sshKeyFileFolderName the directory that holds the ssh key file
     *                             (usually it's somewhere in the test data of the
     *                             current project)
     * @param sshKeyFileName       the name of the ssh key file
     * @param dockerName           the name of the docker instance that you want to
     *                             execute the terminal command inside
     * @param dockerUsername       the username which will be used to access the
     *                             docker instance. Must have the access/privilege
     *                             to execute the terminal command
     */
    public TerminalActions(String sshHostName, int sshPortNumber, String sshUsername, String sshKeyFileFolderName,
                           String sshKeyFileName, String dockerName, String dockerUsername) {
        this.sshHostName = sshHostName;
        this.sshPortNumber = sshPortNumber;
        this.sshUsername = sshUsername;
        this.sshKeyFileFolderName = sshKeyFileFolderName;
        this.sshKeyFileName = sshKeyFileName;
        this.dockerName = dockerName;
        this.dockerUsername = dockerUsername;
    }

    public static TerminalActions getInstance() {
        return new TerminalActions();
    }

    public static TerminalActions getInstance(boolean asynchronous) {
        return new TerminalActions(asynchronous);
    }

    public static TerminalActions getInstance(boolean asynchronous, boolean verbose) {
        return new TerminalActions(asynchronous, verbose, false);
    }

    public static TerminalActions getInstance(boolean asynchronous, boolean verbose, boolean isInternal) {
        return new TerminalActions(asynchronous, verbose, isInternal);
    }

    /**
     * Creates a remote terminal actions instance that reuses the same SSH session
     * for multiple command executions until {@link #quit()} is called.
     *
     * @param sshHostName          the IP address or host name for the remote machine
     * @param sshPortNumber        the SSH service port on the target machine
     * @param sshUsername          the username used to access the target machine
     * @param sshKeyFileFolderName the directory that holds the SSH key file
     * @param sshKeyFileName       the SSH key file name
     * @return a reusable remote {@link TerminalActions} instance
     */
    public static TerminalActions getRemoteInstance(String sshHostName, int sshPortNumber, String sshUsername,
                                                    String sshKeyFileFolderName, String sshKeyFileName) {
        TerminalActions terminalActions = new TerminalActions(sshHostName, sshPortNumber, sshUsername,
                sshKeyFileFolderName, sshKeyFileName);
        terminalActions.reuseRemoteSession = true;
        return terminalActions;
    }

    private static String reportActionResult(String actionName, String testData, String log, Boolean passFailStatus, Exception... rootCauseException) {
        actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "Terminal Action \"" + actionName + "\" successfully performed.";
        } else {
            message = "Terminal Action \"" + actionName + "\" failed.";
        }

        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Terminal Action Test Data - " + actionName,
                    "Actual Value", testData);
            attachments.add(actualValueAttachment);
        } else if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data \"" + testData + "\".";
        }

        if (log != null && !log.trim().isEmpty()) {
            attachments.add(Arrays.asList("Terminal Action Actual Result", "Command Log", log));
        }

        if (rootCauseException != null && rootCauseException.length >= 1) {
            List<Object> actualValueAttachment = Arrays.asList("Terminal Action Exception - " + actionName,
                    "Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(rootCauseException[0]));
            attachments.add(actualValueAttachment);
        }

        if (!attachments.isEmpty()) {
            ReportManagerHelper.log(message, attachments);
        } else {
            ReportManager.log(message);
        }

        return message;
    }

    public boolean isRemoteTerminal() {
        return !sshHostName.isEmpty();
    }

    public boolean isDockerizedTerminal() {
        return !dockerName.isEmpty();
    }

    public String performTerminalCommands(List<String> commands) {
        return performTerminalCommands(commands, Collections.emptyMap());
    }

    /**
     * Executes one or more terminal commands with the supplied environment variables.
     *
     * <p>For local terminals the variables are added to the spawned process environment.
     * For dockerized remote terminals they are injected into the container through
     * {@code docker exec -e}. For non-dockerized remote terminals they are sent as SSH
     * {@code env} requests, which only take effect when the SSH server allows them (for
     * example via {@code AcceptEnv} in {@code sshd_config}); otherwise they are silently
     * ignored by the server.</p>
     *
     * @param commands             the commands to execute
     * @param environmentVariables the environment variables to expose to the command
     * @return the command output log
     */
    public String performTerminalCommands(List<String> commands, Map<String, String> environmentVariables) {
        var internalCommands = commands;

        // Build long command and refactor for dockerized execution if needed
        String longCommand = buildLongCommand(internalCommands, environmentVariables);

        if (internalCommands.size() == 1) {
            if (internalCommands.getFirst().contains(" && ")) {
                internalCommands = List.of(internalCommands.getFirst().split(" && "));
            } else if (internalCommands.getFirst().contains(" ; ")) {
                internalCommands = List.of(internalCommands.getFirst().split(" ; "));
            }
        }

        // Perform command
        List<String> exitLogs = isRemoteTerminal() ? executeRemoteCommand(internalCommands, longCommand, environmentVariables) : executeLocalCommand(internalCommands, longCommand, environmentVariables);
        String log = exitLogs.get(0);
        String exitStatus = exitLogs.get(1);

        // Prepare final log message
        StringBuilder reportMessage = new StringBuilder();
        if (!sshHostName.isEmpty()) {
            reportMessage.append("Host Name: \"").append(sshHostName).append("\"");
            reportMessage.append(" | SSH Port Number: \"").append(sshPortNumber).append("\"");
            reportMessage.append(" | SSH Username: \"").append(sshUsername).append("\"");
        } else {
            reportMessage.append("Host Name: \"" + "localHost" + "\"");
        }
        if (sshKeyFileName != null && !sshKeyFileName.isEmpty()) {
            reportMessage.append(" | Key File: \"").append(sshKeyFileFolderName).append(sshKeyFileName).append("\"");
        }
        reportMessage.append(" | Command: \"").append(longCommand).append("\"");
        reportMessage.append(" | Exit Status: \"").append(exitStatus).append("\"");

        if (log != null) {
            if (!isInternal)
                passAction(reportMessage.toString(), log);
            return log;
        } else {
            return "";
        }
    }

    public String performTerminalCommand(String command) {
        return performTerminalCommands(Collections.singletonList(command));
    }

    /**
     * Executes a terminal command with the supplied environment variables.
     *
     * @param command              the command to execute
     * @param environmentVariables the environment variables to expose to the command
     * @return the command output log
     * @see #performTerminalCommands(List, Map)
     */
    public String performTerminalCommand(String command, Map<String, String> environmentVariables) {
        return performTerminalCommands(Collections.singletonList(command), environmentVariables);
    }

    /**
     * Executes a terminal command and returns this terminal actions instance for fluent chaining.
     *
     * @param command the command to execute
     * @return this {@link TerminalActions} instance
     */
    public TerminalActions executeTerminalCommand(String command) {
        performTerminalCommand(command);
        return this;
    }

    /**
     * Uploads a local file to the connected remote host over SFTP.
     *
     * <p>Requires a remote terminal created through a remote SSH constructor or
     * {@link com.shaft.driver.SHAFT.CLI#remoteTerminal(String, int, String, String, String)}.
     * Docker-wrapped remote terminals are not supported; use {@link #performTerminalCommand(String)}
     * for file operations inside a container.</p>
     *
     * @param localFilePath  relative or absolute path to the local source file
     * @param remoteFilePath destination path on the remote host
     * @return the remote destination path for assertions
     */
    public String uploadFile(String localFilePath, String remoteFilePath) {
        return performSftpTransfer(SftpTransferDirection.UPLOAD, localFilePath, remoteFilePath);
    }

    /**
     * Downloads a file from the connected remote host over SFTP.
     *
     * @param remoteFilePath source path on the remote host
     * @param localFilePath  relative or absolute destination path on the local machine
     * @return the local absolute destination path for assertions
     */
    public String downloadFile(String remoteFilePath, String localFilePath) {
        return performSftpTransfer(SftpTransferDirection.DOWNLOAD, remoteFilePath, localFilePath);
    }

    /**
     * Forwards a local port through the reusable SSH session to a remote host and port.
     *
     * <p>Use {@code 0} for {@code localPort} to let JSch assign an available local port.
     * Active forwards are removed when {@link #quit()} is called.</p>
     *
     * @param localPort  local bind port, or {@code 0} for an ephemeral local port
     * @param remoteHost remote target host as seen from the SSH server
     * @param remotePort remote target port
     * @return the bound local port as a string
     */
    public String forwardLocalPort(int localPort, String remoteHost, int remotePort) {
        verifyReusableRemoteSessionFeature();
        String testData = "Host Name: \"" + sshHostName + "\" | Local Port: \"" + localPort
                + "\" | Remote Host: \"" + remoteHost + "\" | Remote Port: \"" + remotePort + "\"";
        Session remoteSession = getRemoteSession();
        try {
            int assignedPort = remoteSession.setPortForwardingL(localPort, remoteHost, remotePort);
            activeLocalPortForwards.add(assignedPort);
            passAction(testData, "Forwarded local port \"" + assignedPort + "\" to \"" + remoteHost + ":" + remotePort + "\"");
            return String.valueOf(assignedPort);
        } catch (JSchException exception) {
            failAction(testData, exception);
            return "";
        }
    }

    /**
     * Forwards a remote port through the reusable SSH session to a local host and port.
     *
     * <p>Remote port forwarding may require server-side SSH configuration. Active forwards are
     * removed when {@link #quit()} is called.</p>
     *
     * @param remotePort remote bind port on the SSH server
     * @param localHost  local target host as seen from the SSH client machine
     * @param localPort  local target port
     * @return the remote bind port as a string
     */
    public String forwardRemotePort(int remotePort, String localHost, int localPort) {
        verifyReusableRemoteSessionFeature();
        String testData = "Host Name: \"" + sshHostName + "\" | Remote Port: \"" + remotePort
                + "\" | Local Host: \"" + localHost + "\" | Local Port: \"" + localPort + "\"";
        Session remoteSession = getRemoteSession();
        try {
            remoteSession.setPortForwardingR(remotePort, localHost, localPort);
            activeRemotePortForwards.add(remotePort);
            passAction(testData, "Forwarded remote port \"" + remotePort + "\" to \"" + localHost + ":" + localPort + "\"");
            return String.valueOf(remotePort);
        } catch (JSchException exception) {
            failAction(testData, exception);
            return "";
        }
    }

    /**
     * Returns the underlying JSch session for advanced remote terminal usage.
     *
     * <p>Only available for reusable remote terminals created through
     * {@link com.shaft.driver.SHAFT.CLI#remoteTerminal(String, int, String, String, String)}.</p>
     *
     * @return the connected reusable JSch session
     */
    public Session getJschSession() {
        verifyReusableRemoteSessionFeature();
        return getRemoteSession();
    }

    /**
     * Disconnects any reusable SSH session owned by this terminal actions instance.
     *
     * <p>This method is safe to call before the first remote command is executed and is a no-op
     * for local terminals or ephemeral remote terminals.</p>
     */
    public synchronized void quit() {
        cancelReusableSessionTimeoutTask();
        if (reusableRemoteSession != null && reusableRemoteSession.isConnected()) {
            clearPortForwards(reusableRemoteSession);
            reusableRemoteSession.disconnect();
        }
        reusableRemoteSession = null;
        activeLocalPortForwards.clear();
        activeRemotePortForwards.clear();
        if (reusableSessionTimeoutScheduler != null) {
            reusableSessionTimeoutScheduler.shutdown();
            reusableSessionTimeoutScheduler = null;
        }
    }

    private void passAction(String actionName, String testData, String log) {
        reportActionResult(actionName, testData, log, true);
    }

    private void passAction(String testData, String log) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(actionName, testData, log);
    }

    private void failAction(String actionName, String testData, Exception... rootCauseException) {
        String message = reportActionResult(actionName, testData, null, false, rootCauseException);
        FailureReporter.fail(TerminalActions.class, message, rootCauseException[0]);
    }

    private void failAction(String testData, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(actionName, testData, rootCauseException);
    }

    private Session createSSHsession() {
        Session session = null;
        String testData = sshHostName + ", " + sshPortNumber + ", " + sshUsername + ", " + sshKeyFileFolderName + ", "
                + sshKeyFileName;
        try {
            Properties config = new Properties();
            config.put("StrictHostKeyChecking", "no");
            JSch jsch = new JSch();
            if (sshKeyFileName != null && !sshKeyFileName.isEmpty()) {
                jsch.addIdentity(FileActions.getInstance(true).getAbsolutePath(sshKeyFileFolderName, sshKeyFileName));
            }
            session = jsch.getSession(sshUsername, sshHostName, sshPortNumber);
            session.setConfig(config);
            session.connect();
            ReportManager.logDiscrete("Successfully created SSH Session.");
        } catch (JSchException rootCauseException) {
            failAction(testData, rootCauseException);
        }
        return session;
    }

    private synchronized Session getRemoteSession() {
        if (!reuseRemoteSession) {
            return createSSHsession();
        }
        if (reusableRemoteSession == null || !reusableRemoteSession.isConnected()) {
            reusableRemoteSession = createSSHsession();
        }
        scheduleReusableSessionTimeout();
        return reusableRemoteSession;
    }

    private synchronized void scheduleReusableSessionTimeout() {
        long sessionTimeoutMinutes = SHAFT.Properties.timeouts.shellSessionTimeout();
        if (sessionTimeoutMinutes <= 0) {
            return;
        }

        if (reusableSessionTimeoutScheduler == null || reusableSessionTimeoutScheduler.isShutdown()) {
            reusableSessionTimeoutScheduler = Executors.newSingleThreadScheduledExecutor(getReusableSessionTimeoutThreadFactory());
        }
        cancelReusableSessionTimeoutTask();
        reusableSessionTimeoutTask = reusableSessionTimeoutScheduler.schedule(this::quit, sessionTimeoutMinutes, TimeUnit.MINUTES);
    }

    private ThreadFactory getReusableSessionTimeoutThreadFactory() {
        return runnable -> {
            Thread thread = new Thread(runnable, "SHAFT-CLI-Reusable-Session-Timeout");
            thread.setDaemon(true);
            return thread;
        };
    }

    private synchronized void cancelReusableSessionTimeoutTask() {
        if (reusableSessionTimeoutTask != null) {
            reusableSessionTimeoutTask.cancel(false);
            reusableSessionTimeoutTask = null;
        }
    }

    private void disconnectRemoteSessionIfEphemeral(Session session) {
        if (!reuseRemoteSession && session != null && session.isConnected()) {
            session.disconnect();
        }
    }

    private enum SftpTransferDirection {
        UPLOAD,
        DOWNLOAD
    }

    private String performSftpTransfer(SftpTransferDirection direction, String sourcePath, String destinationPath) {
        verifyRemoteSftpTerminal();
        String localPath;
        String remotePath;
        if (direction == SftpTransferDirection.UPLOAD) {
            // upload sources may live under the test data folder, so reuse SHAFT path resolution
            localPath = FileActions.getInstance(true).getAbsolutePath(sourcePath);
            remotePath = destinationPath;
        } else {
            remotePath = sourcePath;
            // download destinations are saved exactly where the caller asks, relative to the project directory
            localPath = new File(destinationPath).getAbsolutePath();
        }

        String testData = "Host Name: \"" + sshHostName + "\" | SSH Port Number: \"" + sshPortNumber
                + "\" | SSH Username: \"" + sshUsername + "\" | Local Path: \"" + localPath
                + "\" | Remote Path: \"" + remotePath + "\"";

        if (direction == SftpTransferDirection.UPLOAD && !Files.isRegularFile(Path.of(localPath))) {
            failAction(testData, new IOException("Local file does not exist: \"" + localPath + "\""));
        }

        if (direction == SftpTransferDirection.DOWNLOAD) {
            Path localDestination = Path.of(localPath);
            Path parentDirectory = localDestination.getParent();
            if (parentDirectory != null) {
                FileActions.getInstance(true).createFolder(parentDirectory.toString());
            }
        }

        Session remoteSession = getRemoteSession();
        ChannelSftp sftpChannel = null;
        try {
            sftpChannel = openSftpChannel(remoteSession);
            if (direction == SftpTransferDirection.UPLOAD) {
                sftpChannel.put(localPath, remotePath);
                passAction(testData, "Uploaded file to \"" + remotePath + "\"");
                return remotePath;
            }
            sftpChannel.get(remotePath, localPath);
            passAction(testData, "Downloaded file to \"" + localPath + "\"");
            return localPath;
        } catch (JSchException | SftpException exception) {
            failAction(testData, exception);
            return "";
        } finally {
            disconnectSftpChannel(sftpChannel);
            disconnectRemoteSessionIfEphemeral(remoteSession);
        }
    }

    private void verifyRemoteSftpTerminal() {
        if (!isRemoteTerminal()) {
            failAction("SFTP file transfer",
                    new IllegalStateException("SFTP is only supported for remote SSH terminals."));
        }
        if (isDockerizedTerminal()) {
            failAction("SFTP file transfer", new IllegalStateException(
                    "SFTP operates on the remote host filesystem. Use performTerminalCommand(...) for dockerized remote terminals."));
        }
    }

    private ChannelSftp openSftpChannel(Session session) throws JSchException {
        int sessionTimeout = Math.toIntExact(TimeUnit.MINUTES.toMillis(SHAFT.Properties.timeouts.shellSessionTimeout()));
        session.setTimeout(sessionTimeout);
        ChannelSftp sftpChannel = (ChannelSftp) session.openChannel("sftp");
        sftpChannel.connect();
        return sftpChannel;
    }

    private void disconnectSftpChannel(ChannelSftp sftpChannel) {
        if (sftpChannel != null && sftpChannel.isConnected()) {
            sftpChannel.disconnect();
        }
    }

    private void verifyReusableRemoteSessionFeature() {
        if (!isRemoteTerminal()) {
            failAction("Reusable remote SSH feature",
                    new IllegalStateException("This feature is only supported for remote SSH terminals."));
        }
        if (isDockerizedTerminal()) {
            failAction("Reusable remote SSH feature", new IllegalStateException(
                    "This feature operates on the remote host SSH session. Use performTerminalCommand(...) for dockerized remote terminals."));
        }
        if (!reuseRemoteSession) {
            failAction("Reusable remote SSH feature", new IllegalStateException(
                    "This feature requires a reusable remote terminal. Use SHAFT.CLI.remoteTerminal(...)."));
        }
    }

    private void clearPortForwards(Session session) {
        for (Integer localPort : new ArrayList<>(activeLocalPortForwards)) {
            try {
                session.delPortForwardingL(localPort);
            } catch (Exception exception) {
                ReportManager.logDiscrete("Failed to remove local port forward on port " + localPort + ": " + exception.getMessage());
            }
        }
        for (Integer remotePort : new ArrayList<>(activeRemotePortForwards)) {
            try {
                session.delPortForwardingR(remotePort);
            } catch (Exception exception) {
                ReportManager.logDiscrete("Failed to remove remote port forward on port " + remotePort + ": " + exception.getMessage());
            }
        }
    }

    private String buildLongCommand(List<String> commands) {
        return buildLongCommand(commands, Collections.emptyMap());
    }

    private String buildLongCommand(List<String> commands, Map<String, String> environmentVariables) {
        StringBuilder command = new StringBuilder();
        // build long command
        for (Iterator<String> i = commands.iterator(); i.hasNext(); ) {
            if (command.isEmpty()) {
                command.append(i.next());
            } else {
                command.append(" && ").append(i.next());
            }
        }

        // refactor long command for dockerized execution
        if (isDockerizedTerminal()) {
            command.insert(0, "docker exec " + buildDockerEnvironmentFlags(environmentVariables) + "-u " + dockerUsername
                    + " -i " + dockerName + " timeout "
                    + SHAFT.Properties.timeouts.dockerCommandTimeout() + " sh -c '");
            command.append("'");
        }
        return command.toString();
    }

    private String buildDockerEnvironmentFlags(Map<String, String> environmentVariables) {
        if (environmentVariables == null || environmentVariables.isEmpty()) {
            return "";
        }
        StringBuilder flags = new StringBuilder();
        environmentVariables.forEach((key, value) -> {
            String safeValue = value == null ? "" : value.replace("\\", "\\\\").replace("\"", "\\\"");
            flags.append("-e \"").append(key).append("=").append(safeValue).append("\" ");
        });
        return flags.toString();
    }

    private List<String> executeLocalCommand(List<String> commands, String longCommand, Map<String, String> environmentVariables) {
        StringBuilder logs = new StringBuilder();
        StringBuilder exitStatuses = new StringBuilder();
        // local execution
//        ReportManager.logDiscrete("Attempting to execute the following command locally. Command: \"" + longCommand + "\"");
        boolean isWindows = SystemUtils.IS_OS_WINDOWS;
        String directory;
        LinkedList<String> internalCommands;
        if (commands.size() > 1 && commands.getFirst().startsWith("cd ")) {
            directory = commands.getFirst().replace("cd ", "");
            internalCommands = new LinkedList<>(commands);
            internalCommands.removeFirst();
        } else {
            directory = System.getProperty("user.dir");
            internalCommands = new LinkedList<>(commands);
        }
        FileActions.getInstance(true).createFolder(directory.replace("\"", ""));
        String finalDirectory = directory;
        internalCommands.forEach(command -> {
            command = command.contains(".bat") && !command.contains(".\\") && !command.matches("(^.:\\\\.*$)") ? ".\\" + command : command;
            ReportManager.logDiscrete("Executing: \"" + command + "\" locally.");
            try {
                if (Thread.currentThread().isInterrupted()) {
                    throw new InterruptedException("Current thread was interrupted before local command execution.");
                }
                ProcessBuilder pb = getProcessBuilder(command, finalDirectory, isWindows);
                pb.environment().put("JAVA_HOME", System.getProperty("java.home"));
                if (environmentVariables != null) {
                    environmentVariables.forEach(pb.environment()::put);
                }
                if (!asynchronous) {
                    pb.redirectErrorStream(true);
                    Process localProcess = pb.start();
                    // output logs
                    String line;
                    try (InputStreamReader isr = new InputStreamReader(localProcess.getInputStream());
                         BufferedReader rdr = new BufferedReader(isr)) {
                        while ((line = rdr.readLine()) != null) {
                            if (Boolean.TRUE.equals(verbose)) {
                                ReportManager.logDiscrete(line);
                            }
                            logs.append(line);
                            logs.append("\n");
                        }
                    }
                    try (InputStreamReader isr = new InputStreamReader(localProcess.getErrorStream());
                         BufferedReader rdr = new BufferedReader(isr)) {
                        while ((line = rdr.readLine()) != null) {
                            if (Boolean.TRUE.equals(verbose)) {
                                ReportManager.logDiscrete(line);
                            }
                            logs.append("\n");
                            logs.append(line);
                        }
                    }
                    // Wait for the process to complete
                    localProcess.waitFor(SHAFT.Properties.timeouts.shellSessionTimeout(), TimeUnit.MINUTES);
                    // Retrieve the exit status of the executed command and destroy open sessions
                    exitStatuses.append(localProcess.exitValue());
                } else {
                    exitStatuses.append("asynchronous");
                    ScheduledExecutorService asynchronousProcessExecution = Executors.newScheduledThreadPool(1);
                    asynchronousProcessExecution.schedule(() -> {
                        try {
                            pb.start();
                            asynchronousProcessExecution.shutdown();
                        } catch (Throwable throwable) {
                            asynchronousProcessExecution.shutdownNow();
                        }
                    }, 0, TimeUnit.SECONDS);
                    if (!asynchronousProcessExecution.awaitTermination(SHAFT.Properties.timeouts.shellSessionTimeout(), TimeUnit.MINUTES)) {
                        asynchronousProcessExecution.shutdownNow();
                    }
                }
            } catch (IOException exception) {
                failAction(longCommand, exception);
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
                failAction(longCommand, exception);
            }
        });
        return Arrays.asList(logs.toString().trim(), exitStatuses.toString());
    }

    private ProcessBuilder getProcessBuilder(String command, String finalDirectory, boolean isWindows) {
        ProcessBuilder pb = new ProcessBuilder();
        pb.directory(new File(finalDirectory));

        // https://stackoverflow.com/a/10954450/12912100
        if (isWindows) {
            if (asynchronous && verbose) {
                // Apply the execution policy to the spawned child PowerShell as well,
                // because Start-Process launches a separate process that does not inherit
                // the outer shell's command-line arguments.
                pb.command("powershell.exe", "-ExecutionPolicy", "Bypass", "Start-Process powershell.exe '-ExecutionPolicy Bypass -NoExit -WindowStyle Minimized -Command \"[Console]::Title = ''shaft-engine''; " + command + "\"'");
            } else {
                pb.command("powershell.exe", "-ExecutionPolicy", "Bypass", "-Command", command);
            }
        } else {
            pb.command("sh", "-c", command);
        }
        return pb;
    }

    private List<String> executeRemoteCommand(List<String> commands, String longCommand, Map<String, String> environmentVariables) {
        StringBuilder logs = new StringBuilder();
        StringBuilder exitStatuses = new StringBuilder();
        int sessionTimeout = Math.toIntExact(TimeUnit.MINUTES.toMillis(SHAFT.Properties.timeouts.shellSessionTimeout()));
        // remote execution
        ReportManager.logDiscrete(
                "Attempting to perform the following command remotely. Command: \"" + longCommand + "\"");
        Session remoteSession = getRemoteSession();
        ChannelExec remoteChannelExecutor = null;
        if (remoteSession != null) {
            try {
                remoteSession.setTimeout(sessionTimeout);
                remoteChannelExecutor = (ChannelExec) remoteSession.openChannel("exec");
                remoteChannelExecutor.setCommand(longCommand);
                if (environmentVariables != null) {
                    environmentVariables.forEach(remoteChannelExecutor::setEnv);
                }
                remoteChannelExecutor.connect();

                // Capture logs and close readers
                try (BufferedReader reader = new BufferedReader(new InputStreamReader(remoteChannelExecutor.getInputStream()));
                     BufferedReader errorReader = new BufferedReader(new InputStreamReader(remoteChannelExecutor.getErrStream()))) {
                    logs.append(readConsoleLogs(reader));
                    logs.append(readConsoleLogs(errorReader));
                }

                // Retrieve the exit status of the executed command and destroy open sessions
                exitStatuses.append(remoteChannelExecutor.getExitStatus());
            } catch (JSchException | IOException exception) {
                failAction(longCommand, exception);
            } finally {
                if (remoteChannelExecutor != null && remoteChannelExecutor.isConnected()) {
                    remoteChannelExecutor.disconnect();
                }
                disconnectRemoteSessionIfEphemeral(remoteSession);
            }
        }
        return Arrays.asList(logs.toString(), exitStatuses.toString());
    }

    private String readConsoleLogs(BufferedReader reader) throws IOException {
        StringBuilder logBuilder = new StringBuilder();
        if (reader != null) {
            String logLine;
            while ((logLine = reader.readLine()) != null) {
                if (logBuilder.isEmpty()) {
                    logBuilder.append(logLine);
                } else {
                    logBuilder.append(System.lineSeparator()).append(logLine);
                }
            }
        }
        return logBuilder.toString();
    }
}
