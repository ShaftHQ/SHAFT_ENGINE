package com.shaft.cli;

import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.shaft.cli.internal.RemoteCommandBundler;
import com.shaft.cli.internal.ShellCommandNormalizer;
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
import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Executes shell commands on local or remote terminals.
 *
 * <p>Remote SSH defaults to one new session per {@link #performTerminalCommand(String)} (backward compatible).
 * Pass {@code reusableRemoteSshSession == true} to keep one session until {@link #quit()} (same idea as
 * {@link com.shaft.driver.SHAFT.CLI.Terminal} with a remote constructor).
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

    /**
     * Last remote command exit status from JSch, or {@code -1} if not applicable / not yet run.
     */
    @Getter
    private int lastRemoteCommandExitStatus = -1;

    private boolean asynchronous = false;
    private boolean verbose = false;
    private boolean isInternal = false;

    private final boolean reusableRemoteSshSession;
    private RemoteSshClient reusableSshClient;

    public TerminalActions() {
        this.reusableRemoteSshSession = false;
    }

    public TerminalActions(boolean asynchronous) {
        this.asynchronous = asynchronous;
        this.reusableRemoteSshSession = false;
    }

    private TerminalActions(boolean asynchronous, boolean verbose, boolean isInternal) {
        this.asynchronous = asynchronous;
        this.verbose = verbose;
        this.isInternal = isInternal;
        this.reusableRemoteSshSession = false;
    }

    public TerminalActions(String dockerName, String dockerUsername) {
        this.dockerName = dockerName;
        this.dockerUsername = dockerUsername;
        this.reusableRemoteSshSession = false;
    }

    public TerminalActions(String sshHostName, int sshPortNumber, String sshUsername, String sshKeyFileFolderName,
                           String sshKeyFileName) {
        this(sshHostName, sshPortNumber, sshUsername, sshKeyFileFolderName, sshKeyFileName, false);
    }

    /**
     * @param reusableRemoteSshSession when {@code true}, reuse one SSH session until {@link #quit()}.
     */
    public TerminalActions(String sshHostName, int sshPortNumber, String sshUsername, String sshKeyFileFolderName,
                           String sshKeyFileName, boolean reusableRemoteSshSession) {
        this.sshHostName = sshHostName;
        this.sshPortNumber = sshPortNumber;
        this.sshUsername = sshUsername;
        this.sshKeyFileFolderName = sshKeyFileFolderName;
        this.sshKeyFileName = sshKeyFileName;
        this.reusableRemoteSshSession = reusableRemoteSshSession;
    }

    public TerminalActions(String sshHostName, int sshPortNumber, String sshUsername, String sshKeyFileFolderName,
                           String sshKeyFileName, String dockerName, String dockerUsername) {
        this(sshHostName, sshPortNumber, sshUsername, sshKeyFileFolderName, sshKeyFileName, dockerName, dockerUsername,
                false);
    }

    public TerminalActions(String sshHostName, int sshPortNumber, String sshUsername, String sshKeyFileFolderName,
                           String sshKeyFileName, String dockerName, String dockerUsername,
                           boolean reusableRemoteSshSession) {
        this.sshHostName = sshHostName;
        this.sshPortNumber = sshPortNumber;
        this.sshUsername = sshUsername;
        this.sshKeyFileFolderName = sshKeyFileFolderName;
        this.sshKeyFileName = sshKeyFileName;
        this.dockerName = dockerName;
        this.dockerUsername = dockerUsername;
        this.reusableRemoteSshSession = reusableRemoteSshSession;
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

    public boolean isReusableRemoteSshSession() {
        return reusableRemoteSshSession;
    }

    /**
     * Closes a reusable remote SSH client if present. No-op for local-only instances.
     */
    public void quit() {
        synchronized (this) {
            if (reusableSshClient != null) {
                reusableSshClient.close();
                reusableSshClient = null;
            }
        }
    }

    /**
     * JSch session when {@link #isReusableRemoteSshSession()} is {@code true} and {@link #isRemoteTerminal()}.
     */
    public Session getJschSession() throws JSchException {
        if (!isRemoteTerminal()) {
            throw new IllegalStateException("getJschSession() applies only to remote SSH TerminalActions.");
        }
        if (!reusableRemoteSshSession) {
            throw new IllegalStateException(
                    "Use a constructor with reusableRemoteSshSession=true for a stable JSch Session.");
        }
        ensureReusableClientConnected();
        return reusableSshClient.getJschSession();
    }

    /**
     * Advanced: {@link RemoteSshClient} when {@link #isReusableRemoteSshSession()} is {@code true}.
     */
    public RemoteSshClient getRemoteSshClient() throws JSchException {
        if (!isRemoteTerminal() || !reusableRemoteSshSession) {
            throw new IllegalStateException(
                    "Use a remote constructor with reusableRemoteSshSession=true for getRemoteSshClient().");
        }
        ensureReusableClientConnected();
        return reusableSshClient;
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
        List<String> expanded = ShellCommandNormalizer.expandSingleCommandChaining(new ArrayList<>(commands));
        String longCommand = RemoteCommandBundler.buildLongCommand(expanded, isDockerizedTerminal(), dockerName,
                dockerUsername);

        if (!isRemoteTerminal()) {
            lastRemoteCommandExitStatus = -1;
        }

        List<String> exitLogs = isRemoteTerminal() ? executeRemoteCommand(expanded, longCommand)
                : executeLocalCommand(expanded, longCommand);
        String log = exitLogs.get(0);
        String exitStatus = exitLogs.get(1);

        StringBuilder reportMessage = new StringBuilder();
        if (!sshHostName.isEmpty()) {
            reportMessage.append("Host Name: \"").append(sshHostName).append("\"");
            reportMessage.append(" | SSH Port Number: \"").append(sshPortNumber).append("\"");
            reportMessage.append(" | SSH Username: \"").append(sshUsername).append("\"");
        } else {
            reportMessage.append("Host Name: \"").append("localHost").append("\"");
        }
        if (sshKeyFileName != null && !sshKeyFileName.isEmpty()) {
            reportMessage.append(" | Key File: \"").append(sshKeyFileFolderName).append(sshKeyFileName).append("\"");
        }
        reportMessage.append(" | Command: \"").append(longCommand).append("\"");
        reportMessage.append(" | Exit Status: \"").append(exitStatus).append("\"");

        if (log != null) {
            if (!isInternal) {
                passAction(reportMessage.toString(), log);
            }
            return log;
        }
        return "";
    }

    public String performTerminalCommand(String command) {
        return performTerminalCommands(Collections.singletonList(command));
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

    private RemoteSshClient newRemoteSshClientForPolicy(SshSessionPolicy sessionPolicy) {
        if (isDockerizedTerminal()) {
            return new RemoteSshClient(sshHostName, sshPortNumber, sshUsername, sshKeyFileFolderName, sshKeyFileName,
                    dockerName, dockerUsername, sessionPolicy);
        }
        return new RemoteSshClient(sshHostName, sshPortNumber, sshUsername, sshKeyFileFolderName, sshKeyFileName,
                sessionPolicy);
    }

    private void ensureReusableClientConnected() throws JSchException {
        synchronized (this) {
            if (reusableSshClient == null) {
                reusableSshClient = newRemoteSshClientForPolicy(SshSessionPolicy.REUSE_SESSION);
            }
            reusableSshClient.connect();
        }
    }

    private List<String> executeLocalCommand(List<String> commands, String longCommand) {
        StringBuilder logs = new StringBuilder();
        StringBuilder exitStatuses = new StringBuilder();
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
            String cmd = command.contains(".bat") && !command.contains(".\\") && !command.matches("(^.:\\\\.*$)")
                    ? ".\\" + command
                    : command;
            ReportManager.logDiscrete("Executing: \"" + cmd + "\" locally.");
            try {
                ProcessBuilder pb = getProcessBuilder(cmd, finalDirectory, isWindows);
                pb.environment().put("JAVA_HOME", System.getProperty("java.home"));
                if (!asynchronous) {
                    pb.redirectErrorStream(true);
                    Process localProcess = pb.start();
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
                    localProcess.waitFor(SHAFT.Properties.timeouts.shellSessionTimeout(), TimeUnit.MINUTES);
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
            } catch (IOException | InterruptedException exception) {
                failAction(longCommand, exception);
            }
        });
        return Arrays.asList(logs.toString().trim(), exitStatuses.toString());
    }

    private ProcessBuilder getProcessBuilder(String command, String finalDirectory, boolean isWindows) {
        ProcessBuilder pb = new ProcessBuilder();
        pb.directory(new File(finalDirectory));

        if (isWindows) {
            if (asynchronous && verbose) {
                pb.command("powershell.exe", "-ExecutionPolicy", "Bypass", "Start-Process powershell.exe '-ExecutionPolicy Bypass -NoExit -WindowStyle Minimized -Command \"[Console]::Title = ''SHAFT_Engine''; " + command + "\"'");
            } else {
                pb.command("powershell.exe", "-ExecutionPolicy", "Bypass", "-Command", command);
            }
        } else {
            pb.command("sh", "-c", command);
        }
        return pb;
    }

    private List<String> executeRemoteCommand(List<String> commands, String longCommand) {
        StringBuilder logs = new StringBuilder();
        StringBuilder exitStatuses = new StringBuilder();
        lastRemoteCommandExitStatus = -1;
        ReportManager.logDiscrete(
                "Attempting to perform the following command remotely. Command: \"" + longCommand + "\"");
        try {
            if (reusableRemoteSshSession) {
                ensureReusableClientConnected();
                SshCommandResult result = reusableSshClient.performCommands(commands);
                logs.append(result.mergedOutput());
                exitStatuses.append(result.exitStatus());
                lastRemoteCommandExitStatus = result.exitStatus();
            } else {
                try (RemoteSshClient client = newRemoteSshClientForPolicy(SshSessionPolicy.NEW_SESSION_PER_COMMAND)) {
                    client.connect();
                    SshCommandResult result = client.performCommands(commands);
                    logs.append(result.mergedOutput());
                    exitStatuses.append(result.exitStatus());
                    lastRemoteCommandExitStatus = result.exitStatus();
                }
            }
        } catch (JSchException | IOException exception) {
            failAction(longCommand, exception);
        }
        return Arrays.asList(logs.toString(), exitStatuses.toString());
    }
}
