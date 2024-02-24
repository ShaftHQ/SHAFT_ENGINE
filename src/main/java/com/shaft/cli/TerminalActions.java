package com.shaft.cli;

import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
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

        if (!attachments.equals(new ArrayList<>())) {
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
        var internalCommands = commands;

        // Build long command and refactor for dockerized execution if needed
        String longCommand = buildLongCommand(internalCommands);

        if (internalCommands.size() == 1) {
            if (internalCommands.getFirst().contains(" && ")) {
                internalCommands = List.of(internalCommands.getFirst().split(" && "));
            } else if (internalCommands.getFirst().contains(" ; ")) {
                internalCommands = List.of(internalCommands.getFirst().split(" ; "));
            }
        }

        // Perform command
        List<String> exitLogs = isRemoteTerminal() ? executeRemoteCommand(internalCommands, longCommand) : executeLocalCommand(internalCommands, longCommand);
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

    private String buildLongCommand(List<String> commands) {
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
            command.insert(0, "docker exec -u " + dockerUsername + " -i " + dockerName + " timeout "
                    + SHAFT.Properties.timeouts.dockerCommandTimeout() + " sh -c '");
            command.append("'");
        }
        return command.toString();
    }

    private List<String> executeLocalCommand(List<String> commands, String longCommand) {
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
                ProcessBuilder pb = getProcessBuilder(command, finalDirectory, isWindows);
                pb.environment().put("JAVA_HOME", System.getProperty("java.home"));
                if (!asynchronous) {
                    pb.redirectErrorStream(true);
                    Process localProcess = pb.start();
                    // output logs
                    String line;
                    InputStreamReader isr = new InputStreamReader(localProcess.getInputStream());
                    BufferedReader rdr = new BufferedReader(isr);
                    while ((line = rdr.readLine()) != null) {
                        if (Boolean.TRUE.equals(verbose)) {
                            ReportManager.logDiscrete(line);
                        }
                        logs.append(line);
                        logs.append("\n");
                    }
                    isr = new InputStreamReader(localProcess.getErrorStream());
                    rdr = new BufferedReader(isr);
                    while ((line = rdr.readLine()) != null) {
                        if (Boolean.TRUE.equals(verbose)) {
                            ReportManager.logDiscrete(line);
                        }
                        logs.append("\n");
                        logs.append(line);
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
            } catch (IOException | InterruptedException exception) {
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
                pb.command("powershell.exe", "Start-Process powershell.exe '-NoExit -WindowStyle Minimized \"[Console]::Title = ''SHAFT_Engine''; " + command + "\"'");
            } else {
                pb.command("powershell.exe", "-Command", command);
            }
        } else {
            pb.command("sh", "-c", command);
        }
        return pb;
    }

    private List<String> executeRemoteCommand(List<String> commands, String longCommand) {
        StringBuilder logs = new StringBuilder();
        StringBuilder exitStatuses = new StringBuilder();
        int sessionTimeout = Integer.parseInt(String.valueOf(SHAFT.Properties.timeouts.shellSessionTimeout() * 1000));
        // remote execution
        ReportManager.logDiscrete(
                "Attempting to perform the following command remotely. Command: \"" + longCommand + "\"");
        Session remoteSession = createSSHsession();
        if (remoteSession != null) {
            try {
                remoteSession.setTimeout(sessionTimeout);
                ChannelExec remoteChannelExecutor = (ChannelExec) remoteSession.openChannel("exec");
                remoteChannelExecutor.setCommand(longCommand);
                remoteChannelExecutor.connect();

                // Capture logs and close readers
                BufferedReader reader = new BufferedReader(new InputStreamReader(remoteChannelExecutor.getInputStream()));
                BufferedReader errorReader = new BufferedReader(new InputStreamReader(remoteChannelExecutor.getErrStream()));
                logs.append(readConsoleLogs(reader));
                logs.append(readConsoleLogs(errorReader));

                // Retrieve the exit status of the executed command and destroy open sessions
                exitStatuses.append(remoteChannelExecutor.getExitStatus());
                remoteSession.disconnect();
            } catch (JSchException | IOException exception) {
                failAction(longCommand, exception);
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
            reader.close();
        }
        return logBuilder.toString();
    }
}
