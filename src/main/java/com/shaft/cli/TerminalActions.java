package com.shaft.cli;

import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.shaft.tools.io.PropertyFileManager;
import com.shaft.tools.io.ReportManager;
import org.testng.Assert;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

@SuppressWarnings("unused")
public class TerminalActions {
    private String sshHostName = "";
    private String sshUsername;
    private String sshKeyFileFolderName;
    private String sshKeyFileName;
    private int sshPortNumber = 22;

    private String dockerName = "";
    private String dockerUsername;

    private boolean unattended = false;

    /**
     * This constructor is used for local terminal actions.
     */
    public TerminalActions() {
    }

    /**
     * This constructor is used for local terminal actions.
     *
     * @param unattended true for unattended execution of commands in a separate thread
     */
    public TerminalActions(boolean unattended) {
        this.unattended = unattended;
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

    private static String reportActionResult(String actionName, String testData, String log, Boolean passFailStatus) {
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "Terminal Action [" + actionName + "] successfully performed.";
        } else {
            message = "Terminal Action [" + actionName + "] failed.";
        }

        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && !testData.isEmpty() && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Terminal Action Test Data - " + actionName,
                    "Actual Value", testData);
            attachments.add(actualValueAttachment);
        } else if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data [" + testData + "].";
        }

        if (log != null && !log.trim().equals("")) {
            attachments.add(Arrays.asList("Terminal Action Actual Result", "Command Log", log));
        }

        if (!attachments.equals(new ArrayList<>())) {
            ReportManager.log(message, attachments);
        } else {
            ReportManager.log(message);
        }

        return message;
    }

    public boolean isRemoteTerminal() {
        return !sshHostName.equals("");
    }

    public boolean isDockerizedTerminal() {
        return !dockerName.equals("");
    }

    public String performTerminalCommands(List<String> commands) {
        String log = null;

        // Build long command and refactor for dockerized execution if needed
        String command = buildLongCommand(commands);

        // Perform command
        List<Object> teminalSession = executeCommand(command);

        String exitStatus = "unattended";
        if (!unattended) {
            // Capture logs and close readers
            BufferedReader reader = (BufferedReader) teminalSession.get(3);
            BufferedReader errorReader = (BufferedReader) teminalSession.get(4);
            log = captureTerminalLogs(reader, errorReader, command);

            // Retrieve the exit status of the executed command and destroy open sessions
            Session remoteSession = (Session) teminalSession.get(0);
            ChannelExec remoteChannelExecutor = (ChannelExec) teminalSession.get(1);
            Process localProcess = (Process) teminalSession.get(2);
            exitStatus = String.valueOf(getExitStatus(remoteSession, remoteChannelExecutor, localProcess));
        }
        // Prepare final log message
        StringBuilder reportMessage = new StringBuilder();
        if (!sshHostName.equals("")) {
            reportMessage.append("Host Name: \"").append(sshHostName).append("\"");
            reportMessage.append(" | SSH Port Number: \"").append(sshPortNumber).append("\"");
            reportMessage.append(" | SSH Username: \"").append(sshUsername).append("\"");
        } else {
            reportMessage.append("Host Name: \"" + "localHost" + "\"");
        }
        if (sshKeyFileName != null && !sshKeyFileName.equals("")) {
            reportMessage.append(" | Key File: \"").append(sshKeyFileFolderName).append(sshKeyFileName).append("\"");
        }
        reportMessage.append(" | Command: \"").append(command).append("\"");
        reportMessage.append(" | Exit Status: \"").append(exitStatus).append("\"");

        if (log != null) {
            passAction(reportMessage.toString(), log);
            return log;
        } else {
            return "";
        }
    }

    public String performTerminalCommand(String command) {
        return performTerminalCommands(Collections.singletonList(command));
    }

    public String getSshHostName() {
        return sshHostName;
    }

    public String getSshUsername() {
        return sshUsername;
    }

    public String getSshKeyFileFolderName() {
        return sshKeyFileFolderName;
    }

    public String getSshKeyFileName() {
        return sshKeyFileName;
    }

    public int getSshPortNumber() {
        return sshPortNumber;
    }

    public String getDockerName() {
        return dockerName;
    }

    public String getDockerUsername() {
        return dockerUsername;
    }

    private void passAction(String actionName, String testData, String log) {
        reportActionResult(actionName, testData, log, true);
    }

    private void passAction(String testData, String log) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(actionName, testData, log);
    }

    private void failAction(String actionName, String testData, Exception... rootCauseException) {
        String message = reportActionResult(actionName, testData, null, false);
        if (rootCauseException != null && rootCauseException.length >= 1) {
            Assert.fail(message, rootCauseException[0]);
        } else {
            Assert.fail(message);
        }
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
            if (sshKeyFileName != null && !sshKeyFileName.equals("")) {
                jsch.addIdentity(FileActions.getAbsolutePath(sshKeyFileFolderName, sshKeyFileName));
            }
            session = jsch.getSession(sshUsername, sshHostName, sshPortNumber);
            session.setConfig(config);
            session.connect();
            ReportManager.logDiscrete("Successfully created SSH Session.");
        } catch (JSchException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction(testData, rootCauseException);
        }
        return session;
    }

    private String buildLongCommand(List<String> commands) {
        StringBuilder command = new StringBuilder();
        // build long command
        for (Iterator<String> i = commands.iterator(); i.hasNext(); ) {
            if (command.length() == 0) {
                command.append(i.next());
            } else {
                command.append(" && ").append(i.next());
            }
        }

        // refactor long command for dockerized execution
        if (isDockerizedTerminal()) {
            if (System.getProperty("dockerCommandTimeout") == null) {
                PropertyFileManager.readPropertyFiles();
            }
            command.insert(0, "docker exec -u " + dockerUsername + " -i " + dockerName + " timeout "
                    + Integer.parseInt(System.getProperty("dockerCommandTimeout")) + " sh -c '");
            command.append("'");
        }
        return command.toString();
    }

    private List<Object> executeCommand(String command) {
        BufferedReader reader = null;
        BufferedReader errorReader = null;
        Session remoteSession = null;
        ChannelExec remoteChannelExecutor = null;
        Process localProcess = null;
        try {
            if (isRemoteTerminal()) {
                int sessionTimeout = Integer.parseInt(System.getProperty("shellSessionTimeout")) * 1000;
                // remote execution
                ReportManager.logDiscrete(
                        "Attempting to perform the following command remotely. Command: [" + command + "]");
                remoteSession = createSSHsession();
                if (remoteSession != null) {
                    remoteSession.setTimeout(sessionTimeout);
                    remoteChannelExecutor = (ChannelExec) remoteSession.openChannel("exec");
                    remoteChannelExecutor.setCommand(command);
                    remoteChannelExecutor.connect();
                    reader = new BufferedReader(new InputStreamReader(remoteChannelExecutor.getInputStream()));
                    errorReader = new BufferedReader(new InputStreamReader(remoteChannelExecutor.getErrStream()));
                }
            } else {
                // local execution
                ReportManager
                        .logDiscrete("Attempting to perform the following command locally. Command: [" + command + "]");
                localProcess = Runtime.getRuntime().exec(command);
                if (!unattended) {
                    localProcess.waitFor();
                }
                reader = new BufferedReader(new InputStreamReader(localProcess.getInputStream()));
                errorReader = new BufferedReader(new InputStreamReader(localProcess.getErrorStream()));
            }
        } catch (InterruptedException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction(command, rootCauseException);
            Thread.currentThread().interrupt();
        } catch (IOException | NullPointerException | JSchException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction(command, rootCauseException);
        }
        return Arrays.asList(remoteSession, remoteChannelExecutor, localProcess, reader, errorReader);
    }

    private String captureTerminalLogs(BufferedReader reader, BufferedReader errorReader, String command) {
        StringBuilder logBuilder = new StringBuilder();
        try {
            String logLine;
            logBuilder.append(readConsoleLogs(reader));
            logBuilder.append(readConsoleLogs(errorReader));
        } catch (IOException rootCauseException) {
            ReportManager.log(rootCauseException);
            failAction(command, rootCauseException);
        }
        return logBuilder.toString();
    }

    private String readConsoleLogs(BufferedReader errorReader) throws IOException {
        StringBuilder logBuilder = new StringBuilder();
        if (errorReader != null) {
            String logLine;
            while ((logLine = errorReader.readLine()) != null) {
                if (logBuilder.length() == 0) {
                    logBuilder.append(logLine);
                } else {
                    logBuilder.append(System.lineSeparator()).append(logLine);
                }
            }
            errorReader.close();
        }
        return logBuilder.toString();
    }

    private int getExitStatus(Session remoteSession, ChannelExec remoteChannelExecutor, Process localProcess) {
        int exitStatus = 0;
        if (remoteSession != null && remoteChannelExecutor != null) {
            exitStatus = remoteChannelExecutor.getExitStatus();
            remoteSession.disconnect();
        } else if (localProcess != null) {
            exitStatus = localProcess.exitValue();
            localProcess.destroy();
        }
        return exitStatus;
    }

}
