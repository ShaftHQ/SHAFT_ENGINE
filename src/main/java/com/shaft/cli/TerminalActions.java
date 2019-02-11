package com.shaft.cli;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.testng.Assert;

import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.shaft.io.FileActions;
import com.shaft.io.ReportManager;

public class TerminalActions {
    private String sshHostName = "";
    private String sshUsername;
    private String sshKeyFileFolderName;
    private String sshKeyFileName;
    private int sshPortNumber = 22;

    private String dockerName = "";
    private String dockerUsername;

    /**
     * This constructor is used for local terminal actions.
     */
    public TerminalActions() {
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

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Reporting Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private void passAction(String actionName, String testData, String log) {
	String message = "Successfully performed action [" + actionName + "].";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	ReportManager.log(message);
	if (log != null) {
	    ReportManager.attachAsStep("CLI Response", "Terminal Log", log);
	}
    }

    private void passAction(String actionName, String testData) {
	passAction(actionName, testData, null);
    }

    private void failAction(String actionName, String testData, String log) {
	String message = "Failed to perform action [" + actionName + "].";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	ReportManager.log(message);
	if (log != null) {
	    ReportManager.attachAsStep("API Response", "Command Log", log);
	}
	Assert.fail(message);
    }

    private void failAction(String actionName, String testData) {
	failAction(actionName, testData, null);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Preparation and Support Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private Session createSSHsession() {
	Session session = null;
	String testData = sshHostName + ", " + sshPortNumber + ", " + sshUsername + ", " + sshKeyFileFolderName + ", "
		+ sshKeyFileName;
	try {
	    Properties config = new Properties();
	    config.put("StrictHostKeyChecking", "no");
	    JSch jsch = new JSch();

	    jsch.addIdentity(FileActions.getAbsolutePath(sshKeyFileFolderName, sshKeyFileName));
	    session = jsch.getSession(sshUsername, sshHostName, sshPortNumber);
	    session.setConfig(config);

	    session.connect();
	    Boolean discreetLoggingState = ReportManager.isDiscreetLogging();
	    ReportManager.setDiscreetLogging(true);
	    passAction("createSSHsession", testData);
	    ReportManager.setDiscreetLogging(discreetLoggingState);
	} catch (JSchException e) {
	    ReportManager.log(e);
	    failAction("createSSHsession", testData);
	}
	return session;
    }

    private boolean isRemoteTerminal() {
	return !sshHostName.equals("");
    }

    private boolean isDockerizedTerminal() {
	return !dockerName.equals("");
    }

    private String buildLongCommand(List<String> commands) {
	StringBuilder command = new StringBuilder();
	// build long command
	for (Iterator<String> i = commands.iterator(); i.hasNext();) {
	    if (command.length() == 0) {
		command = command.append(i.next());
	    } else {
		command = command.append(" && " + i.next());
	    }
	}

	// refactor long command for dockerized execution
	if (isDockerizedTerminal()) {
	    command.insert(0, "docker exec -u " + dockerUsername + " -i " + dockerName + " sh -c '");
	    command.append("'");
	}
	return command.toString();
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [Public] Core Terminal Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    public String performTerminalCommands(List<String> commands) {
	StringBuilder logBuilder = new StringBuilder();
	String log = "";

	// Build long command and refactor for dockerized execution if needed
	String command = buildLongCommand(commands);

	// Attempt to execute long command
	try {
	    // Declare Buffered Readers to track terminal session output
	    BufferedReader reader;
	    BufferedReader errorReader;

	    // Declare Variables which will need to be destroyed at the end of the sessions
	    Session session = null;
	    ChannelExec channelExec = null;
	    Process p = null;

	    // Perform command
	    if (isRemoteTerminal()) {
		// remote execution
		ReportManager.logDiscreet(
			"Attempting to perform the following command remotely. Command: [" + command + "]");
		session = createSSHsession();
		channelExec = (ChannelExec) session.openChannel("exec");
		channelExec.setCommand(command);
		channelExec.connect();
		reader = new BufferedReader(new InputStreamReader(channelExec.getInputStream()));
		errorReader = new BufferedReader(new InputStreamReader(channelExec.getErrStream()));
	    } else {
		// local execution
		ReportManager
			.logDiscreet("Attempting to perform the following command locally. Command: [" + command + "]");
		p = Runtime.getRuntime().exec(command);
		p.waitFor();
		reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
		errorReader = new BufferedReader(new InputStreamReader(p.getErrorStream()));
	    }

	    // Capture logs and close readers
	    String logLine = "";
	    while ((logLine = reader.readLine()) != null) {
		logBuilder.append(System.lineSeparator() + logLine);
	    }
	    while ((logLine = errorReader.readLine()) != null) {
		logBuilder.append(System.lineSeparator() + logLine);
	    }
	    log = logBuilder.toString();
	    reader.close();
	    errorReader.close();

	    // Retrieve the exit status of the executed command and destroy open sessions
	    int exitStatus = 0;
	    if (session != null && channelExec != null) {
		exitStatus = channelExec.getExitStatus();
		session.disconnect();
	    } else if (p != null) {
		exitStatus = p.exitValue();
		p.destroy();
	    }

	    // Report Command exit status
	    ReportManager.logDiscreet("Command Executed with exit status: [" + exitStatus + "]");
	    if (exitStatus > 0) {
		// Remote script exec error!
	    }
	} catch (IOException | NullPointerException | JSchException | InterruptedException e) {
	    if (e.getMessage().contains("Cannot run program \"cd\": error=2, No such file or directory")) {
		ReportManager.log("Failed to perform command [" + command
			+ "] because you cannot use 'cd' with a local terminal. Try to do your action directly instead.");
	    } else if (e.getMessage().contains("Connection refused (Connection refused)")) {
		ReportManager.log("Failed to connect to remote machine [" + sshUsername + "@" + sshHostName + ":"
			+ sshPortNumber + "] using this key ["
			+ FileActions.getAbsolutePath(sshKeyFileFolderName, sshKeyFileName)
			+ "]. Please confirm that this data is correct.");
	    } else {
		ReportManager.log(e);
	    }
	    failAction("performTerminalCommands", command, log);
	    return log;
	}

	passAction("performTerminalCommands", command, log);
	return log;
    }

    public String performTerminalCommand(String command) {
	return performTerminalCommands(Arrays.asList(command));
    }

}
