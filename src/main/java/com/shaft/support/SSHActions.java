package com.shaft.support;

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
import com.shaft.io.FileManager;
import com.shaft.io.ReportManager;

public class SSHActions {
    String hostname;
    int sshPortNumber;
    String username;
    String keyFileFolderName;
    String keyFileName;
    String dockerName;
    String dockerUsername;

    public SSHActions(String hostname, int sshPortNumber, String username, String keyFileFolderName, String keyFileName, String dockerName, String dockerUsername) {
	this.hostname = hostname;
	this.sshPortNumber = sshPortNumber;
	this.username = username;
	this.keyFileFolderName = keyFileFolderName;
	this.keyFileName = keyFileName;
	this.dockerName = dockerName;
	this.dockerUsername = dockerUsername;
    }

    public SSHActions(String hostname, int sshPortNumber, String username, String keyFileFolderName, String keyFileName) {
	this.hostname = hostname;
	this.sshPortNumber = sshPortNumber;
	this.username = username;
	this.keyFileFolderName = keyFileFolderName;
	this.keyFileName = keyFileName;
    }

    public SSHActions() {
    }

    private void passAction(String actionName, String testData, String log) {
	String message = "Successfully performed action [" + actionName + "].";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	ReportManager.log(message);
	if (log != null) {
	    ReportManager.attachAsStep("API Response", "Command Log", log);
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

    private Session createSSHsession() {
	Session session = null;
	String testData = hostname + ", " + sshPortNumber + ", " + username + ", " + keyFileFolderName + ", " + keyFileName;
	try {
	    Properties config = new Properties();
	    config.put("StrictHostKeyChecking", "no");
	    JSch jsch = new JSch();

	    jsch.addIdentity(FileManager.getAbsolutePath(keyFileFolderName, keyFileName));
	    session = jsch.getSession(username, hostname, sshPortNumber);
	    session.setConfig(config);

	    session.connect();
	    // System.out.println("Connected");
	    passAction("createSSHsession", testData);
	} catch (JSchException e) {
	    ReportManager.log(e);
	    failAction("createSSHsession", testData);
	}
	return session;
    }

    private String performSSHcommand(Session session, List<String> commands) {
	StringBuilder logBuilder = new StringBuilder();
	String log = "";
	String command = "";

	try {
	    for (Iterator<String> i = commands.iterator(); i.hasNext();) {
		command = i.next();
		ReportManager.log("Attempting to perform the following command remotely. Command: [" + command + "]");

		ChannelExec channelExec = (ChannelExec) session.openChannel("exec");
		channelExec.setCommand(command);
		channelExec.connect();
		BufferedReader reader = new BufferedReader(new InputStreamReader(channelExec.getInputStream()));
		BufferedReader errorReader = new BufferedReader(new InputStreamReader(channelExec.getErrStream()));

		String logLine = "";

		while ((logLine = reader.readLine()) != null) {
		    logBuilder.append(System.lineSeparator() + logLine);
		}
		while ((logLine = errorReader.readLine()) != null) {
		    logBuilder.append(System.lineSeparator() + logLine);
		}

		log = logBuilder.toString();

		// String line = "";
		// while ((line = reader.readLine()) != null) {
		// log = log + System.lineSeparator() + line;
		// // System.out.println(line);
		// }
		// while ((line = errorReader.readLine()) != null) {
		// log = log + System.lineSeparator() + line;
		// // System.out.println(line);
		// }

		// Command execution completed here.

		// Retrieve the exit status of the executed command
		int exitStatus = channelExec.getExitStatus();
		if (exitStatus > 0) {
		    // System.out.println("Remote script exec error! " + exitStatus);
		}

		reader.close();
		errorReader.close();

	    }
	    // Disconnect the Session
	    session.disconnect();
	    // System.out.println("DONE");
	} catch (IOException | NullPointerException | JSchException e) {
	    ReportManager.log(e);
	    failAction("performSSHcommand", String.join(" && ", commands), log);
	    return log;
	}
	passAction("performSSHcommand", String.join(" && ", commands), log);
	return log;
    }

    /**
     * Establish a connection to a remote SSH server using a key file, then perform
     * a certain command and return its logs.
     * 
     * @param commands
     *            The target command that should be executed on the SSH server
     * @return a string value that contains the execution log of the performed
     *         command(s)
     */
    public String performSSHcommand(List<String> commands) {

	Session session = createSSHsession();
	return performSSHcommand(session, commands);
    }

    public String performSSHcommand(String command) {
	return performSSHcommand(Arrays.asList(command));
    }

    public String performDockerizedSSHcommand(List<String> commands) {
	// List<String> dockerCommands = Arrays.asList();

	commands.replaceAll(command -> "docker exec -u " + dockerUsername + " -i " + dockerName + " sh -c " + command);

	// commands.forEach(new Consumer<String>() {
	// public void accept(String command) {
	// dockerCommands.add("docker exec -u " + dockerUsername + " -i " + dockerName +
	// " sh -c " + command);
	// }
	// });

	Session session = createSSHsession();
	return performSSHcommand(session, (List<String>) commands);
    }

    public String performDockerizedSSHcommand(String command) {
	String dockerizedCommand = "docker exec -u " + dockerUsername + " -i " + dockerName + " sh -c " + command;
	return performDockerizedSSHcommand(Arrays.asList(dockerizedCommand));
    }

    public String executeShellCommand(List<String> commands) {
	String log = "";
	StringBuilder logBuilder = new StringBuilder();
	String command = "";

	try {
	    for (Iterator<String> i = commands.iterator(); i.hasNext();) {
		command = i.next();

		ReportManager.log("Attempting to perform the following command locally. Command: [" + command + "]");

		Process p = Runtime.getRuntime().exec(command);
		p.waitFor();

		BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
		BufferedReader errorReader = new BufferedReader(new InputStreamReader(p.getErrorStream()));

		String logLine = "";

		while ((logLine = reader.readLine()) != null) {
		    logBuilder.append(System.lineSeparator() + logLine);
		}
		while ((logLine = errorReader.readLine()) != null) {
		    logBuilder.append(System.lineSeparator() + logLine);
		}

		log = logBuilder.toString();

		// String line = "";
		// while ((line = reader.readLine()) != null) {
		// log = log + System.lineSeparator() + line;
		// }
		// while ((line = errorReader.readLine()) != null) {
		// log = log + System.lineSeparator() + line;
		// }
		reader.close();
		errorReader.close();
	    }
	} catch (Exception e) {
	    // this used to throw IOException | InterruptedException e, but was changed to
	    // the generic exception to resolve the sonar lint comment
	    ReportManager.log(e);
	    failAction("executeShellCommand", command, log);
	    return log;
	}

	passAction("executeShellCommand", String.join(" && ", commands), log);
	return log;
    }

    public String executeShellCommand(String command) {
	return executeShellCommand(Arrays.asList(command));
    }
}
