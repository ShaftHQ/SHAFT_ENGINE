package com.shaft.cli;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.TrueFileFilter;
import org.apache.commons.lang3.SystemUtils;
import org.testng.Assert;

import com.google.common.hash.Hashing;
import com.shaft.tools.io.ReportManager;

public class FileActions {
    private FileActions() {
	throw new IllegalStateException("Utility class");
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Reporting Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static void failAction(String actionName, String testData, String log) {
	String message = "Failed to perform action [" + actionName + "].";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	if ((log != null) && (!log.trim().equals(""))) {
	    ReportManager.log(message, Arrays.asList(Arrays.asList("API Response", "Command Log", log)));
	} else {
	    ReportManager.log(message);

	}
	Assert.fail(message);
    }

    private static void failAction(String actionName, String testData) {
	failAction(actionName, testData, null);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Preparation and Support Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static boolean isTargetOSUnixBased() {
	if (System.getProperty("executionAddress").trim().equals("local")) {
	    // local execution
	    if (SystemUtils.IS_OS_WINDOWS) {
		return false;
	    } else if (SystemUtils.IS_OS_LINUX || SystemUtils.IS_OS_MAC) {
		return true;
	    } else {
		ReportManager.logDiscrete("Unsupported OS type, will assume it's unix based.");
		return true;
	    }
	} else {
	    // remote execution
	    String targetOS = System.getProperty("targetOperatingSystem");
	    if (targetOS.equals("Windows-64")) {
		return false;
	    } else if (targetOS.equals("Linux-64") || targetOS.equals("Mac-64")) {
		return true;
	    } else {
		ReportManager.logDiscrete("Unsupported OS type, will assume it's unix based.");
		return true;
	    }
	}
    }

    private static void copyFile(File sourceFile, File destinationFile) {
	try {
	    FileUtils.copyFile(sourceFile, destinationFile);
	} catch (IOException e) {
	    ReportManager.log(e);
	}
    }

    /**
     * zip the folders
     * 
     * @param srcFolder
     * @param destZipFile
     */
    private static void zipFolder(String srcFolder, String destZipFile) {
	/*
	 * create the output stream to zip file result
	 */
	try (FileOutputStream fileWriter = new FileOutputStream(destZipFile);
		ZipOutputStream zip = new ZipOutputStream(fileWriter);) {

	    /*
	     * add the folder to the zip
	     */
	    addFolderToZip("", srcFolder, zip);
	    /*
	     * close the zip objects
	     */
	    zip.flush();
	} catch (IOException e) {
	    ReportManager.log(e);
	}
    }

    /**
     * recursively add files to the zip files
     * 
     * @param path
     * @param srcFile
     * @param zip
     * @param flag
     * @throws IOException
     */
    private static void addFileToZip(String path, String srcFile, ZipOutputStream zip, boolean flag)
	    throws IOException {
	/*
	 * create the file object for inputs
	 */
	File folder = new File(srcFile);

	/*
	 * if the folder is empty add empty folder to the Zip file
	 */
	if (flag) {
	    zip.putNextEntry(new ZipEntry(path + FileSystems.getDefault().getSeparator() + folder.getName()
		    + FileSystems.getDefault().getSeparator()));
	} else { /*
		  * if the current name is directory, recursively traverse it to get the files
		  */
	    if (folder.isDirectory()) {
		/*
		 * if folder is not empty
		 */
		addFolderToZip(path, srcFile, zip);
	    } else {
		/*
		 * write the file to the output
		 */
		try (FileInputStream in = new FileInputStream(srcFile);) {
		    byte[] buf = new byte[1024];
		    int len;
		    zip.putNextEntry(new ZipEntry(path + FileSystems.getDefault().getSeparator() + folder.getName()));
		    while ((len = in.read(buf)) > 0) {
			/*
			 * Write the Result
			 */
			zip.write(buf, 0, len);
		    }
		} catch (Exception e) {
		    ReportManager.log(e);
		}
	    }
	}
    }

    /**
     * add folder to the zip file
     * 
     * @param path
     * @param srcFolder
     * @param zip
     * @throws IOException
     */
    private static void addFolderToZip(String path, String srcFolder, ZipOutputStream zip) throws IOException {
	File folder = new File(srcFolder);

	/*
	 * check the empty folder
	 */
	if (folder.list().length == 0) {
	    addFileToZip(path, srcFolder, zip, true);
	} else {
	    /*
	     * list the files in the folder
	     */
	    for (String fileName : folder.list()) {
		if (path.equals("")) {
		    addFileToZip(folder.getName(), srcFolder + FileSystems.getDefault().getSeparator() + fileName, zip,
			    false);
		} else {
		    addFileToZip(path + FileSystems.getDefault().getSeparator() + folder.getName(),
			    srcFolder + FileSystems.getDefault().getSeparator() + fileName, zip, false);
		}
	    }
	}
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [Public] Core File Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * Copies a file from sourceFilePath to destinationFilePath on the local storage
     * 
     * @param sourceFilePath      the full (absolute) path of the source file that
     *                            will be copied
     * @param destinationFilePath the full (absolute) path of the desired location
     *                            and file name for the newly created copied file
     */
    public static void copyFile(String sourceFilePath, String destinationFilePath) {
	File sourceFile = new File(sourceFilePath);
	File destinationFile = new File(destinationFilePath);
	copyFile(sourceFile, destinationFile);
    }

    /**
     * Copies files from sourceDirectory to destinationDirectory using the provided
     * terminalSession. References: https://www.computerhope.com/unix/ucp.htm
     * https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/robocopy
     * 
     * @param terminalSession      an object that determines the type of
     *                             terminalSession which will be used to execute
     *                             this File Action
     * @param sourceDirectory      full path to the sourceDirectory
     * @param destinationDirectory full path to the destinationDirectory
     * @param fileName             target fileName
     * @return a string value that holds the result of this terminal command
     */
    public static String copyFile(TerminalActions terminalSession, String sourceDirectory, String destinationDirectory,
	    String fileName) {
	String command;
	if (isTargetOSUnixBased()) {
	    if (fileName.trim().equals("")) {
		command = "rsync --verbose --recursive " + sourceDirectory + File.separator + " "
			+ destinationDirectory;
	    } else {
		command = "rsync --verbose --recursive " + sourceDirectory + File.separator + fileName + " "
			+ destinationDirectory + File.separator;
	    }
	} else {
	    command = "robocopy  /e /v /fp " + sourceDirectory + " " + destinationDirectory + " " + fileName;
	}
	return terminalSession.performTerminalCommand(command);
    }

    public static String listFilesInDirectory(String targetDirectory) {
	StringBuilder files = new StringBuilder();
	try {
	    Collection<File> filesList = FileUtils.listFiles(new File(targetDirectory), TrueFileFilter.TRUE,
		    TrueFileFilter.TRUE);
	    filesList.forEach(file -> files.append(file.getName() + System.lineSeparator()));
	} catch (IllegalArgumentException e) {
	    ReportManager.log(e);
	    failAction("listFilesInDirectory", "Failed to list files in this directory: \"" + targetDirectory + "\"");
	}
	return files.toString().trim();
    }

    /**
     * Lists all files inside the targetDirectory
     * 
     * @param terminalSession an object that determines the type of terminalSession
     *                        which will be used to execute this File Action
     * @param targetDirectory full path to the targetDirectory
     * @return a string value that holds the result of this terminal command
     */
    public static String listFilesInDirectory(TerminalActions terminalSession, String targetDirectory) {
	List<String> commands;
	if (isTargetOSUnixBased()) {
	    commands = Arrays.asList("ls " + targetDirectory);
	} else {
	    commands = Arrays.asList("dir " + targetDirectory);
	}
	return terminalSession.performTerminalCommands(commands);
    }

    /**
     * This method is used to compute the SHA256 checksum for any file. This
     * checksum can be used to compare two files and confirm that they are
     * identical, regardless of the file type.
     * 
     * @param terminalSession                    provides information about the
     *                                           machine which contains the target
     *                                           file
     * @param targetFileFolderPath               the full absolute path of the
     *                                           folder that contains the target
     *                                           file, must end with a slash "/"
     * @param targetFileName                     the name and extension of the
     *                                           target file
     * @param pathToTempDirectoryOnRemoteMachine [OPTIONAL] to be used in case of
     *                                           dockerized target machine. This is
     *                                           a temporary directory that will be
     *                                           created on the remote machine to
     *                                           extract a file from inside a
     *                                           docker, and will be deleted
     *                                           afterwards
     * @return a string that holds the SHA256 checksum for the target file
     */
    public static String getFileChecksum(TerminalActions terminalSession, String targetFileFolderPath,
	    String targetFileName, String... pathToTempDirectoryOnRemoteMachine) {

	String targetFilePath = copyFileToLocalMachine(terminalSession, targetFileFolderPath, targetFileName,
		pathToTempDirectoryOnRemoteMachine);

	// read file
	byte[] fileBytes;
	String sha256 = "";
	try {
	    fileBytes = Files.readAllBytes(Paths.get(targetFilePath));
	    sha256 = Hashing.sha256().hashBytes(fileBytes).toString();
	} catch (IOException e) {
	    ReportManager.log(e);
	    failAction("getFileChecksum", "Failed to read file \"" + targetFilePath + "\"");
	}
	ReportManager.logDiscrete(
		"Successfully calculated SHA-256: \"" + sha256 + "\" from this file \"" + targetFilePath + "\"");
	return sha256;
    }

    /**
     * This method is used to copy a certain file from a remote machine (dockerized
     * or not) to the current execution machine.
     * 
     * @param terminalSession                    provides information about the
     *                                           machine which contains the target
     *                                           file
     * @param targetFileFolderPath               the full absolute path of the
     *                                           folder that contains the target
     *                                           file, must end with a slash "/"
     * @param targetFileName                     the name and extension of the
     *                                           target file
     * @param pathToTempDirectoryOnRemoteMachine [OPTIONAL] to be used in case of
     *                                           dockerized target machine. This is
     *                                           a temporary directory that will be
     *                                           created on the remote machine to
     *                                           extract a file from inside a
     *                                           docker, and will be deleted
     *                                           afterwards
     * @return a string that holds the full absolute path (inside a temporary
     *         folder) for the file that was copied to the local machine
     */
    public static String copyFileToLocalMachine(TerminalActions terminalSession, String targetFileFolderPath,
	    String targetFileName, String... pathToTempDirectoryOnRemoteMachine) {
	String targetFilePath = targetFileFolderPath + targetFileName;
	TerminalActions terminalSessionForRemoteMachine = new TerminalActions();

	// fetch file from inside docker to the machine itself
	if (terminalSession.isDockerizedTerminal()) {
	    terminalSessionForRemoteMachine = new TerminalActions(terminalSession.getSshHostName(),
		    terminalSession.getSshPortNumber(), terminalSession.getSshUsername(),
		    terminalSession.getSshKeyFileFolderName(), terminalSession.getSshKeyFileName());

	    // copy from docker to machine
	    terminalSessionForRemoteMachine.performTerminalCommand("rm -r " + pathToTempDirectoryOnRemoteMachine[0]);
	    terminalSessionForRemoteMachine
		    .performTerminalCommand("mkdir -p " + pathToTempDirectoryOnRemoteMachine[0] + targetFileFolderPath);
	    terminalSessionForRemoteMachine.performTerminalCommand("docker cp " + terminalSession.getDockerName() + ":"
		    + targetFilePath + " " + pathToTempDirectoryOnRemoteMachine[0] + targetFilePath);
	    targetFilePath = pathToTempDirectoryOnRemoteMachine[0] + targetFilePath;
	}

	// fetch file from terminal session to local machine
	if (terminalSession.isRemoteTerminal()) {
	    // remote regular
	    String sshParameters = "-i " + FileActions.getAbsolutePath(terminalSession.getSshKeyFileFolderName(),
		    terminalSession.getSshKeyFileName()) + " -P " + terminalSession.getSshPortNumber();

	    String pathToRemoteFileThatWillBeCopied = targetFilePath;
	    String source = terminalSession.getSshUsername() + "@" + terminalSession.getSshHostName() + ":"
		    + pathToRemoteFileThatWillBeCopied;

	    // creating local temp directory
	    String pathToLocalParentFolder = FileActions.getAbsolutePath("target/temp");
	    FileActions.deleteFolder(pathToLocalParentFolder);
	    FileActions.createFolder(pathToLocalParentFolder);

	    String destination = pathToLocalParentFolder + "/" + targetFileName;
	    targetFilePath = destination;

	    String command = "scp -v -o StrictHostKeyChecking=no " + sshParameters + " -r " + source + " "
		    + destination;

	    // restricting file access to bypass jenkins issue
	    (new TerminalActions()).performTerminalCommand("chmod 400 " + FileActions
		    .getAbsolutePath(terminalSession.getSshKeyFileFolderName(), terminalSession.getSshKeyFileName()));

	    (new TerminalActions()).performTerminalCommand(command);
	} else {
	    // local regular
	    // it's already on the local machine so no need to do anything here
	}

	// clean temp directory on remote machine
	if (terminalSession.isDockerizedTerminal() && terminalSession.isRemoteTerminal()) {
	    terminalSessionForRemoteMachine.performTerminalCommand("rm -r " + pathToTempDirectoryOnRemoteMachine);
	}
	return targetFilePath;
    }

    /**
     * Deletes a file from the local storage
     * 
     * @param filePath the full (absolute) path of the source file that will be
     *                 deleted
     */
    public static void deleteFile(String filePath) {
	FileUtils.deleteQuietly(new File(filePath));
    }

    public static void writeToFile(String fileFolderName, String fileName, List<String> text) {
	byte[] textToBytes = String.join(System.lineSeparator(), text).getBytes();
	writeToFile(fileFolderName, fileName, textToBytes);
    }

    public static void writeToFile(String fileFolderName, String fileName, byte[] content) {
	String absoluteFilePath = getAbsolutePath(fileFolderName, fileName);
	try {
	    Path filePath = Paths.get(absoluteFilePath);
	    Path parentDir = filePath.getParent();
	    if (!parentDir.toFile().exists()) {
		Files.createDirectories(parentDir);
	    }
	    Files.write(filePath, content);
	} catch (InvalidPathException | IOException e) {
	    ReportManager.log(e);
	    failAction("writeToFile", "Folder Name: \"" + fileFolderName + "\", File Name \"" + fileName + "\".");
	}
    }

    public static void writeToFile(String fileFolderName, String fileName, String text) {
	byte[] textToBytes = text.getBytes();
	writeToFile(fileFolderName, fileName, textToBytes);
    }

    public static String readFromFile(String fileFolderName, String fileName) {
	String text = "";
	String absoluteFilePath = getAbsolutePath(fileFolderName, fileName);
	Path filePath = Paths.get(absoluteFilePath);

	try {
	    text = String.join(System.lineSeparator(), Files.readAllLines(filePath));
	} catch (IOException e) {
	    ReportManager.log(e);
	}
	return text;
    }

    public static String readFromFile(String pathToTargetFile) {
	String text = "";
	String absoluteFilePath = getAbsolutePath(pathToTargetFile);
	Path filePath = Paths.get(absoluteFilePath);

	try {
	    text = String.join(System.lineSeparator(), Files.readAllLines(filePath));
	} catch (IOException e) {
	    ReportManager.log(e);
	}
	return text;
    }

    /**
     * Tests whether the file or directory denoted by this abstract pathname exists.
     * 
     * @param fileFolderName  The location of the folder that contains the target
     *                        file, relative to the project's root folder, ending
     *                        with a /
     * @param fileName        The name of the target file (including its extension
     *                        if any)
     * @param numberOfRetries number of times to try to find the file, given that
     *                        each retry is separated by a 500 millisecond wait time
     * @return true if the file exists, false if it doesn't
     */
    public static boolean doesFileExist(String fileFolderName, String fileName, int numberOfRetries) {
	Boolean doesFileExit = false;
	int i = 0;
	while (i < numberOfRetries) {
	    try {
		doesFileExit = (new File(fileFolderName + fileName)).getAbsoluteFile().exists();
	    } catch (Exception e) {
		ReportManager.log(e);
	    }

	    if (Boolean.FALSE.equals(doesFileExit)) {
		try {
		    Thread.sleep(500);
		} catch (Exception e1) {
		    ReportManager.log(e1);
		}
	    }

	    i++;
	}
	return doesFileExit;
    }

    public static boolean doesFileExist(String targetFile) {
	Boolean doesFileExit = false;
	try {
	    doesFileExit = (new File(targetFile)).getAbsoluteFile().exists();
	} catch (Exception e) {
	    ReportManager.log(e);
	}
	return doesFileExit;
    }

    /**
     * Returns the full (absolute) file/folder path using the project-relative
     * fileFolderName and the fileName
     * 
     * @param fileFolderName The location of the folder that contains the target
     *                       file, relative to the project's root folder, ending
     *                       with a /
     * @param fileName       The name of the target file (including its extension if
     *                       any)
     * @return a string value that represents the full/absolute file/folder path
     */
    public static String getAbsolutePath(String fileFolderName, String fileName) {
	String filePath = "";
	try {
	    filePath = (new File(fileFolderName + fileName)).getAbsolutePath();
	} catch (Exception e) {
	    ReportManager.log(e);
	}
	return filePath;
    }

    public static String getAbsolutePath(String fileFolderName) {
	String filePath = "";
	try {
	    filePath = (new File(fileFolderName)).getAbsolutePath();
	} catch (Exception e) {
	    ReportManager.log(e);
	}
	return filePath;
    }

    public static void copyFolder(String sourceFolderPath, String destinationFolderPath) {
	File sourceFolder = new File(sourceFolderPath);
	File destinationFolder = new File(destinationFolderPath);
	try {
	    FileUtils.copyDirectory(sourceFolder, destinationFolder);
	} catch (IOException e) {
	    ReportManager.log(e);
	}
    }

    public static void deleteFolder(String folderPath) {
	File directory = new File(folderPath);
	try {
	    FileUtils.forceDelete(directory);
	} catch (FileNotFoundException e) {
	    // file is already deleted or was not found
	    ReportManager.log("Folder [" + folderPath + "] was not found, it may have already been deleted.");
	} catch (IOException e) {
	    ReportManager.log(e);
	}
    }

    public static void createFolder(String folderPath) {
	try {
	    FileUtils.forceMkdir(new File(folderPath));
	} catch (IOException e) {
	    ReportManager.log(e);
	}
    }

    public static boolean zipFiles(String srcFolder, String destZipFile) {
	boolean result = false;
	try {
	    zipFolder(srcFolder, destZipFile);
	    result = true;
	} catch (Exception e) {
	    ReportManager.log(e);
	}
	return result;
    }

    public static File unpackArchive(URL url, String destinationFolderPath) {
	File targetDir = new File(destinationFolderPath);
	if (!targetDir.exists()) {
	    targetDir.mkdirs();
	}
	File unpacked = null;
	try {
	    InputStream in = new BufferedInputStream(url.openStream(), 1024);
	    // make sure we get the actual file
	    File zip = File.createTempFile("arc", ".zip", targetDir);
	    OutputStream out = new BufferedOutputStream(new FileOutputStream(zip));
	    copyInputStream(in, out);
	    out.close();
	    unpacked = unpackArchive(zip, targetDir);
	    FileActions.deleteFile(zip.getAbsolutePath());
	} catch (IOException e) {
	    ReportManager.log(e);
	    failAction("unpackArchive", "file: " + url.toString() + " to directory: " + destinationFolderPath);
	}
	return unpacked;
    }

    private static File unpackArchive(File theFile, File targetDir) throws IOException {
	if (!theFile.exists()) {
	    throw new IOException(theFile.getAbsolutePath() + " does not exist");
	}
	if (!buildDirectory(targetDir)) {
	    throw new IOException("Could not create directory: " + targetDir);
	}
	ZipFile zipFile = new ZipFile(theFile);
	for (Enumeration<? extends ZipEntry> entries = zipFile.entries(); entries.hasMoreElements();) {
	    ZipEntry entry = entries.nextElement();
	    File file = new File(targetDir, File.separator + entry.getName());
	    if (!buildDirectory(file.getParentFile())) {
		zipFile.close();
		throw new IOException("Could not create directory: " + file.getParentFile());
	    }
	    if (!entry.isDirectory()) {
		copyInputStream(zipFile.getInputStream(entry), new BufferedOutputStream(new FileOutputStream(file)));
	    } else {
		if (!buildDirectory(file)) {
		    zipFile.close();
		    throw new IOException("Could not create directory: " + file);
		}
	    }
	}
	zipFile.close();
	return theFile;
    }

    private static void copyInputStream(InputStream in, OutputStream out) throws IOException {
	byte[] buffer = new byte[1024];
	int len = in.read(buffer);
	while (len >= 0) {
	    out.write(buffer, 0, len);
	    len = in.read(buffer);
	}
	in.close();
	out.close();
    }

    private static boolean buildDirectory(File file) {
	return file.exists() || file.mkdirs();
    }

    public static URL downloadFile(String targetFileURL, String destinationFilePath) {
	return downloadFile(targetFileURL, destinationFilePath, 0, 0);
    }

    public static URL downloadFile(String targetFileURL, String destinationFilePath, int connectionTimeout,
	    int readTimeout) {
	if (targetFileURL != null && destinationFilePath != null) {
	    // force logging
	    Boolean initialLoggingState = ReportManager.isDiscreteLogging();
	    ReportManager.setDiscreteLogging(false);
	    try {
		ReportManager.log("Downloading a file from this url [" + targetFileURL + "] to this directory ["
			+ destinationFilePath + "], please wait as downloading may take some time...");
		FileUtils.copyURLToFile(new URL(targetFileURL), new File(destinationFilePath), connectionTimeout,
			readTimeout);
		ReportManager.logDiscrete("Downloading completed successfully.");
		return new File(destinationFilePath).toURI().toURL();
	    } catch (IOException e) {
		ReportManager.log(e);
		failAction("downloadFile", "Target File URL: [" + targetFileURL + "], and Destination File Path: ["
			+ destinationFilePath + "]");
		return null;
	    } finally {
		ReportManager.setDiscreteLogging(initialLoggingState);
	    }
	} else {
	    failAction("downloadFile", "Target File URL: [" + targetFileURL + "], and Destination File Path: ["
		    + destinationFilePath + "]");
	    return null;
	}
    }
}
