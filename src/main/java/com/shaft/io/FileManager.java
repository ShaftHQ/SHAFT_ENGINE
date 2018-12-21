package com.shaft.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.FileUtils;

public class FileManager {
    private FileManager() {
	throw new IllegalStateException("Utility class");
    }

    private static void copyFile(File sourceFile, File destinationFile) {
	try {
	    FileUtils.copyFile(sourceFile, destinationFile);
	} catch (IOException e) {
	    ReportManager.log(e);
	}
    }

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
     * Deletes a file from the local storage
     * 
     * @param filePath the full (absolute) path of the source file that will be
     *                 deleted
     */
    public static void deleteFile(String filePath) {
	FileUtils.deleteQuietly(new File(filePath));
    }

    public static void writeToFile(String fileFolderName, String fileName, List<String> text) {
	String absoluteFilePath = getAbsolutePath(fileFolderName, fileName);
	Path filePath = Paths.get(absoluteFilePath);

	try {
	    byte[] textToBytes = String.join(System.lineSeparator(), text).getBytes();

	    Path parentDir = filePath.getParent();
	    if (!parentDir.toFile().exists()) {
		Files.createDirectories(parentDir);
	    }
	    Files.write(filePath, textToBytes);
	} catch (IOException e) {
	    ReportManager.log(e);
	}
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

	    if (!doesFileExit) {
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
	File directory = new File(folderPath);
	try {
	    FileUtils.forceMkdir(directory);
	} catch (IOException e) {
	    ReportManager.log(e);
	}
    }

    public static boolean zipFiles(String srcFolder, String destZipFile) {
	boolean result = false;
	try {
	    // System.out.println("Program Start zipping the given files");
	    /*
	     * send to the zip procedure
	     */
	    zipFolder(srcFolder, destZipFile);
	    result = true;
	    // System.out.println("Given files are successfully zipped");
	} catch (Exception e) {
	    ReportManager.log(e);
	    // System.out.println("Some Errors happned during the zip process");
	}
	return result;
    }

    /*
     * zip the folders
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

    /*
     * recursively add files to the zip files
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

    /*
     * add folder to the zip file
     */
    private static void addFolderToZip(String path, String srcFolder, ZipOutputStream zip) throws IOException {
	File folder = new File(srcFolder);

	/*
	 * check the empty folder
	 */
	if (folder.list().length == 0) {
	    // System.out.println(folder.getName());
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
}
