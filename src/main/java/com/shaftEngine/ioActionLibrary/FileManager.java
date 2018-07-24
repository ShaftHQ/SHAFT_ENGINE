package com.shaftEngine.ioActionLibrary;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;

public class FileManager {
	private static void copyFile(File sourceFile, File destinationFile) {
		try {
			FileUtils.copyFile(sourceFile, destinationFile);
		} catch (IOException e) {
			e.printStackTrace();
			ReportManager.log(e.getMessage());
		}
	}

	/**
	 * Copies a file from sourceFilePath to destinationFilePath on the local storage
	 * 
	 * @param sourceFilePath
	 *            the full (absolute) path of the source file that will be copied
	 * @param destinationFilePath
	 *            the full (absolute) path of the desired location and file name for
	 *            the newly created copied file
	 */
	public static void copyFile(String sourceFilePath, String destinationFilePath) {
		File sourceFile = new File(sourceFilePath);
		File destinationFile = new File(destinationFilePath);
		copyFile(sourceFile, destinationFile);
	}

	/**
	 * Deletes a file from the local storage
	 * 
	 * @param filePath
	 *            the full (absolute) path of the source file that will be deleted
	 */
	public static void deleteFile(String filePath) {
		FileUtils.deleteQuietly(new File(filePath));
	}

	/**
	 * Returns the full (absolute) file path using the project-relative
	 * fileFolderName and the fileName
	 * 
	 * @param fileFolderName
	 *            The location of the folder that contains the target file, relative
	 *            to the project's root folder
	 * @param fileName
	 *            The name of the target file (including its extension if any)
	 * @return
	 */
	public static String getAbsoluteFilePath(String fileFolderName, String fileName) {
		String filePath = "";
		try {
			filePath = (new File(fileFolderName + fileName)).getAbsolutePath();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return filePath;
	}
}
