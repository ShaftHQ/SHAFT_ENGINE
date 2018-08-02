package com.shaftEngine.ioActionLibrary;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

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
	// List<String> supplierNames = Arrays.asList("sup1", "sup2", "sup3");

	public static void writeToFile(String fileFolderName, String fileName, List<String> text) {
		String absoluteFilePath = getAbsoluteFilePath(fileFolderName, fileName);
		Path filePath = Paths.get(absoluteFilePath);

		try {
			byte[] textToBytes = String.join(System.lineSeparator(), text).getBytes();
			Files.write(filePath, textToBytes);
		} catch (IOException e) {
			e.printStackTrace();
			ReportManager.log(e.getMessage());
		}
	}

	public static String readFromFile(String fileFolderName, String fileName) {
		String text = "";
		String absoluteFilePath = getAbsoluteFilePath(fileFolderName, fileName);
		Path filePath = Paths.get(absoluteFilePath);

		try {
			text = String.join(System.lineSeparator(), Files.readAllLines(filePath));
		} catch (IOException e) {
			e.printStackTrace();
			ReportManager.log(e.getMessage());
		}
		return text;
	}

	/**
	 * Returns the full (absolute) file path using the project-relative
	 * fileFolderName and the fileName
	 * 
	 * @param fileFolderName
	 *            The location of the folder that contains the target file, relative
	 *            to the project's root folder, ending with a /
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
