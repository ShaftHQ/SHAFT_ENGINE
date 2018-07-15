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

	public static void copyFile(String sourceFilePath, String destinationFilePath) {
		File sourceFile = new File(sourceFilePath);
		File destinationFile = new File(destinationFilePath);
		copyFile(sourceFile, destinationFile);
	}

	public static void deleteFile(String filePath) {
		FileUtils.deleteQuietly(new File(filePath));
	}
}
