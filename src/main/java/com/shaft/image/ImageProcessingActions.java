package com.shaft.image;

import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.util.Arrays;

import javax.imageio.ImageIO;

import org.testng.Assert;

import com.shaft.io.FileManager;
import com.shaft.io.ReportManager;
import com.shaft.validation.Assertions;
import com.shaft.validation.Verifications;

public class ImageProcessingActions {
    private ImageProcessingActions() {
	throw new IllegalStateException("Utility class");
    }

    public static void compareImageFolders(String refrenceFolderPath, String testFolderPath, double threshhold) {

	try {
	    long fileCounter = 1;

	    File refrenceFolder = new File(refrenceFolderPath);
	    File testFolder = new File(testFolderPath);

	    // cleaning processing folders
	    FileManager.deleteFolder(refrenceFolder.getAbsolutePath() + "/processingDirectory/");
	    FileManager.deleteFolder(testFolder.getAbsolutePath() + "/processingDirectory/");
	    FileManager.deleteFolder(testFolder.getAbsolutePath() + "/failedImagesDirectory/");

	    // preparing objects for files
	    File[] refrenceFiles = refrenceFolder.listFiles();
	    File[] testFiles = testFolder.listFiles();

	    ReportManager.log("Comparing [" + testFiles.length + "] image files from the testFolder ["
		    + testFolder.getPath() + "] against [" + refrenceFiles.length
		    + "] image files from the refrenceFolder [" + testFolder.getPath() + "]");

	    // sorting objects for files by fileName
	    Arrays.sort(refrenceFiles);
	    Arrays.sort(testFiles);

	    // confirming that the number of screenshots match
	    if (refrenceFiles.length == testFiles.length) {
		// copy and rename reference screenshots to a processing directory
		for (File refrenceScreenshot : refrenceFiles) {
		    FileManager.copyFile(refrenceScreenshot.getAbsolutePath(),
			    refrenceScreenshot.getParent() + "/processingDirectory/" + fileCounter);
		    fileCounter++;
		}

		// copy and rename test screenshots to match reference screenshots in a
		// processing directory

		fileCounter = 1;
		for (File testScreenshot : testFiles) {
		    FileManager.copyFile(testScreenshot.getAbsolutePath(),
			    testScreenshot.getParent() + "/processingDirectory/" + fileCounter);
		    fileCounter++;
		}

		// point to the two new processing directories
		File refrenceProcessingFolder = new File(refrenceFolderPath + "/processingDirectory/");
		File testProcessingFolder = new File(testFolderPath + "/processingDirectory/");

		// preparing objects for files
		File[] testProcessingFiles = testProcessingFolder.listFiles();

		// sorting objects for files by fileName
		Arrays.sort(testProcessingFiles);

		// compare images from the test directory against the reference directory
		compareImageFolders(refrenceFiles, testFiles, testProcessingFiles, refrenceProcessingFolder,
			testProcessingFolder, threshhold);

		// cleaning processing folders
		FileManager.deleteFolder(refrenceFolder.getAbsolutePath() + "/processingDirectory/");
		FileManager.deleteFolder(testFolder.getAbsolutePath() + "/processingDirectory/");

	    } else {
		// fail because the number of screenshots don't match
		// refrenceFiles.length == testFiles.length
		ReportManager.log("Number of screenshots  [" + testFiles.length + "] from the test folder ["
			+ testFolderPath + "] do not match the number of screenshots [" + refrenceFiles.length
			+ "] from the reference folder [" + refrenceFolderPath + "].");
		Assert.fail("Number of screenshots  [" + testFiles.length + "] from the test folder [" + testFolderPath
			+ "] do not match the number of screenshots [" + refrenceFiles.length
			+ "] from the reference folder [" + refrenceFolderPath + "].");
	    }

	} catch (NullPointerException | IOException e) {
	    ReportManager.log(e);
	    ReportManager.log("Failed to compare image files ...");
	}
    }

    private static void compareImageFolders(File[] refrenceFiles, File[] testFiles, File[] testProcessingFiles,
	    File refrenceProcessingFolder, File testProcessingFolder, double threshhold) throws IOException {
	int passedImagesCount = 0;
	int failedImagesCount = 0;

	// compare images from the test directory against the reference directory
	for (File screenshot : testProcessingFiles) {
	    float percentage = 0;
	    // take buffer data from both image files //

	    BufferedImage biA = ImageIO.read(screenshot);
	    DataBuffer dbA = biA.getData().getDataBuffer();
	    float sizeA = dbA.getSize();

	    BufferedImage biB = ImageIO.read(new File(
		    refrenceProcessingFolder + FileSystems.getDefault().getSeparator() + screenshot.getName()));
	    DataBuffer dbB = biB.getData().getDataBuffer();
	    float sizeB = dbB.getSize();
	    float count = 0;

	    // compare data-buffer objects //
	    if (sizeA == sizeB) {

		for (int i = 0; i < sizeA; i++) {

		    if (dbA.getElem(i) == dbB.getElem(i)) {
			count = count + 1;
		    }

		}
		percentage = (count * 100) / sizeA;
	    } else {
		ReportManager.log("Both the images are not of same size");
	    }

	    // fetch the related reference screenshot file name using the current file
	    // name/number as index
	    String relatedReferenceFileName = refrenceFiles[Integer.parseInt(screenshot.getName()) - 1].getName();
	    ReportManager.attachAsStep("Reference Screenshot", relatedReferenceFileName, new FileInputStream(new File(
		    refrenceProcessingFolder + FileSystems.getDefault().getSeparator() + screenshot.getName())));

	    String relatedTestFileName = testFiles[Integer.parseInt(screenshot.getName()) - 1].getName();

	    ReportManager.attachAsStep("Test Screenshot", relatedTestFileName, new FileInputStream(screenshot));

	    ReportManager.log("Test Screenshot [" + relatedTestFileName + "] and related Refrence Image ["
		    + relatedReferenceFileName + "] match by [" + percentage + "] percent.");

	    try {
		// add to pass/fail counter depending on assertion result, without logging
		ReportManager.setDiscreetLogging(true);
		Assertions.assertComparativeRelation(threshhold, percentage, ">=", true);
		ReportManager.setDiscreetLogging(false);
		passedImagesCount++;
	    } catch (AssertionError e) {
		ReportManager.setDiscreetLogging(false);
		// copying image to failed images directory
		FileManager.copyFile(screenshot.getAbsolutePath(), testProcessingFolder.getParent()
			+ "/failedImagesDirectory/" + relatedTestFileName + "_testImage");
		FileManager.copyFile(
			refrenceProcessingFolder + FileSystems.getDefault().getSeparator() + screenshot.getName(),
			testProcessingFolder.getParent() + "/failedImagesDirectory/" + relatedTestFileName
				+ "_refrenceImage");
		failedImagesCount++;
	    }

	    Verifications.verifyComparativeRelation(threshhold, percentage, ">=", true);
	}

	ReportManager.log("[" + passedImagesCount + "] images passed, and [" + failedImagesCount
		+ "] images failed the threshold of [" + threshhold + "%] matching.");

    }
}
