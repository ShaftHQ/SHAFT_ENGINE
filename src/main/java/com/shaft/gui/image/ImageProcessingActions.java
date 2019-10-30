package com.shaft.gui.image;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.util.Arrays;
import java.util.List;

import javax.imageio.ImageIO;

import org.opencv.core.Core;
import org.opencv.core.Core.MinMaxLocResult;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.highgui.HighGui;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.openqa.selenium.By;
import org.testng.Assert;

import com.shaft.cli.FileActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Assertions;
import com.shaft.validation.Verifications;

import nu.pattern.OpenCV;

public class ImageProcessingActions {
    private ImageProcessingActions() {
	throw new IllegalStateException("Utility class");
    }

    public static void compareImageFolders(String refrenceFolderPath, String testFolderPath, double threshhold) {
	// TODO: refactor to minimize File IO actions
	try {
	    long fileCounter = 1;

	    File refrenceFolder = new File(refrenceFolderPath);
	    File testFolder = new File(testFolderPath);

	    // cleaning processing folders
	    FileActions.deleteFolder(refrenceFolder.getAbsolutePath() + "/processingDirectory/");
	    FileActions.deleteFolder(testFolder.getAbsolutePath() + "/processingDirectory/");
	    FileActions.deleteFolder(testFolder.getAbsolutePath() + "/failedImagesDirectory/");

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
		    FileActions.copyFile(refrenceScreenshot.getAbsolutePath(),
			    refrenceScreenshot.getParent() + "/processingDirectory/" + fileCounter);
		    fileCounter++;
		}

		// copy and rename test screenshots to match reference screenshots in a
		// processing directory

		fileCounter = 1;
		for (File testScreenshot : testFiles) {
		    FileActions.copyFile(testScreenshot.getAbsolutePath(),
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
		FileActions.deleteFolder(refrenceFolder.getAbsolutePath() + "/processingDirectory/");
		FileActions.deleteFolder(testFolder.getAbsolutePath() + "/processingDirectory/");

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
	// TODO: refactor to minimize File IO actions
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

	    List<Object> referenceScreenshotAttachment = Arrays.asList("Reference Screenshot", relatedReferenceFileName,
		    new FileInputStream(new File(refrenceProcessingFolder + FileSystems.getDefault().getSeparator()
			    + screenshot.getName())));

	    String relatedTestFileName = testFiles[Integer.parseInt(screenshot.getName()) - 1].getName();

	    List<Object> testScreenshotAttachment = Arrays.asList("Test Screenshot", relatedTestFileName,
		    new FileInputStream(screenshot));

	    ReportManager.log(
		    "Test Screenshot [" + relatedTestFileName + "] and related Refrence Image ["
			    + relatedReferenceFileName + "] match by [" + percentage + "] percent.",
		    Arrays.asList(referenceScreenshotAttachment, testScreenshotAttachment));

	    Boolean discreetLoggingState = ReportManager.isDiscreteLogging();
	    try {
		// add to pass/fail counter depending on assertion result, without logging
		ReportManager.setDiscreteLogging(true);
		Assertions.assertComparativeRelation(threshhold, percentage, ">=", true);
		ReportManager.setDiscreteLogging(discreetLoggingState);
		passedImagesCount++;
	    } catch (AssertionError e) {
		ReportManager.setDiscreteLogging(discreetLoggingState);
		// copying image to failed images directory
		FileActions.copyFile(screenshot.getAbsolutePath(), testProcessingFolder.getParent()
			+ "/failedImagesDirectory/" + relatedTestFileName + "_testImage");
		FileActions.copyFile(
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

    public static byte[] highlightElementInScreenshot(byte[] targetScreenshot,
	    org.openqa.selenium.Rectangle elementLocation, java.awt.Color highlightColor) {

	OpenCV.loadLocally();
	Mat img = Imgcodecs.imdecode(new MatOfByte(targetScreenshot), Imgcodecs.IMREAD_COLOR);

	int outlineThickness = 5;

	Point startPoint = new Point((double) elementLocation.getX() - outlineThickness,
		(double) elementLocation.getY() - outlineThickness);
	Point endPoint = new Point((double) elementLocation.getX() + elementLocation.getWidth() + outlineThickness,
		(double) elementLocation.getY() + elementLocation.getHeight() + outlineThickness);

	// BGR color
	Scalar highlightColorScalar = new Scalar(highlightColor.getBlue(), highlightColor.getGreen(),
		highlightColor.getRed());

	// Outline
	Imgproc.rectangle(img, startPoint, endPoint, highlightColorScalar, outlineThickness, 8, 0);

	Image tmpImg = HighGui.toBufferedImage(img);
	BufferedImage image = (BufferedImage) tmpImg;
	ByteArrayOutputStream baos = new ByteArrayOutputStream();
	try {
	    ImageIO.write(image, "jpg", baos);
	} catch (IOException e) {
	    ReportManager.log(e);
	}
	return baos.toByteArray();
    }

    public static List<Integer> findImageWithinCurrentPage(String referenceImagePath, byte[] currentPageScreenshot,
	    int matchMethod) {

	if (FileActions.doesFileExist(referenceImagePath)) {
	    OpenCV.loadLocally();
	    Mat img = Imgcodecs.imdecode(new MatOfByte(currentPageScreenshot), Imgcodecs.IMREAD_COLOR);
	    Mat templ = Imgcodecs.imread(referenceImagePath, Imgcodecs.IMREAD_COLOR);

	    // / Create the result matrix
	    int resultCols = img.cols() - templ.cols() + 1;
	    int resultRows = img.rows() - templ.rows() + 1;

	    Mat result = new Mat(resultRows, resultCols, CvType.CV_32FC1);

	    // / Do the Matching and Normalize
	    try {
		Imgproc.matchTemplate(img, templ, result, matchMethod);
		Core.normalize(result, result, 0, 1, Core.NORM_MINMAX, -1, new Mat());

		// / Localizing the best match with minMaxLoc
		MinMaxLocResult mmr = Core.minMaxLoc(result);

		Point matchLoc;
		if (matchMethod == Imgproc.TM_SQDIFF || matchMethod == Imgproc.TM_SQDIFF_NORMED) {
		    matchLoc = mmr.minLoc;
		} else {
		    matchLoc = mmr.maxLoc;
		}

		if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("debugMode")))) {
		    // debugging
		    Imgproc.rectangle(img, matchLoc, new Point(matchLoc.x + templ.cols(), matchLoc.y + templ.rows()),
			    new Scalar(0, 0, 0), 2, 8, 0);
		    Image tmpImg = HighGui.toBufferedImage(img);
		    BufferedImage image = (BufferedImage) tmpImg;

		    try {
			FileActions.createFolder("target/openCV/");
			File output = new File("target/openCV/" + System.currentTimeMillis() + ".png");
			ImageIO.write(image, "png", output);
		    } catch (IOException e) {
			ReportManager.log(e);
			return Arrays.asList();
		    }
		}

		// returning the top left corner resulted in an issue with round edged text
		// boxes
		// matchLoc.x
		// matchLoc.y

		// returning the center of the element resulted in an issue with another element
		// overlaying it
		// matchLoc.x + templ.cols() / 2
		// matchLoc.y + templ.rows() / 2

		// returning the top left corner point plus 1x and 1y
		int x = Integer.parseInt(String.valueOf(matchLoc.x + 1).split("\\.")[0]);
		int y = Integer.parseInt(String.valueOf(matchLoc.y + 1).split("\\.")[0]);
		return Arrays.asList(x, y);
	    } catch (org.opencv.core.CvException e) {
		ReportManager.log(e);
		ReportManager.log("Failed to identify the element using AI; openCV core exception.");
		return Arrays.asList();
	    }
	} else {
	    // no reference screenshot exists
	    ReportManager.log("Failed to identify the element using AI; No reference element screenshot exists.");
	    return Arrays.asList();
	}
    }

    public static String formatElementLocatorToImagePath(By elementLocator) {
	StackTraceElement[] callingStack = Thread.currentThread().getStackTrace();
	String className = "";
	for (int i = 1; i < callingStack.length; i++) {
	    if (!callingStack[i].getClassName().contains("com.shaft")) {
		className = callingStack[i].getClassName();
		break;
	    }
	}

	String elementFileName = className + "_" + elementLocator.toString();
	return elementFileName.replaceAll("[\\[\\]\\'\\/:]", "").replaceAll("[\\W\\s]", "_").replaceAll("_{2}", "_")
		.replaceAll("_{2}", "_").replaceAll("contains", "_contains").replaceAll("_$", "");
    }

}
