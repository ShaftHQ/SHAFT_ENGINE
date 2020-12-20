package com.shaft.gui.image;

import com.applitools.eyes.LogHandler;
import com.applitools.eyes.MatchLevel;
import com.applitools.eyes.TestResults;
import com.applitools.eyes.exceptions.DiffsFoundException;
import com.applitools.eyes.images.Eyes;
import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionType;
import com.shaft.validation.Assertions.ComparativeRelationType;
import com.shaft.validation.Verifications;
import nu.pattern.OpenCV;
import org.opencv.core.*;
import org.opencv.core.Point;
import org.opencv.core.Core.MinMaxLocResult;
import org.opencv.highgui.HighGui;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

public class ImageProcessingActions {
    private static final String DIRECTORY_PROCESSING = "/processingDirectory/";
    private static final String DIRECTORY_FAILED = "/failedImagesDirectory/";

    private ImageProcessingActions() {
        throw new IllegalStateException("Utility class");
    }

    public static void compareImageFolders(String referenceFolderPath, String testFolderPath, double threshhold) {
        // TODO: refactor to minimize File IO actions
        try {
            long fileCounter = 1;

            File refrenceFolder = new File(referenceFolderPath);
            File testFolder = new File(testFolderPath);

            // cleaning processing folders
            FileActions.deleteFolder(refrenceFolder.getAbsolutePath() + DIRECTORY_PROCESSING);
            FileActions.deleteFolder(testFolder.getAbsolutePath() + DIRECTORY_PROCESSING);
            FileActions.deleteFolder(testFolder.getAbsolutePath() + DIRECTORY_FAILED);

            // preparing objects for files
            File[] referenceFiles = refrenceFolder.listFiles();
            File[] testFiles = testFolder.listFiles();

            ReportManager.log("Comparing [" + Objects.requireNonNull(testFiles).length + "] image files from the testFolder ["
                    + testFolder.getPath() + "] against [" + Objects.requireNonNull(referenceFiles).length
                    + "] image files from the referenceFolder [" + testFolder.getPath() + "]");

            // sorting objects for files by fileName
            Arrays.sort(referenceFiles);
            Arrays.sort(testFiles);

            // confirming that the number of screenshots match
            if (referenceFiles.length == testFiles.length) {
                // copy and rename reference screenshots to a processing directory
                for (File refrenceScreenshot : referenceFiles) {
                    FileActions.copyFile(refrenceScreenshot.getAbsolutePath(),
                            refrenceScreenshot.getParent() + DIRECTORY_PROCESSING + fileCounter);
                    fileCounter++;
                }

                // copy and rename test screenshots to match reference screenshots in a
                // processing directory

                fileCounter = 1;
                for (File testScreenshot : testFiles) {
                    FileActions.copyFile(testScreenshot.getAbsolutePath(),
                            testScreenshot.getParent() + DIRECTORY_PROCESSING + fileCounter);
                    fileCounter++;
                }

                // point to the two new processing directories
                File refrenceProcessingFolder = new File(referenceFolderPath + DIRECTORY_PROCESSING);
                File testProcessingFolder = new File(testFolderPath + DIRECTORY_PROCESSING);

                // preparing objects for files
                File[] testProcessingFiles = testProcessingFolder.listFiles();

                // sorting objects for files by fileName
                if (testProcessingFiles != null) {
                    Arrays.sort(testProcessingFiles);
                }

                // compare images from the test directory against the reference directory
                compareImageFolders(referenceFiles, testFiles, Objects.requireNonNull(testProcessingFiles), refrenceProcessingFolder,
                        testProcessingFolder, threshhold);

                // cleaning processing folders
                FileActions.deleteFolder(refrenceFolder.getAbsolutePath() + DIRECTORY_PROCESSING);
                FileActions.deleteFolder(testFolder.getAbsolutePath() + DIRECTORY_PROCESSING);

            } else {
                // fail because the number of screenshots don't match
                // referenceFiles.length == testFiles.length
                final String message = "Number of screenshots  [" + testFiles.length + "] from the test folder [" + testFolderPath
                        + "] do not match the number of screenshots [" + referenceFiles.length
                        + "] from the reference folder [" + referenceFolderPath + "].";
                ReportManager.log(message);
                Assert.fail(message);
            }

        } catch (NullPointerException | IOException e) {
            ReportManager.log(e);
            ReportManager.log("Failed to compare image files ...");
        }
    }

    public static byte[] highlightElementInScreenshot(byte[] targetScreenshot,
                                                      org.openqa.selenium.Rectangle elementLocation, Color highlightColor) {

        loadOpenCV();
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
            if (currentPageScreenshot == null || Arrays.equals(currentPageScreenshot, new byte[]{})) {
                //target image is empty, force fail comparison
                ReportManager.log("Failed to identify the element using AI; target screenshot is empty.");
                return Collections.emptyList();
            } else {
                loadOpenCV();
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
                            return Collections.emptyList();
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
                    ReportManager.logDiscrete("Successfully identified the element using AI; OpenCV.");
                    return Arrays.asList(x, y);
                } catch (org.opencv.core.CvException e) {
                    ReportManager.log(e);
                    ReportManager.log("Failed to identify the element using AI; openCV core exception.");
                    return Collections.emptyList();
                }
            }
        } else {
            // no reference screenshot exists
            ReportManager.log("Failed to identify the element using AI; No reference element screenshot exists.");
            return Collections.emptyList();
        }
    }

    @SuppressWarnings("RegExpRedundantEscape")
    public static String formatElementLocatorToImagePath(By elementLocator) {
        String elementFileName = ReportManager.getCallingMethodFullName() + "_" + elementLocator.toString();
        return elementFileName.replaceAll("[\\[\\]\\'\\/:]", "").replaceAll("[\\W\\s]", "_").replaceAll("_{2}", "_")
                .replaceAll("_{2}", "_").replaceAll("contains", "_contains").replaceAll("_$", "");
    }

    public static byte[] getReferenceImage(By elementLocator) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
        String aiFolderPath = ScreenshotManager.getAiAidedElementIdentificationFolderpath();
        String referenceImagePath = aiFolderPath + hashedLocatorName + ".png";
        if (FileActions.doesFileExist(referenceImagePath)) {
            return FileActions.readFromImageFile(referenceImagePath);
        } else {
            return new byte[0];
        }
    }

    public static synchronized Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot, VisualValidationEngine visualValidationEngine) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);

        if (visualValidationEngine == VisualValidationEngine.EXACT_OPENCV) {
            String aiFolderPath = ScreenshotManager.getAiAidedElementIdentificationFolderpath();
            String referenceImagePath = aiFolderPath + hashedLocatorName + ".png";

            boolean doesReferenceFileExist = FileActions.doesFileExist(referenceImagePath);

            if (!Arrays.equals(elementScreenshot, new byte[]{})) {
                if (!doesReferenceFileExist || !ImageProcessingActions.findImageWithinCurrentPage(referenceImagePath, elementScreenshot, Imgproc.TM_CCORR_NORMED).equals(Collections.emptyList())) {
                    //pass: element found and matched || first time element
                    if (!doesReferenceFileExist) {
                        ReportManager.logDiscrete("Passing the test and saving a reference image");
                        FileActions.writeToFile(aiFolderPath, hashedLocatorName + ".png", elementScreenshot);
                    }
                    return true;
                } else {
                    //fail: element doesn't match
                    return false;
                }
            } else {
                //TODO: if element locator was not found, attempt to use AI to find it
                Boolean initialState = ScreenshotManager.getAiSupportedElementIdentification();
                ScreenshotManager.setAiSupportedElementIdentification(true);
                if (!doesReferenceFileExist || ElementActions.attemptToFindElementUsingAI(driver, elementLocator)) {
                    //pass: element found using AI and new locator suggested || first time element
                    if (!doesReferenceFileExist) {
                        ReportManager.logDiscrete("Passing the test and saving a reference image");
                        FileActions.writeToFile(aiFolderPath, hashedLocatorName + ".png", elementScreenshot);
                    }
                    ScreenshotManager.setAiSupportedElementIdentification(initialState);
                    return true;
                } else {
                    //fail: element not found using AI
                    ScreenshotManager.setAiSupportedElementIdentification(initialState);
                    return false;
                }
            }
        }//all the other cases of Eyes
        Eyes eyes = new Eyes();
        // Define global settings
        eyes.setLogHandler(new LogHandler() {
            @Override
            public void open() {
            }

            @Override
            public void onMessage(boolean b, String s) {
                ReportManager.logDiscrete(s);
            }

            @Override
            public void close() {
            }
        });
        eyes.setApiKey(System.getProperty("applitoolsApiKey"));
        MatchLevel targetMatchLevel = MatchLevel.STRICT;
        switch (visualValidationEngine) {
            // https://help.applitools.com/hc/en-us/articles/360007188591-Match-Levels
            case EXACT_EYES:
                targetMatchLevel = MatchLevel.EXACT;
                break;
            case STRICT_EYES:
                targetMatchLevel = MatchLevel.STRICT;
                break;
            case CONTENT_EYES:
                targetMatchLevel = MatchLevel.CONTENT;
                break;
            case LAYOUT_EYES:
                targetMatchLevel = MatchLevel.LAYOUT;
                break;
            default:
                break;
        }
        eyes.setMatchLevel(targetMatchLevel);
        // Define the OS and hosting application to identify the baseline.
        if (BrowserFactory.isMobileNativeExecution()) {
            eyes.setHostOS(System.getProperty("mobile_platformName") + "_" + System.getProperty("mobile_platformVersion"));
            eyes.setHostApp("NativeMobileExecution");
        } else if (BrowserFactory.isMobileWebExecution()) {
            eyes.setHostOS(System.getProperty("mobile_platformName") + "_" + System.getProperty("mobile_platformVersion"));
            eyes.setHostApp(System.getProperty("mobile_browserName"));
        } else {
            eyes.setHostOS(System.getProperty("targetOperatingSystem"));
            eyes.setHostApp(System.getProperty("targetBrowserName"));
        }
        try {
            eyes.open("SHAFT_Engine", ReportManager.getCallingMethodFullName());
            eyes.checkImage(elementScreenshot, hashedLocatorName);
            TestResults eyesValidationResult = eyes.close();
            ReportManager.logDiscrete("Successfully validated the element using AI; Applitools Eyes.");
            return eyesValidationResult.isNew() || eyesValidationResult.isPassed();
        } catch (DiffsFoundException e) {
            ReportManager.log(e);
            return false;
        } finally {
            eyes.abortIfNotClosed();
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
                    new FileInputStream(refrenceProcessingFolder + FileSystems.getDefault().getSeparator()
                            + screenshot.getName()));

            String relatedTestFileName = testFiles[Integer.parseInt(screenshot.getName()) - 1].getName();

            List<Object> testScreenshotAttachment = Arrays.asList("Test Screenshot", relatedTestFileName,
                    new FileInputStream(screenshot));

            ReportManager.log(
                    "Test Screenshot [" + relatedTestFileName + "] and related Refrence Image ["
                            + relatedReferenceFileName + "] match by [" + percentage + "] percent.",
                    Arrays.asList(referenceScreenshotAttachment, testScreenshotAttachment));

            boolean discreetLoggingState = ReportManager.isDiscreteLogging();
            try {
                // add to pass/fail counter depending on assertion result, without logging
                ReportManager.setDiscreteLogging(true);
                Assertions.assertComparativeRelation(threshhold, percentage,
                        ComparativeRelationType.GREATER_THAN_OR_EQUALS, AssertionType.POSITIVE);
                ReportManager.setDiscreteLogging(discreetLoggingState);
                passedImagesCount++;
            } catch (AssertionError e) {
                ReportManager.setDiscreteLogging(discreetLoggingState);
                // copying image to failed images directory
                FileActions.copyFile(screenshot.getAbsolutePath(),
                        testProcessingFolder.getParent() + DIRECTORY_FAILED + relatedTestFileName + "_testImage");
                FileActions.copyFile(
                        refrenceProcessingFolder + FileSystems.getDefault().getSeparator() + screenshot.getName(),
                        testProcessingFolder.getParent() + DIRECTORY_FAILED + relatedTestFileName + "_refrenceImage");
                failedImagesCount++;
            }

            Verifications.verifyComparativeRelation(threshhold, percentage, Verifications.ComparativeRelationType.GREATER_THAN_OR_EQUALS, Verifications.VerificationType.POSITIVE);
        }

        ReportManager.log("[" + passedImagesCount + "] images passed, and [" + failedImagesCount
                + "] images failed the threshold of [" + threshhold + "%] matching.");

    }

    private static void loadOpenCV() {
        try {
            OpenCV.loadShared();
            ReportManager.logDiscrete("Loaded Shared OpenCV");
        } catch (NoClassDefFoundError | RuntimeException | ExceptionInInitializerError e) {
            try {
                OpenCV.loadLocally();
                ReportManager.logDiscrete("Loaded Local OpenCV");
            } catch (UnsatisfiedLinkError e2) {
                ReportManager.log(e);
                ReportManager.logDiscrete("Failed to load OpenCV");
            }
        }
    }

    public enum VisualValidationEngine {
        EXACT_OPENCV,
        EXACT_EYES,
        STRICT_EYES,
        CONTENT_EYES,
        LAYOUT_EYES
    }
}
