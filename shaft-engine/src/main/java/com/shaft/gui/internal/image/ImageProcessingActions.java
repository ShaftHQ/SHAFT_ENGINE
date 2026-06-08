package com.shaft.gui.internal.image;

import com.applitools.eyes.LogHandler;
import com.applitools.eyes.MatchLevel;
import com.applitools.eyes.TestResults;
import com.applitools.eyes.exceptions.DiffsFoundException;
import com.applitools.eyes.images.Eyes;
import com.assertthat.selenium_shutterbug.core.CaptureElement;
import com.assertthat.selenium_shutterbug.core.Shutterbug;
import com.assertthat.selenium_shutterbug.utils.image.UnableToCompareImagesException;
import com.google.common.hash.Hashing;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.Validations;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.openqa.selenium.UnsupportedCommandException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.Browser;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

@SuppressWarnings("SpellCheckingInspection")
public class ImageProcessingActions {
    private static final String DIRECTORY_PROCESSING = "/processingDirectory/";
    private static final String DIRECTORY_FAILED = "/failedImagesDirectory/";
    private static final ThreadLocal<String> aiFolderPath = ThreadLocal.withInitial(() -> "");

    private ImageProcessingActions() {
        throw new IllegalStateException("Utility class");
    }

    public static void compareImageFolders(String referenceFolderPath, String testFolderPath, double threshold) {
        // TODO: refactor to minimize File IO actions
        try {
            long fileCounter = 1;

            File referenceFolder = new File(referenceFolderPath);
            File testFolder = new File(testFolderPath);

            // cleaning processing folders
            FileActions.getInstance(true).deleteFolder(referenceFolder.getAbsolutePath() + DIRECTORY_PROCESSING);
            FileActions.getInstance(true).deleteFolder(testFolder.getAbsolutePath() + DIRECTORY_PROCESSING);
            FileActions.getInstance(true).deleteFolder(testFolder.getAbsolutePath() + DIRECTORY_FAILED);

            // preparing objects for files
            File[] referenceFiles = referenceFolder.listFiles();
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
                for (File referenceScreenshot : referenceFiles) {
                    FileActions.getInstance(true).copyFile(referenceScreenshot.getAbsolutePath(),
                            referenceScreenshot.getParent() + DIRECTORY_PROCESSING + fileCounter);
                    fileCounter++;
                }

                // copy and rename test screenshots to match reference screenshots in a
                // processing directory

                fileCounter = 1;
                for (File testScreenshot : testFiles) {
                    FileActions.getInstance(true).copyFile(testScreenshot.getAbsolutePath(),
                            testScreenshot.getParent() + DIRECTORY_PROCESSING + fileCounter);
                    fileCounter++;
                }

                // point to the two new processing directories
                File referenceProcessingFolder = new File(referenceFolderPath + DIRECTORY_PROCESSING);
                File testProcessingFolder = new File(testFolderPath + DIRECTORY_PROCESSING);

                // preparing objects for files
                File[] testProcessingFiles = testProcessingFolder.listFiles();

                // sorting objects for files by fileName
                if (testProcessingFiles != null) {
                    Arrays.sort(testProcessingFiles);
                }

                // compare images from the test directory against the reference directory
                compareImageFolders(referenceFiles, testFiles, Objects.requireNonNull(testProcessingFiles), referenceProcessingFolder,
                        testProcessingFolder, threshold);

                // cleaning processing folders
                FileActions.getInstance(true).deleteFolder(referenceFolder.getAbsolutePath() + DIRECTORY_PROCESSING);
                FileActions.getInstance(true).deleteFolder(testFolder.getAbsolutePath() + DIRECTORY_PROCESSING);

            } else {
                // fail because the number of screenshots don't match
                // referenceFiles.length == testFiles.length
                final String message = "Number of screenshots  [" + testFiles.length + "] from the test folder [" + testFolderPath
                        + "] do not match the number of screenshots [" + referenceFiles.length
                        + "] from the reference folder [" + referenceFolderPath + "].";
                FailureReporter.fail(message);
            }

        } catch (NullPointerException | IOException e) {
            FailureReporter.fail(ImageProcessingActions.class, "Failed to compare image files ...", e);
        }
    }

    public static byte[] highlightElementInScreenshot(byte[] targetScreenshot,
                                                      org.openqa.selenium.Rectangle elementLocation, Color highlightColor) {
        BufferedImage image;
        try (var input = new java.io.ByteArrayInputStream(targetScreenshot)) {
            image = ImageIO.read(input);
        } catch (IOException e) {
            throw new IllegalArgumentException("Failed to decode screenshot bytes.", e);
        }
        if (image == null) {
            throw new IllegalArgumentException("Failed to decode screenshot bytes.");
        }

        int outlineThickness = 5;
        double elementHeight = elementLocation.getHeight();
        double elementWidth = elementLocation.getWidth();
        double xPos = elementLocation.getX();
        double yPos = elementLocation.getY();

        if (SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(Platform.IOS.name())
                || SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(Platform.MAC.name())
                || (SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(Platform.LINUX.name())
                && SHAFT.Properties.visuals.screenshotParamsScalingFactor() != 1)) {
            elementHeight *= 2;
            elementWidth *= 2;
            xPos *= 2;
            yPos *= 2;
        }

        if (SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(Platform.IOS.name())
                && SHAFT.Properties.mobile.browserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            yPos += elementHeight + 2 * outlineThickness;
        }
        if (SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(Platform.ANDROID.name())
                && SHAFT.Properties.mobile.appPackage().equalsIgnoreCase("com.android.chrome")) {
            yPos += 2 * outlineThickness;
        }
        if (SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(Platform.MAC.name())) {
            yPos += 2 * outlineThickness;
        }
        if (SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(Platform.WINDOWS.name())) {
            double scalingFactor = SHAFT.Properties.visuals.screenshotParamsScalingFactor();
            elementHeight *= scalingFactor;
            elementWidth *= scalingFactor;
            xPos *= scalingFactor;
            yPos *= scalingFactor;
        }

        Graphics2D graphics = image.createGraphics();
        try {
            graphics.setColor(highlightColor);
            graphics.setStroke(new BasicStroke(outlineThickness));
            graphics.drawRect((int) Math.round(xPos - outlineThickness),
                    (int) Math.round(yPos - outlineThickness),
                    (int) Math.round(elementWidth + 2 * outlineThickness),
                    (int) Math.round(elementHeight + 2 * outlineThickness));
        } finally {
            graphics.dispose();
        }

        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try {
            ImageIO.write(image, "jpg", output);
        } catch (IOException e) {
            ReportManagerHelper.logDiscrete(e);
        }
        return output.toByteArray();
    }

    public static List<Integer> findImageWithinCurrentPage(String referenceImagePath, byte[] currentPageScreenshot) {
        return VisualProcessingProviderRegistry.requireProvider()
                .findImageWithinCurrentPage(referenceImagePath, currentPageScreenshot);
    }

    private static final ConcurrentHashMap<String, String> locatorHashMapping = new ConcurrentHashMap<>();

    public static String formatElementLocatorToImagePath(By elementLocator) {
        String elementFileName = ReportManagerHelper.getCallingClassFullName() + "_" + JavaHelper.formatLocatorToString(elementLocator);
        return locatorHashMapping.computeIfAbsent(elementFileName, key -> {
            String hashedFileName = key.replaceAll("[\\[\\]\\'\\/:]", "").replaceAll("[\\W\\s]", "_").replaceAll("_{2}", "_")
                    .replaceAll("_{2}", "_").replaceAll("contains", "_contains").replaceAll("_$", "");
            // https://github.com/ShaftHQ/SHAFT_ENGINE/issues/1604
            hashedFileName = Hashing.sha256().hashString(key, StandardCharsets.UTF_8).toString();
            ReportManager.log("Element Locator: " + elementLocator + " was formatted to: " + key, Level.INFO);
            return hashedFileName;
        });
    }

    public static byte[] getReferenceImage(By elementLocator) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
        String referenceImagePath = getAiFolderPath() + hashedLocatorName + ".png";
        if (FileActions.getInstance(true).doesFileExist(referenceImagePath)) {
            return FileActions.getInstance(true).readFileAsByteArray(referenceImagePath);
        } else {
            return null;
        }
    }

    public static byte[] getShutterbugDifferencesImage(By elementLocator) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
        String referenceImagePath = getAiFolderPath() + hashedLocatorName + "_shutterbug.png";
        if (FileActions.getInstance(true).doesFileExist(referenceImagePath)) {
            return FileActions.getInstance(true).readFileAsByteArray(referenceImagePath);
        } else {
            return new byte[0];
        }
    }

    public static Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot, VisualValidationEngine visualValidationEngine) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
        String aiAidedElementIdentificationFolderPath = getAiFolderPath();

        if (visualValidationEngine == VisualValidationEngine.EXACT_SHUTTERBUG) {
            String referenceImagePath = aiAidedElementIdentificationFolderPath + hashedLocatorName + ".png";
            String resultingImagePath = aiAidedElementIdentificationFolderPath + hashedLocatorName + "_shutterbug";

            if (getReferenceImage(elementLocator) !=null && elementScreenshot != null && elementScreenshot.length > 0) {
                boolean actualResult = false;
                try {
                    var snapshot = Shutterbug.shootElement(driver, elementLocator, CaptureElement.VIEWPORT, true);
                    actualResult = snapshot.equalsWithDiff(referenceImagePath, resultingImagePath, 0.1);
                } catch (IOException e) {
                    ReportManagerHelper.logDiscrete(e);
                } catch (UnableToCompareImagesException | UnsupportedCommandException exception) {
                    ReportManager.logDiscrete("Failed to locate element using \"" + VisualValidationEngine.EXACT_SHUTTERBUG + "\", attempting to use \"" + VisualValidationEngine.EXACT_OPENCV + "\".");
                    // com.assertthat.selenium_shutterbug.utils.image.UnableToCompareImagesException: Images dimensions mismatch
                    actualResult = compareAgainstBaseline(driver, elementLocator, elementScreenshot, VisualValidationEngine.EXACT_OPENCV);
                }
                return actualResult;
            } else {
                ReportManager.logDiscrete("Passing the test and saving a reference image");
                FileActions.getInstance(true).writeToFile(aiAidedElementIdentificationFolderPath, hashedLocatorName + ".png", elementScreenshot);
                return true;
            }
        }

        if (visualValidationEngine == VisualValidationEngine.EXACT_OPENCV) {
            String referenceImagePath = aiAidedElementIdentificationFolderPath + hashedLocatorName + ".png";

            boolean doesReferenceFileExist = FileActions.getInstance(true).doesFileExist(referenceImagePath);
            if (!doesReferenceFileExist || !ImageProcessingActions.findImageWithinCurrentPage(referenceImagePath, elementScreenshot).equals(Collections.emptyList())) {
                //pass: element found and matched || first time element
                if (!doesReferenceFileExist) {
                    ReportManager.logDiscrete("Passing the test and saving a reference image");
                    FileActions.getInstance(true).writeToFile(referenceImagePath, elementScreenshot);
                }
                return true;
            } else {
                //fail: element doesn't match
                return false;
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
        eyes.setApiKey(SHAFT.Properties.paths.applitoolsApiKey());
        MatchLevel targetMatchLevel = MatchLevel.STRICT;
        switch (visualValidationEngine) {
            // https://help.applitools.com/hc/en-us/articles/360007188591-Match-Levels
            case EXACT_EYES -> targetMatchLevel = MatchLevel.EXACT;
            case CONTENT_EYES -> targetMatchLevel = MatchLevel.CONTENT;
            case LAYOUT_EYES -> targetMatchLevel = MatchLevel.LAYOUT;
            default -> {
            }
        }
        eyes.setMatchLevel(targetMatchLevel);
        // Define the OS and hosting application to identify the baseline.
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            eyes.setHostOS(SHAFT.Properties.mobile.platformName() + "_" + SHAFT.Properties.mobile.platformVersion());
            eyes.setHostApp("NativeMobileExecution");
        } else if (DriverFactoryHelper.isMobileWebExecution()) {
            eyes.setHostOS(SHAFT.Properties.mobile.platformName() + "_" + SHAFT.Properties.mobile.platformVersion());
            eyes.setHostApp(SHAFT.Properties.mobile.browserName());
        } else {
            eyes.setHostOS(SHAFT.Properties.platform.targetPlatform());
            eyes.setHostApp(SHAFT.Properties.web.targetBrowserName());
        }
        try {
            eyes.open("SHAFT_Engine", ReportManagerHelper.getCallingMethodFullName());
            eyes.checkImage(elementScreenshot, hashedLocatorName);
            TestResults eyesValidationResult = eyes.close();
            ReportManager.logDiscrete("Successfully validated the element using AI; Applitools Eyes.");
            return eyesValidationResult.isNew() || eyesValidationResult.isPassed();
        } catch (DiffsFoundException e) {
            ReportManagerHelper.logDiscrete(e);
            return false;
        } finally {
            eyes.abortIfNotClosed();
        }
    }

    private static void compareImageFolders(File[] referenceFiles, File[] testFiles, File[] testProcessingFiles,
                                            File referenceProcessingFolder, File testProcessingFolder, double threshold) throws IOException {
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
                    referenceProcessingFolder + FileSystems.getDefault().getSeparator() + screenshot.getName()));
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
            String relatedReferenceFileName = referenceFiles[Integer.parseInt(screenshot.getName()) - 1].getName();
            String relatedTestFileName = testFiles[Integer.parseInt(screenshot.getName()) - 1].getName();

            Path referenceImagePath = Paths.get(
                    referenceProcessingFolder + FileSystems.getDefault().getSeparator() + screenshot.getName());
            try (InputStream refIn = Files.newInputStream(referenceImagePath);
                 InputStream testIn = Files.newInputStream(screenshot.toPath())) {
                List<Object> referenceScreenshotAttachment = Arrays.asList(
                        "Reference Screenshot", relatedReferenceFileName, refIn);
                List<Object> testScreenshotAttachment = Arrays.asList(
                        "Test Screenshot", relatedTestFileName, testIn);
                ReportManagerHelper.log(
                        "Test Screenshot [" + relatedTestFileName + "] and related Reference Image ["
                                + relatedReferenceFileName + "] match by [" + percentage + "] percent.",
                        Arrays.asList(referenceScreenshotAttachment, testScreenshotAttachment));
            } catch (IOException ioEx) {
                ReportManagerHelper.logDiscrete(ioEx);
                continue;
            }

            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            try {
                // add to pass/fail counter depending on assertion result, without logging
                ReportManagerHelper.setDiscreteLogging(true);
                Validations.assertThat()
                        .number(percentage)
                        .isGreaterThanOrEquals(threshold)
                        .perform();
                ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
                passedImagesCount++;
            } catch (AssertionError e) {
                ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
                // copying image to failed images directory
                FileActions.getInstance(true).copyFile(screenshot.getAbsolutePath(),
                        testProcessingFolder.getParent() + DIRECTORY_FAILED + relatedTestFileName + "_testImage");
                FileActions.getInstance(true).copyFile(
                        referenceProcessingFolder + FileSystems.getDefault().getSeparator() + screenshot.getName(),
                        testProcessingFolder.getParent() + DIRECTORY_FAILED + relatedTestFileName + "_referenceImage");
                failedImagesCount++;
            }

            Validations.verifyThat()
                    .number(percentage)
                    .isGreaterThanOrEquals(threshold)
                    .perform();
        }

        ReportManager.log("[" + passedImagesCount + "] images passed, and [" + failedImagesCount
                + "] images failed the threshold of [" + threshold + "%] matching.");

    }

    public static void loadOpenCV() {
        VisualProcessingProviderRegistry.requireProvider().load();
    }

    private static String getAiFolderPath() {
        String currentAiFolderPath = aiFolderPath.get();
        if (currentAiFolderPath == null || currentAiFolderPath.isEmpty()) {
            currentAiFolderPath = ScreenshotHelper.getAiAidedElementIdentificationFolderPath();
            aiFolderPath.set(currentAiFolderPath);
        }
        return currentAiFolderPath;
    }

    @SuppressWarnings("unused")
    public enum VisualValidationEngine {
        EXACT_SHUTTERBUG,
        EXACT_OPENCV,
        EXACT_EYES,
        STRICT_EYES,
        CONTENT_EYES,
        LAYOUT_EYES
    }
}
