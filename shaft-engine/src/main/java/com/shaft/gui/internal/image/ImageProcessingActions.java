package com.shaft.gui.internal.image;

import com.shaft.tools.io.internal.CheckpointStatus;
import io.qameta.allure.model.Status;
import com.google.common.hash.Hashing;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
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
import java.nio.file.Files;
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
        try {
            File referenceFolder = new File(referenceFolderPath);
            File testFolder = new File(testFolderPath);

            // cleaning legacy processing folders and previous failures
            FileActions.getInstance(true).deleteFolder(referenceFolder.getAbsolutePath() + DIRECTORY_PROCESSING);
            FileActions.getInstance(true).deleteFolder(testFolder.getAbsolutePath() + DIRECTORY_PROCESSING);
            FileActions.getInstance(true).deleteFolder(testFolder.getAbsolutePath() + DIRECTORY_FAILED);

            // preparing objects for files
            File[] referenceFiles = referenceFolder.listFiles();
            File[] testFiles = testFolder.listFiles();

            ReportManager.log("Comparing \"" + Objects.requireNonNull(testFiles).length + "\" image files from the testFolder \""
                    + testFolder.getPath() + "\" against \"" + Objects.requireNonNull(referenceFiles).length
                    + "\" image files from the referenceFolder \"" + referenceFolder.getPath() + "\".");

            // sorting objects for files by fileName
            Arrays.sort(referenceFiles);
            Arrays.sort(testFiles);

            // confirming that the number of screenshots match
            if (referenceFiles.length == testFiles.length) {
                // compare images from the test directory against the reference directory
                compareImageFolders(referenceFiles, testFiles, testFolder, threshold);

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

        // JPEG cannot encode an alpha channel: ImageIO.write(.., "jpg", ..) silently returns false
        // and produces an empty byte[] for an alpha-bearing image. Native mobile-app screenshots
        // (PNG) decode with alpha, so the highlighted screenshot would come back empty and be
        // dropped from the report. Flatten onto an opaque RGB raster before drawing/encoding.
        if (image.getColorModel().hasAlpha()) {
            BufferedImage opaqueImage = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_INT_RGB);
            Graphics2D flatten = opaqueImage.createGraphics();
            try {
                flatten.drawImage(image, 0, 0, null);
            } finally {
                flatten.dispose();
            }
            image = opaqueImage;
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

    /**
     * Formats a Selenium locator into the stable visual-baseline file-name hash used by SHAFT.
     *
     * @param elementLocator locator of the element being compared
     * @return stable hashed baseline file name without extension
     */
    public static String formatElementLocatorToImagePath(By elementLocator) {
        return formatElementLocatorToImagePath(JavaHelper.formatLocatorToString(elementLocator), String.valueOf(elementLocator));
    }

    /**
     * Formats a backend-neutral locator description into the stable visual-baseline file-name hash used by SHAFT.
     *
     * @param elementLocatorName stable element locator description
     * @return stable hashed baseline file name without extension
     */
    public static String formatElementLocatorToImagePath(String elementLocatorName) {
        return formatElementLocatorToImagePath(Objects.requireNonNull(elementLocatorName, "elementLocatorName"), elementLocatorName);
    }

    private static String formatElementLocatorToImagePath(String locatorKey, String locatorLogText) {
        String elementFileName = ReportManagerHelper.getCallingClassFullName() + "_" + locatorKey;
        return locatorHashMapping.computeIfAbsent(elementFileName, key -> {
            String hashedFileName = key.replaceAll("[\\[\\]\\'\\/:]", "").replaceAll("[\\W\\s]", "_").replaceAll("_{2}", "_")
                    .replaceAll("_{2}", "_").replaceAll("contains", "_contains").replaceAll("_$", "");
            // https://github.com/ShaftHQ/SHAFT_ENGINE/issues/1604
            hashedFileName = Hashing.sha256().hashString(key, StandardCharsets.UTF_8).toString();
            ReportManager.logDiscrete("Element Locator \"" + locatorLogText + "\" was formatted to \"" + key + "\".", Level.DEBUG);
            return hashedFileName;
        });
    }

    /**
     * Reads the existing reference image for a Selenium locator, when present.
     *
     * @param elementLocator locator of the element being compared
     * @return encoded reference image bytes, or {@code null} when no baseline exists
     */
    public static byte[] getReferenceImage(By elementLocator) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
        return getReferenceImageByHashedLocatorName(hashedLocatorName);
    }

    /**
     * Reads the existing reference image for a backend-neutral locator, when present.
     *
     * @param elementLocatorName stable element locator description
     * @return encoded reference image bytes, or {@code null} when no baseline exists
     */
    public static byte[] getReferenceImage(String elementLocatorName) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocatorName);
        return getReferenceImageByHashedLocatorName(hashedLocatorName);
    }

    private static byte[] getReferenceImageByHashedLocatorName(String hashedLocatorName) {
        String referenceImagePath = resolveExistingBaselinePath(hashedLocatorName, false);
        if (FileActions.getInstance(true).doesFileExist(referenceImagePath)) {
            return FileActions.getInstance(true).readFileAsByteArray(referenceImagePath);
        } else {
            return null;
        }
    }

    public static byte[] getShutterbugDifferencesImage(By elementLocator) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
        return getShutterbugDifferencesImageByHashedLocatorName(hashedLocatorName);
    }

    /**
     * Reads a generated Shutterbug differences image for a backend-neutral locator, when present.
     *
     * @param elementLocatorName stable element locator description
     * @return encoded differences image bytes, or an empty byte array when no differences image exists
     */
    public static byte[] getShutterbugDifferencesImage(String elementLocatorName) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocatorName);
        return getShutterbugDifferencesImageByHashedLocatorName(hashedLocatorName);
    }

    private static byte[] getShutterbugDifferencesImageByHashedLocatorName(String hashedLocatorName) {
        String referenceImagePath = getAiFolderPath() + hashedLocatorName + "_shutterbug.png";
        if (FileActions.getInstance(true).doesFileExist(referenceImagePath)) {
            return FileActions.getInstance(true).readFileAsByteArray(referenceImagePath);
        } else {
            return new byte[0];
        }
    }

    public static Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot, VisualValidationEngine visualValidationEngine) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
        String visualBaselinePath = getAiFolderPath() + hashedLocatorName;
        return VisualProcessingProviderRegistry.requireProvider().compareAgainstBaseline(
                driver, elementLocator, elementScreenshot, visualValidationEngine,
                visualBaselinePath + ".png", visualBaselinePath + "_shutterbug");
    }

    /**
     * Compares backend-neutral screenshot bytes against the visual baseline for the supplied locator description.
     *
     * @param elementLocatorName stable element locator description
     * @param elementScreenshot encoded screenshot bytes
     * @param visualValidationEngine requested visual validation engine
     * @return {@code true} when the comparison passes or creates a new baseline
     */
    public static Boolean compareAgainstBaseline(String elementLocatorName, byte[] elementScreenshot, VisualValidationEngine visualValidationEngine) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocatorName);
        String visualBaselinePath = getAiFolderPath() + hashedLocatorName;
        return VisualProcessingProviderRegistry.requireProvider().compareAgainstBaseline(
                elementLocatorName, elementScreenshot, visualValidationEngine,
                visualBaselinePath + ".png", visualBaselinePath + "_shutterbug");
    }

    /**
     * Compares an element's actual screenshot against its visual-regression baseline using a pixel-level diff.
     * Saves the actual screenshot as the new baseline when none exists yet, or when {@code -Dshaft.updateSnapshots=true}.
     *
     * @param elementLocator locator of the element being compared
     * @param actualScreenshot encoded screenshot bytes of the current state
     * @param maskRects pixel-space {@code [x, y, width, height]} regions to exclude from comparison, or {@code null}
     * @param maxDiffPixels maximum allowed differing pixel count, or {@code null} to ignore this budget
     * @param maxDiffPixelRatio maximum allowed differing pixel ratio (0.0-1.0), or {@code null} to ignore this budget
     * @return the comparison result
     */
    public static VisualProcessingProvider.ScreenshotComparisonResult compareScreenshotAgainstBaseline(
            By elementLocator, byte[] actualScreenshot, List<int[]> maskRects, Integer maxDiffPixels, Double maxDiffPixelRatio) {
        return compareScreenshotAgainstBaselineByHash(formatElementLocatorToImagePath(elementLocator), actualScreenshot, maskRects, maxDiffPixels, maxDiffPixelRatio);
    }

    /**
     * Compares a backend-neutral actual screenshot (e.g. a full-page capture) against its visual-regression
     * baseline using a pixel-level diff. Saves the actual screenshot as the new baseline when none exists yet,
     * or when {@code -Dshaft.updateSnapshots=true}.
     *
     * @param baselineKey stable backend-neutral baseline description
     * @param actualScreenshot encoded screenshot bytes of the current state
     * @param maskRects pixel-space {@code [x, y, width, height]} regions to exclude from comparison, or {@code null}
     * @param maxDiffPixels maximum allowed differing pixel count, or {@code null} to ignore this budget
     * @param maxDiffPixelRatio maximum allowed differing pixel ratio (0.0-1.0), or {@code null} to ignore this budget
     * @return the comparison result
     */
    public static VisualProcessingProvider.ScreenshotComparisonResult compareScreenshotAgainstBaseline(
            String baselineKey, byte[] actualScreenshot, List<int[]> maskRects, Integer maxDiffPixels, Double maxDiffPixelRatio) {
        return compareScreenshotAgainstBaselineByHash(formatElementLocatorToImagePath(baselineKey), actualScreenshot, maskRects, maxDiffPixels, maxDiffPixelRatio);
    }

    private static VisualProcessingProvider.ScreenshotComparisonResult compareScreenshotAgainstBaselineByHash(
            String hashedName, byte[] actualScreenshot, List<int[]> maskRects, Integer maxDiffPixels, Double maxDiffPixelRatio) {
        String newBaselineImagePath = getAiFolderPath() + hashedName + browserPlatformSuffix() + ".png";
        boolean updateSnapshots = SHAFT.Properties.visuals.updateSnapshots();
        String baselineImagePath = resolveExistingBaselinePath(hashedName, true);

        if (!FileActions.getInstance(true).doesFileExist(baselineImagePath) || updateSnapshots) {
            ReportManager.logDiscrete("Passing the test and saving a reference screenshot baseline.");
            FileActions.getInstance(true).writeToFile(newBaselineImagePath, actualScreenshot);
            return new VisualProcessingProvider.ScreenshotComparisonResult(true, new byte[0], 0, 0.0);
        }

        byte[] baselineImage = FileActions.getInstance(true).readFileAsByteArray(baselineImagePath);
        VisualProcessingProvider.ScreenshotComparisonResult result = VisualProcessingProviderRegistry.requireProvider()
                .compareScreenshotAgainstBaseline(baselineImage, actualScreenshot, maskRects, maxDiffPixels, maxDiffPixelRatio);

        if (!result.matched() && result.diffImage() != null && result.diffImage().length > 0) {
            FileActions.getInstance(true).writeToFile(getAiFolderPath() + hashedName + browserPlatformSuffix() + "_diff.png", result.diffImage());
        }
        return result;
    }

    /**
     * Resolves the on-disk path of an existing {@code matchesScreenshot} baseline for the given hashed name,
     * preferring the per-browser/OS naming scheme and falling back to the legacy (pre-per-browser/OS) baseline
     * when only that one exists. When neither exists, returns the new-scheme path so callers create baselines there.
     *
     * @param hashedName    stable hashed baseline file name without extension or browser/OS suffix
     * @param logFallback   whether to emit a one-line notice when falling back to a legacy baseline
     * @return the resolved baseline path to read from (or to treat as "missing" when creating a new baseline)
     */
    private static String resolveExistingBaselinePath(String hashedName, boolean logFallback) {
        String newBaselineImagePath = getAiFolderPath() + hashedName + browserPlatformSuffix() + ".png";
        if (FileActions.getInstance(true).doesFileExist(newBaselineImagePath)) {
            return newBaselineImagePath;
        }
        String legacyBaselineImagePath = getAiFolderPath() + hashedName + ".png";
        if (FileActions.getInstance(true).doesFileExist(legacyBaselineImagePath)) {
            if (logFallback) {
                ReportManager.logDiscrete("No per-browser/OS baseline found at \"" + newBaselineImagePath
                        + "\"; falling back to legacy baseline \"" + legacyBaselineImagePath + "\".");
            }
            return legacyBaselineImagePath;
        }
        return newBaselineImagePath;
    }

    /**
     * Builds the sanitized {@code _<browser>_<platform>} suffix appended to {@code matchesScreenshot} baseline
     * file names so that cross-browser/OS runs no longer share (and fight over) a single baseline image.
     *
     * @return the sanitized browser/platform suffix, e.g. {@code "_chrome_windows"}
     */
    private static String browserPlatformSuffix() {
        String browser = sanitizeForBaselineFileName(SHAFT.Properties.web.targetBrowserName());
        String platform = sanitizeForBaselineFileName(SHAFT.Properties.platform.targetPlatform());
        return "_" + browser + "_" + platform;
    }

    /**
     * Lowercases and strips non-alphanumeric characters so browser/platform names are safe to embed in file names.
     *
     * @param value raw browser or platform name
     * @return sanitized, file-system-safe value (possibly empty)
     */
    private static String sanitizeForBaselineFileName(String value) {
        if (value == null) {
            return "";
        }
        return value.toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9]", "");
    }

    private static void compareImageFolders(File[] referenceFiles, File[] testFiles, File testFolder, double threshold) throws IOException {
        int passedImagesCount = 0;
        int failedImagesCount = 0;

        // compare images from the test directory against the reference directory
        for (int index = 0; index < testFiles.length; index++) {
            File testFile = testFiles[index];
            File referenceFile = referenceFiles[index];
            float percentage = 0;
            // take buffer data from both image files //

            BufferedImage biA = ImageIO.read(testFile);
            DataBuffer dbA = biA.getData().getDataBuffer();
            float sizeA = dbA.getSize();

            BufferedImage biB = ImageIO.read(referenceFile);
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

            String relatedReferenceFileName = referenceFile.getName();
            String relatedTestFileName = testFile.getName();

            try (InputStream refIn = Files.newInputStream(referenceFile.toPath());
                 InputStream testIn = Files.newInputStream(testFile.toPath())) {
                List<Object> referenceScreenshotAttachment = Arrays.asList(
                        "Reference Screenshot", relatedReferenceFileName, refIn);
                List<Object> testScreenshotAttachment = Arrays.asList(
                        "Test Screenshot", relatedTestFileName, testIn);
                ReportManagerHelper.log(
                        "Test Screenshot \"" + relatedTestFileName + "\" and related Reference Image \""
                                + relatedReferenceFileName + "\" match by \"" + percentage + "\" percent.",
                        Arrays.asList(referenceScreenshotAttachment, testScreenshotAttachment),
                        percentage >= threshold ? CheckpointStatus.PASS : CheckpointStatus.FAIL);
            } catch (IOException ioEx) {
                ReportManagerHelper.logDiscrete(ioEx);
                continue;
            }

            if (percentage >= threshold) {
                passedImagesCount++;
            } else {
                // copying image to failed images directory
                FileActions.getInstance(true).copyFile(testFile.getAbsolutePath(),
                        testFolder.getAbsolutePath() + DIRECTORY_FAILED + relatedTestFileName + "_testImage");
                FileActions.getInstance(true).copyFile(referenceFile.getAbsolutePath(),
                        testFolder.getAbsolutePath() + DIRECTORY_FAILED + relatedTestFileName + "_referenceImage");
                failedImagesCount++;
            }
        }

        ReportManager.log("\"" + passedImagesCount + "\" images passed, and \"" + failedImagesCount
                        + "\" images failed the threshold of \"" + threshold + "%\" matching.",
                failedImagesCount > 0 ? Status.FAILED : Status.PASSED);
        if (failedImagesCount > 0) {
            FailureReporter.failAssertion("\"" + failedImagesCount + "\" images failed the threshold of \""
                    + threshold + "%\" matching.");
        }

    }

    public static void loadOpenCV() {
        VisualProcessingProviderRegistry.requireProvider().load();
    }

    /**
     * Loads the optional visual processing provider when it is available.
     * Engine startup uses this method so installations that do not include
     * {@code shaft-visual} remain quiet until a visual operation is requested.
     */
    public static void loadOpenCVIfAvailable() {
        VisualProcessingProviderRegistry.findProvider().ifPresent(VisualProcessingProvider::load);
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
