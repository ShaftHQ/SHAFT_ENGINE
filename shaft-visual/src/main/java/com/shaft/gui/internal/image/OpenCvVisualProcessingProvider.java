package com.shaft.gui.internal.image;

import com.applitools.eyes.LogHandler;
import com.applitools.eyes.MatchLevel;
import com.applitools.eyes.TestResults;
import com.applitools.eyes.exceptions.DiffsFoundException;
import com.applitools.eyes.images.Eyes;
import com.assertthat.selenium_shutterbug.core.CaptureElement;
import com.assertthat.selenium_shutterbug.core.Shutterbug;
import com.assertthat.selenium_shutterbug.utils.image.UnableToCompareImagesException;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import nu.pattern.OpenCV;
import org.opencv.core.*;
import org.opencv.highgui.HighGui;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.openqa.selenium.By;
import org.openqa.selenium.UnsupportedCommandException;
import org.openqa.selenium.WebDriver;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * OpenCV-backed implementation of SHAFT visual processing supplied by the optional
 * {@code io.github.shafthq:shaft-visual} artifact.
 */
public class OpenCvVisualProcessingProvider implements VisualProcessingProvider {
    private static final int CV_THRESH_OTSU = 8;
    private static final int CV_THRESH_BINARY = 0;
    private static final double[] TEMPLATE_SCALE_CANDIDATES = {
            1.0, 0.5, 0.625, 2.0 / 3.0, 0.75, 0.8, 0.9, 1.1, 1.25, 4.0 / 3.0, 1.5, 1.6, 1.75, 2.0, 2.5, 3.0
    };
    private static final int[] TEMPLATE_MATCH_METHODS = {
            Imgproc.TM_CCOEFF_NORMED, Imgproc.TM_SQDIFF_NORMED, Imgproc.TM_CCORR_NORMED
    };
    private static final double MIN_TEMPLATE_SCREEN_AREA_RATIO = 0.02;
    private static final double MAX_TEMPLATE_SCREEN_AREA_RATIO = 0.50;
    private static final double SIMILAR_MATCH_ACCURACY_TOLERANCE = 0.05;
    private static final double TINY_TEMPLATE_ACCURACY_TOLERANCE = 0.10;
    private static final double CLEAR_MATCH_ACCURACY_MARGIN = 0.03;

    private static Mat preprocess(byte[] image) {
        return preprocess(Imgcodecs.imdecode(new MatOfByte(image), Imgcodecs.IMREAD_COLOR));
    }

    private static Mat preprocess(Mat image) {
        //https://stackoverflow.com/questions/37302098/image-preprocessing-with-opencv-before-doing-character-recognition-tesseract
        Mat imgGray = new Mat();
        Mat imgGaussianBlur = new Mat();
        Mat imgSobel = new Mat();
        Mat imgThreshold = new Mat();

        Imgproc.cvtColor(image, imgGray, Imgproc.COLOR_BGR2GRAY);
        Imgproc.GaussianBlur(imgGray, imgGaussianBlur, new Size(3, 3), 0);
        Imgproc.Sobel(imgGaussianBlur, imgSobel, -1, 1, 0);
        Imgproc.threshold(imgSobel, imgThreshold, 0, 255, CV_THRESH_OTSU + CV_THRESH_BINARY);

        if (SHAFT.Properties.reporting.debugMode()) {
            FileActions.getInstance(true).createFolder("target/openCV/temp/");
            String timestamp = String.valueOf(System.currentTimeMillis());
            Imgcodecs.imwrite("target/openCV/temp/" + timestamp + "_1_True_Image.png", image);
            Imgcodecs.imwrite("target/openCV/temp/" + timestamp + "_2_imgGray.png", imgGray);
            Imgcodecs.imwrite("target/openCV/temp/" + timestamp + "_3_imgGaussianBlur.png", imgGaussianBlur);
            Imgcodecs.imwrite("target/openCV/temp/" + timestamp + "_4_imgSobel.png", imgSobel);
            Imgcodecs.imwrite("target/openCV/temp/" + timestamp + "_5_imgThreshold.png", imgThreshold);
        }
        return imgThreshold;
    }

    /**
     * Decodes an image file from disk via an in-memory byte buffer instead of {@link Imgcodecs#imread(String)}.
     * {@code imread} opens the path through the native OS file APIs, which silently fail (returning an empty
     * {@link Mat}, not an exception) for absolute paths beyond the Windows {@code MAX_PATH} (260 character) limit —
     * a realistic scenario for the AI-aided element identification folder nested under a long project/build path.
     * Reading the bytes with Java NIO and decoding them with {@link Imgcodecs#imdecode} sidesteps that native path
     * handling entirely, mirroring how the current-page screenshot bytes are already decoded above.
     */
    private static Mat readImage(String imagePath) {
        try {
            return Imgcodecs.imdecode(new MatOfByte(Files.readAllBytes(Paths.get(imagePath))), Imgcodecs.IMREAD_COLOR);
        } catch (IOException e) {
            return new Mat();
        }
    }

    private static List<Integer> attemptToFindImageUsingOpenCV(String referenceImagePath, byte[] currentPageScreenshot) {
        if (currentPageScreenshot == null || Arrays.equals(currentPageScreenshot, new byte[]{})) {
            //target image is empty, force fail comparison
            ReportManager.log("Failed to identify the element using AI; target screenshot is empty.");
        } else {
            Mat img_original = Imgcodecs.imdecode(new MatOfByte(currentPageScreenshot), Imgcodecs.IMREAD_COLOR);
            Mat templ_original = readImage(referenceImagePath);
            if (img_original.empty() || templ_original.empty()) {
                ReportManager.log("Failed to identify the element using AI; target or reference image is invalid.");
                return Collections.emptyList();
            }

            Mat img = preprocess(currentPageScreenshot);

            // / Do the Matching and Normalize
            try {
                double threshold = SHAFT.Properties.visuals.visualMatchingThreshold();

                TemplateMatch bestMatch = findBestScaledTemplateMatch(img, img_original, templ_original, threshold);
                if (bestMatch == null) {
                    return Collections.emptyList();
                }
                var accuracyMessage = "Match accuracy is " + (int) Math.round(bestMatch.matchAccuracy() * 100)
                        + "% and threshold is " + (int) Math.round(threshold * 100)
                        + "%. Match Method: " + bestMatch.matchMethod()
                        + ". Scale: " + String.format(java.util.Locale.ROOT, "%.3f", bestMatch.scale()) + ".";
                ReportManager.logDiscrete(accuracyMessage);

                if (SHAFT.Properties.reporting.debugMode()) {
                    // debugging
                    try {
                        FileActions.getInstance(true).createFolder("target/openCV/");
                        String timestamp = String.valueOf(System.currentTimeMillis());

                        File output = new File("target/openCV/" + timestamp + "_1_templ.png");
                        ImageIO.write((BufferedImage) HighGui.toBufferedImage(bestMatch.templateOriginal()), "png", output);

                        output = new File("target/openCV/" + timestamp + "_3_img.png");
                        ImageIO.write((BufferedImage) HighGui.toBufferedImage(img_original), "png", output);

                        Imgproc.rectangle(img_original, bestMatch.matchLoc(),
                                new Point(bestMatch.matchLoc().x + bestMatch.templateWidth(), bestMatch.matchLoc().y + bestMatch.templateHeight()),
                                new Scalar(0, 0, 0), 2, 8, 0);
                        output = new File("target/openCV/" + timestamp + "_5_output.png");
                        ImageIO.write((BufferedImage) HighGui.toBufferedImage(img_original), "png", output);
                    } catch (IOException e) {
                        ReportManagerHelper.logDiscrete(e);
                        return Collections.emptyList();
                    }
                }

                if (bestMatch.matchAccuracy() < threshold) {
                    return Collections.emptyList();
                }

                // Return the center of the matched reference so coordinate actions land inside the target.
                int x = (int) Math.round(bestMatch.matchLoc().x + bestMatch.templateWidth() / 2.0);
                int y = (int) Math.round(bestMatch.matchLoc().y + bestMatch.templateHeight() / 2.0);

                // creating highlighted image to be attached to the report
                try {
                    Imgproc.rectangle(img_original, bestMatch.matchLoc(),
                            new Point(bestMatch.matchLoc().x + bestMatch.templateWidth(), bestMatch.matchLoc().y + bestMatch.templateHeight()),
                            new Scalar(67, 176, 42), 2, 8, 0); // selenium-green
                    ByteArrayOutputStream baos = new ByteArrayOutputStream();
                    ImageIO.write((BufferedImage) HighGui.toBufferedImage(img_original), "png", baos);
                    var screenshot = new ScreenshotManager().prepareImageForReport(baos.toByteArray(), "AI identified element");
                    List<List<Object>> attachments = new LinkedList<>();
                    attachments.add(screenshot);
                    ReportManagerHelper.log("Successfully identified the element using AI; OpenCV. " + accuracyMessage, attachments);
                } catch (IOException e) {
                    ReportManager.log("Successfully identified the element using AI; OpenCV. " + accuracyMessage);
                }
                return Arrays.asList(x, y);
            } catch (org.opencv.core.CvException e) {
                ReportManagerHelper.logDiscrete(e);
                ReportManager.log("Failed to identify the element using AI; openCV core exception.");
            }
        }
        return Collections.emptyList();
    }

    private static TemplateMatch findBestScaledTemplateMatch(Mat image, Mat originalImage, Mat referenceOriginal,
                                                             double threshold) {
        TemplateMatch bestMatch = null;
        double imageArea = (double) image.cols() * image.rows();
        Mat grayImage = toGray(originalImage);
        for (int matchMethod : TEMPLATE_MATCH_METHODS) {
            for (double scale : validScales(referenceOriginal, image)) {
                Mat scaledReference = scaleReference(referenceOriginal, scale);
                Mat template = preprocess(scaledReference);
                if (template.cols() > image.cols() || template.rows() > image.rows()) {
                    continue;
                }

                TemplateMatch edgeMatch = locateBestMatch(image, template, matchMethod, scale, scaledReference);
                Mat grayTemplate = toGray(scaledReference);
                TemplateMatch grayMatch = locateBestMatch(grayImage, grayTemplate, matchMethod, scale, scaledReference);
                TemplateMatch candidate = selectScaleCandidate(edgeMatch, grayMatch);
                if (isBetterMatch(candidate, bestMatch, threshold, imageArea)) {
                    bestMatch = candidate;
                }
            }
        }
        return bestMatch;
    }

    private static TemplateMatch locateBestMatch(Mat image, Mat template, int matchMethod, double scale, Mat templateOriginal) {
        Mat result = new Mat(image.rows() - template.rows() + 1, image.cols() - template.cols() + 1, CvType.CV_32FC1);
        Imgproc.matchTemplate(image, template, result, matchMethod);
        Core.MinMaxLocResult mmr = Core.minMaxLoc(result);

        Point matchLoc;
        double matchAccuracy;
        if (matchMethod == Imgproc.TM_SQDIFF || matchMethod == Imgproc.TM_SQDIFF_NORMED) {
            matchLoc = mmr.minLoc;
            matchAccuracy = 1 - mmr.minVal;
        } else {
            matchLoc = mmr.maxLoc;
            matchAccuracy = mmr.maxVal;
        }
        return new TemplateMatch(matchLoc, matchAccuracy, matchMethod, scale, templateOriginal, template.cols(), template.rows());
    }

    private static TemplateMatch selectScaleCandidate(TemplateMatch edgeMatch, TemplateMatch grayMatch) {
        if (isSimilarMatchLocation(edgeMatch, grayMatch) && grayMatch.matchAccuracy() > edgeMatch.matchAccuracy()) {
            return grayMatch;
        }
        return edgeMatch;
    }

    private static boolean isSimilarMatchLocation(TemplateMatch first, TemplateMatch second) {
        double maxDistance = Math.max(3, Math.min(first.templateWidth(), first.templateHeight()) * 0.25);
        double firstCenterX = first.matchLoc().x + first.templateWidth() / 2.0;
        double firstCenterY = first.matchLoc().y + first.templateHeight() / 2.0;
        double secondCenterX = second.matchLoc().x + second.templateWidth() / 2.0;
        double secondCenterY = second.matchLoc().y + second.templateHeight() / 2.0;
        return Math.abs(firstCenterX - secondCenterX) <= maxDistance
                && Math.abs(firstCenterY - secondCenterY) <= maxDistance;
    }

    private static boolean isBetterMatch(TemplateMatch candidate, TemplateMatch currentBest, double threshold, double imageArea) {
        if (currentBest == null) {
            return true;
        }
        boolean candidatePassed = candidate.matchAccuracy() >= threshold;
        boolean currentBestPassed = currentBest.matchAccuracy() >= threshold;
        if (candidatePassed && currentBestPassed) {
            double candidateAreaRatio = areaRatio(candidate, imageArea);
            double currentBestAreaRatio = areaRatio(currentBest, imageArea);
            if (candidateAreaRatio > MAX_TEMPLATE_SCREEN_AREA_RATIO && currentBestAreaRatio <= MAX_TEMPLATE_SCREEN_AREA_RATIO
                    && candidate.matchAccuracy() <= currentBest.matchAccuracy() + SIMILAR_MATCH_ACCURACY_TOLERANCE) {
                return false;
            }
            if (currentBestAreaRatio > MAX_TEMPLATE_SCREEN_AREA_RATIO && candidateAreaRatio <= MAX_TEMPLATE_SCREEN_AREA_RATIO
                    && candidate.matchAccuracy() + SIMILAR_MATCH_ACCURACY_TOLERANCE >= currentBest.matchAccuracy()) {
                return true;
            }
            if (currentBestAreaRatio < MIN_TEMPLATE_SCREEN_AREA_RATIO && candidateAreaRatio >= MIN_TEMPLATE_SCREEN_AREA_RATIO
                    && candidate.matchAccuracy() + TINY_TEMPLATE_ACCURACY_TOLERANCE >= currentBest.matchAccuracy()) {
                return true;
            }
            if (candidateAreaRatio < MIN_TEMPLATE_SCREEN_AREA_RATIO && currentBestAreaRatio >= MIN_TEMPLATE_SCREEN_AREA_RATIO
                    && currentBest.matchAccuracy() + TINY_TEMPLATE_ACCURACY_TOLERANCE >= candidate.matchAccuracy()) {
                return false;
            }
            if (candidate.matchAccuracy() > currentBest.matchAccuracy() + CLEAR_MATCH_ACCURACY_MARGIN) {
                return true;
            }
            if (currentBest.matchAccuracy() > candidate.matchAccuracy() + CLEAR_MATCH_ACCURACY_MARGIN) {
                return false;
            }
            int candidateArea = candidate.templateWidth() * candidate.templateHeight();
            int currentBestArea = currentBest.templateWidth() * currentBest.templateHeight();
            return candidateArea > currentBestArea
                    || (candidateArea == currentBestArea && candidate.matchAccuracy() > currentBest.matchAccuracy());
        }
        if (candidatePassed != currentBestPassed) {
            return candidatePassed;
        }
        return candidate.matchAccuracy() > currentBest.matchAccuracy();
    }

    private static double areaRatio(TemplateMatch match, double imageArea) {
        return ((double) match.templateWidth() * match.templateHeight()) / imageArea;
    }

    private static Mat toGray(Mat image) {
        Mat gray = new Mat();
        Imgproc.cvtColor(image, gray, Imgproc.COLOR_BGR2GRAY);
        return gray;
    }

    private static List<Double> validScales(Mat referenceOriginal, Mat image) {
        List<Double> scales = new ArrayList<>();
        List<String> sizes = new ArrayList<>();
        for (double scale : TEMPLATE_SCALE_CANDIDATES) {
            int width = (int) Math.round(referenceOriginal.cols() * scale);
            int height = (int) Math.round(referenceOriginal.rows() * scale);
            String size = width + "x" + height;
            if (width > 0 && height > 0 && width <= image.cols() && height <= image.rows() && !sizes.contains(size)) {
                scales.add(scale);
                sizes.add(size);
            }
        }
        return scales;
    }

    private static Mat scaleReference(Mat referenceOriginal, double scale) {
        if (Double.compare(scale, 1.0) == 0) {
            return referenceOriginal;
        }
        Mat resized = new Mat();
        Imgproc.resize(referenceOriginal, resized,
                new Size(Math.round(referenceOriginal.cols() * scale), Math.round(referenceOriginal.rows() * scale)),
                0, 0, scale < 1.0 ? Imgproc.INTER_AREA : Imgproc.INTER_CUBIC);
        return resized;
    }

    private record TemplateMatch(Point matchLoc, double matchAccuracy, int matchMethod, double scale, Mat templateOriginal,
                                 int templateWidth, int templateHeight) {
    }

    @Override
    public List<Integer> findImageWithinCurrentPage(String referenceImagePath, byte[] currentPageScreenshot) {
        try {
            return attemptToFindImageUsingOpenCV(referenceImagePath, currentPageScreenshot);
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            return Collections.emptyList();
        }
    }

    @Override
    public Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot,
                                          ImageProcessingActions.VisualValidationEngine visualValidationEngine,
                                          String referenceImagePath, String differencesImagePath) {
        if (visualValidationEngine == ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG) {
            return compareUsingShutterbug(driver, elementLocator, elementScreenshot, referenceImagePath, differencesImagePath);
        }
        if (visualValidationEngine == ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV) {
            return compareUsingOpenCv(elementScreenshot, referenceImagePath);
        }
        return compareUsingEyes(elementScreenshot, visualValidationEngine, referenceImagePath);
    }

    @Override
    public Boolean compareAgainstBaseline(String elementLocatorName, byte[] elementScreenshot,
                                          ImageProcessingActions.VisualValidationEngine visualValidationEngine,
                                          String referenceImagePath, String differencesImagePath) {
        if (visualValidationEngine == ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG) {
            ReportManager.logDiscrete("Playwright visual validation cannot use Selenium Shutterbug; using EXACT_OPENCV for screenshot bytes.");
            return compareUsingOpenCv(elementScreenshot, referenceImagePath);
        }
        if (visualValidationEngine == ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV) {
            return compareUsingOpenCv(elementScreenshot, referenceImagePath);
        }
        return compareUsingEyes(elementScreenshot, visualValidationEngine, referenceImagePath);
    }

    private boolean compareUsingShutterbug(WebDriver driver, By elementLocator, byte[] elementScreenshot,
                                           String referenceImagePath, String differencesImagePath) {
        if (Files.exists(Paths.get(referenceImagePath)) && elementScreenshot != null && elementScreenshot.length > 0) {
            try {
                var snapshot = Shutterbug.shootElement(driver, elementLocator, CaptureElement.VIEWPORT, true);
                return snapshot.equalsWithDiff(referenceImagePath, differencesImagePath, 0.1);
            } catch (IOException e) {
                ReportManagerHelper.logDiscrete(e);
                return false;
            } catch (UnableToCompareImagesException | UnsupportedCommandException exception) {
                ReportManager.logDiscrete("Failed to locate element using \""
                        + ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG
                        + "\", attempting to use \""
                        + ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV + "\".");
                return compareUsingOpenCv(elementScreenshot, referenceImagePath);
            }
        }
        saveReferenceImage(referenceImagePath, elementScreenshot);
        return true;
    }

    private boolean compareUsingOpenCv(byte[] elementScreenshot, String referenceImagePath) {
        if (!Files.exists(Paths.get(referenceImagePath))) {
            saveReferenceImage(referenceImagePath, elementScreenshot);
            return true;
        }
        return !findImageWithinCurrentPage(referenceImagePath, elementScreenshot).isEmpty();
    }

    private boolean compareUsingEyes(byte[] elementScreenshot,
                                     ImageProcessingActions.VisualValidationEngine visualValidationEngine,
                                     String referenceImagePath) {
        Eyes eyes = new Eyes();
        eyes.setLogHandler(new LogHandler() {
            @Override
            public void open() {
            }

            @Override
            public void onMessage(boolean verbose, String message) {
                ReportManager.logDiscrete(message);
            }

            @Override
            public void close() {
            }
        });
        eyes.setApiKey(SHAFT.Properties.paths.applitoolsApiKey());
        MatchLevel targetMatchLevel = switch (visualValidationEngine) {
            case EXACT_EYES -> MatchLevel.EXACT;
            case CONTENT_EYES -> MatchLevel.CONTENT;
            case LAYOUT_EYES -> MatchLevel.LAYOUT;
            default -> MatchLevel.STRICT;
        };
        eyes.setMatchLevel(targetMatchLevel);
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
            eyes.open("shaft-engine", ReportManagerHelper.getCallingMethodFullName());
            eyes.checkImage(elementScreenshot, Paths.get(referenceImagePath).getFileName().toString().replace(".png", ""));
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

    private void saveReferenceImage(String referenceImagePath, byte[] elementScreenshot) {
        ReportManager.logDiscrete("Passing the test and saving a reference image");
        FileActions.getInstance(true).writeToFile(referenceImagePath, elementScreenshot);
    }

    @Override
    public ScreenshotComparisonResult compareScreenshotAgainstBaseline(byte[] baselineImage, byte[] actualImage,
                                                                        List<int[]> maskRects, Integer maxDiffPixels,
                                                                        Double maxDiffPixelRatio) {
        Mat baseline = Imgcodecs.imdecode(new MatOfByte(baselineImage), Imgcodecs.IMREAD_COLOR);
        Mat actual = Imgcodecs.imdecode(new MatOfByte(actualImage), Imgcodecs.IMREAD_COLOR);
        if (baseline.empty() || actual.empty()) {
            return new ScreenshotComparisonResult(false, new byte[0], Long.MAX_VALUE, 1.0);
        }
        if (baseline.rows() != actual.rows() || baseline.cols() != actual.cols()) {
            long totalPixels = (long) baseline.rows() * baseline.cols();
            return new ScreenshotComparisonResult(false, encodePng(actual), totalPixels, 1.0);
        }

        applyMask(baseline, maskRects);
        applyMask(actual, maskRects);

        Mat diff = new Mat();
        Core.absdiff(baseline, actual, diff);
        Mat grayDiff = new Mat();
        Imgproc.cvtColor(diff, grayDiff, Imgproc.COLOR_BGR2GRAY);
        Mat thresholded = new Mat();
        Imgproc.threshold(grayDiff, thresholded, 30, 255, Imgproc.THRESH_BINARY);

        long diffPixels = Core.countNonZero(thresholded);
        long totalPixels = (long) baseline.rows() * baseline.cols();
        double diffRatio = totalPixels == 0 ? 0.0 : (double) diffPixels / totalPixels;

        long allowedDiffPixels;
        if (maxDiffPixels != null) {
            allowedDiffPixels = maxDiffPixels;
        } else if (maxDiffPixelRatio != null) {
            allowedDiffPixels = Math.round(maxDiffPixelRatio * totalPixels);
        } else {
            allowedDiffPixels = 0;
        }
        boolean matched = diffPixels <= allowedDiffPixels;

        byte[] diffImageBytes = new byte[0];
        if (!matched) {
            Mat highlighted = actual.clone();
            highlighted.setTo(new Scalar(67, 176, 42), thresholded); // selenium-green highlight on differing pixels
            diffImageBytes = encodePng(highlighted);
        }

        return new ScreenshotComparisonResult(matched, diffImageBytes, diffPixels, diffRatio);
    }

    private static void applyMask(Mat image, List<int[]> maskRects) {
        if (maskRects == null) {
            return;
        }
        for (int[] rect : maskRects) {
            if (rect == null || rect.length < 4) {
                continue;
            }
            int x = Math.max(0, rect[0]);
            int y = Math.max(0, rect[1]);
            int width = Math.min(rect[2], image.cols() - x);
            int height = Math.min(rect[3], image.rows() - y);
            if (width <= 0 || height <= 0) {
                continue;
            }
            Imgproc.rectangle(image, new Point(x, y), new Point(x + width, y + height), new Scalar(0, 0, 0), -1);
        }
    }

    private static byte[] encodePng(Mat mat) {
        MatOfByte buffer = new MatOfByte();
        Imgcodecs.imencode(".png", mat, buffer);
        return buffer.toArray();
    }

    @Override
    public void load() {
        var libName = "";
        try {
            //https://github.com/openpnp/opencv#api
            libName = org.opencv.core.Core.NATIVE_LIBRARY_NAME;
            OpenCV.loadLocally();
            ReportManager.logDiscrete("Loaded OpenCV \"" + libName + "\".");
        } catch (Throwable throwable) {
            ReportManagerHelper.logDiscrete(throwable);
            if (!libName.isEmpty()) {
                ReportManager.logDiscrete("Failed to load OpenCV \"" + libName + "\". Try installing the binaries manually https://opencv.org/releases/, switching element highlighting to JavaScript...");
            } else {
                ReportManager.logDiscrete("Failed to load OpenCV. Try installing the binaries manually https://opencv.org/releases/, switching element highlighting to JavaScript...");
            }
            SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod("JavaScript");
        }
    }

}
