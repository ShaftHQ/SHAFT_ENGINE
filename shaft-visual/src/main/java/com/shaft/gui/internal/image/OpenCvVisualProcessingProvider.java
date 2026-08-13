package com.shaft.gui.internal.image;

import io.qameta.allure.model.Status;
import com.shaft.tools.io.internal.CheckpointStatus;
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
import com.shaft.gui.image.ImageMatch;
import com.shaft.gui.image.ImageMatchingAlgorithm;
import com.shaft.gui.image.ImageRectangle;
import com.shaft.gui.image.ImageTarget;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import nu.pattern.OpenCV;
import org.opencv.core.*;
import org.opencv.highgui.HighGui;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.opencv.calib3d.Calib3d;
import org.opencv.features2d.DescriptorMatcher;
import org.opencv.features2d.SIFT;
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
import java.util.Comparator;
import java.util.Locale;
import java.util.Map;

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
            ReportManager.log("Failed to identify the element using AI; target screenshot is empty.", Status.FAILED);
        } else {
            Mat img_original = Imgcodecs.imdecode(new MatOfByte(currentPageScreenshot), Imgcodecs.IMREAD_COLOR);
            Mat templ_original = readImage(referenceImagePath);
            if (img_original.empty() || templ_original.empty()) {
                ReportManager.log("Failed to identify the element using AI; target or reference image is invalid.", Status.FAILED);
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
                    ReportManagerHelper.log("Successfully identified the element using AI; OpenCV. " + accuracyMessage, attachments, CheckpointStatus.PASS);
                } catch (Exception e) {
                    // Report-attachment failures (encoding/highlighting issues, etc.) must never downgrade
                    // an already-successful template match into a "not found" verdict; log and continue.
                    ReportManagerHelper.logDiscrete(e);
                    ReportManager.log("Successfully identified the element using AI; OpenCV. " + accuracyMessage
                            + " Failed to attach the highlighted match image to the report.", Status.PASSED);
                }
                return Arrays.asList(x, y);
            } catch (org.opencv.core.CvException e) {
                ReportManagerHelper.logDiscrete(e);
                ReportManager.log("Failed to identify the element using AI; openCV core exception.", Status.FAILED);
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
        if (elementScreenshot == null || elementScreenshot.length == 0) {
            return false;
        }
        byte[] referenceBytes;
        try {
            referenceBytes = Files.readAllBytes(Paths.get(referenceImagePath));
        } catch (IOException exception) {
            ReportManagerHelper.logDiscrete(exception);
            return false;
        }
        Mat reference = null;
        Mat actual = null;
        try {
            reference = decodeImage(referenceBytes, Imgcodecs.IMREAD_UNCHANGED);
            actual = decodeImage(elementScreenshot, Imgcodecs.IMREAD_UNCHANGED);
            return !reference.empty()
                    && !actual.empty()
                    && reference.rows() == actual.rows()
                    && reference.cols() == actual.cols()
                    && reference.type() == actual.type()
                    && Core.norm(reference, actual, Core.NORM_INF) == 0;
        } finally {
            if (reference != null) {
                reference.release();
            }
            if (actual != null) {
                actual.release();
            }
        }
    }

    @Override
    public List<ImageMatch> findImageMatches(ImageTarget target, byte[] currentPageScreenshot) {
        if (target == null || currentPageScreenshot == null || currentPageScreenshot.length == 0) {
            return List.of();
        }
        Mat screenshot = null;
        Mat reference = null;
        Mat searchHeader = null;
        Mat searchImage = null;
        try {
            screenshot = decodeImage(currentPageScreenshot, Imgcodecs.IMREAD_COLOR);
            reference = decodeImage(target.imageBytes(), Imgcodecs.IMREAD_UNCHANGED);
            if (screenshot.empty() || reference.empty()) {
                return List.of();
            }

            ImageRectangle region = target.searchRegion().orElse(
                    new ImageRectangle(0, 0, screenshot.cols(), screenshot.rows()));
            if ((long) region.x() + region.width() > screenshot.cols()
                    || (long) region.y() + region.height() > screenshot.rows()) {
                throw new IllegalArgumentException("Image search region lies outside the screenshot bounds.");
            }
            searchHeader = screenshot.submat(region.y(), region.y() + region.height(),
                    region.x(), region.x() + region.width());
            searchImage = searchHeader.clone();
            double threshold = target.minimumConfidence().orElseGet(
                    () -> SHAFT.Properties.visuals.visualMatchingThreshold());
            List<RichTemplateMatch> candidates = switch (target.matchingMode()) {
                case TEMPLATE -> findTemplateMatches(searchImage, reference, threshold);
                case FEATURE -> findFeatureMatches(searchImage, reference, threshold);
                case AUTO -> {
                    List<RichTemplateMatch> templateMatches = findTemplateMatches(searchImage, reference, threshold);
                    yield templateMatches.isEmpty() ? findFeatureMatches(searchImage, reference, threshold) : templateMatches;
                }
            };
            if (target.occurrence().orElse(0) > 0 && candidates.stream()
                    .anyMatch(match -> match.algorithm() == ImageMatchingAlgorithm.FEATURE_HOMOGRAPHY)) {
                throw new UnsupportedOperationException(
                        "Feature matching currently resolves one geometrically verified occurrence; use TEMPLATE or narrow the region.");
            }
            return suppressOverlappingMatches(candidates).stream()
                    .sorted(Comparator.comparingInt((RichTemplateMatch match) -> match.bounds().y())
                            .thenComparingInt(match -> match.bounds().x()))
                    .map(match -> new ImageMatch(
                            new ImageRectangle(match.bounds().x() + region.x(), match.bounds().y() + region.y(),
                                    match.bounds().width(), match.bounds().height()),
                            Math.max(0, Math.min(1, match.confidence())), match.scale(), match.algorithm(),
                            Map.of("matcher", "opencv", "mode", target.matchingMode().name().toLowerCase(Locale.ROOT))))
                    .toList();
        } finally {
            if (searchImage != null) searchImage.release();
            if (searchHeader != null) searchHeader.release();
            if (screenshot != null) screenshot.release();
            if (reference != null) reference.release();
        }
    }

    private static List<RichTemplateMatch> findTemplateMatches(Mat searchImage, Mat reference, double threshold) {
        Mat graySearch = toGray(searchImage);
        List<RichTemplateMatch> matches = new ArrayList<>();
        try {
            for (double scale : validScales(reference, searchImage)) {
                Mat scaledReference = scaleReference(reference, scale);
                Mat matchReference = new Mat();
                Mat alphaMask = new Mat();
                try {
                    if (scaledReference.channels() == 4) {
                        Imgproc.cvtColor(scaledReference, matchReference, Imgproc.COLOR_BGRA2BGR);
                        Core.extractChannel(scaledReference, alphaMask, 3);
                        Imgproc.threshold(alphaMask, alphaMask, 0, 255, Imgproc.THRESH_BINARY);
                    } else if (scaledReference.channels() == 3) {
                        scaledReference.copyTo(matchReference);
                    } else {
                        scaledReference.copyTo(matchReference);
                    }
                    Mat compatibleSearch = matchReference.channels() == searchImage.channels() ? searchImage : graySearch;
                    if (matchReference.cols() > compatibleSearch.cols() || matchReference.rows() > compatibleSearch.rows()) {
                        continue;
                    }
                    collectTemplateMatches(compatibleSearch, matchReference, alphaMask, threshold, scale, matches);
                } finally {
                    alphaMask.release();
                    matchReference.release();
                    if (scaledReference != reference) {
                        scaledReference.release();
                    }
                }
            }
            return matches;
        } finally {
            graySearch.release();
        }
    }

    private static void collectTemplateMatches(Mat image, Mat template, Mat alphaMask, double threshold, double scale,
                                               List<RichTemplateMatch> matches) {
        Mat result = new Mat();
        MatOfDouble mean = new MatOfDouble();
        MatOfDouble standardDeviation = new MatOfDouble();
        try {
            boolean hasAlphaMask = !alphaMask.empty() && Core.countNonZero(alphaMask) < alphaMask.total();
            Core.meanStdDev(template, mean, standardDeviation);
            boolean flatTemplate = Arrays.stream(standardDeviation.toArray()).allMatch(value -> value < 0.5);
            int method = hasAlphaMask ? Imgproc.TM_CCORR_NORMED
                    : flatTemplate ? Imgproc.TM_SQDIFF : Imgproc.TM_CCOEFF_NORMED;
            if (hasAlphaMask) {
                Imgproc.matchTemplate(image, template, result, method, alphaMask);
            } else {
                Imgproc.matchTemplate(image, template, result, method);
            }
            for (int matchCount = 0; matchCount < 100; matchCount++) {
                Core.MinMaxLocResult extrema = Core.minMaxLoc(result);
                boolean squaredDifference = method == Imgproc.TM_SQDIFF;
                double rawScore = squaredDifference ? extrema.minVal : extrema.maxVal;
                double maximumSquaredError = (double) template.total() * template.channels() * 255 * 255;
                double confidence = squaredDifference ? 1 - rawScore / maximumSquaredError : rawScore;
                Point location = squaredDifference ? extrema.minLoc : extrema.maxLoc;
                if (!Double.isFinite(confidence) || confidence < threshold) {
                    break;
                }
                int x = (int) Math.round(location.x);
                int y = (int) Math.round(location.y);
                matches.add(new RichTemplateMatch(new ImageRectangle(x, y, template.cols(), template.rows()),
                        confidence, scale, template.channels() > 1 ? ImageMatchingAlgorithm.TEMPLATE_COLOR
                        : ImageMatchingAlgorithm.TEMPLATE_GRAYSCALE));

                int suppressionLeft = Math.max(0, x - template.cols() / 2);
                int suppressionTop = Math.max(0, y - template.rows() / 2);
                int suppressionRight = Math.min(result.cols() - 1, x + template.cols() / 2);
                int suppressionBottom = Math.min(result.rows() - 1, y + template.rows() / 2);
                Imgproc.rectangle(result, new Point(suppressionLeft, suppressionTop),
                        new Point(suppressionRight, suppressionBottom),
                        new Scalar(squaredDifference ? maximumSquaredError + 1 : -1), -1);
            }
        } finally {
            standardDeviation.release();
            mean.release();
            result.release();
        }
    }

    private static List<RichTemplateMatch> suppressOverlappingMatches(List<RichTemplateMatch> candidates) {
        List<RichTemplateMatch> retained = new ArrayList<>();
        candidates.stream()
                .sorted(Comparator.comparingDouble(RichTemplateMatch::confidence).reversed()
                        .thenComparingDouble(match -> Math.abs(1.0 - match.scale())))
                .forEach(candidate -> {
                    boolean overlaps = retained.stream().anyMatch(existing -> {
                        ImageRectangle candidateBounds = candidate.bounds();
                        ImageRectangle existingBounds = existing.bounds();
                        long intersection = intersectionArea(candidateBounds, existingBounds);
                        long smallerArea = Math.min((long) candidateBounds.width() * candidateBounds.height(),
                                (long) existingBounds.width() * existingBounds.height());
                        double centerDistance = Math.hypot(candidateBounds.centerX() - existingBounds.centerX(),
                                candidateBounds.centerY() - existingBounds.centerY());
                        double largerDiagonal = Math.hypot(Math.max(candidateBounds.width(), existingBounds.width()),
                                Math.max(candidateBounds.height(), existingBounds.height()));
                        return intersectionOverUnion(candidateBounds, existingBounds) >= 0.30
                                || (smallerArea > 0 && (double) intersection / smallerArea >= 0.60
                                && centerDistance <= largerDiagonal * 0.30);
                    });
                    if (!overlaps) {
                        retained.add(candidate);
                    }
                });
        return retained;
    }

    private static long intersectionArea(ImageRectangle first, ImageRectangle second) {
        int left = Math.max(first.x(), second.x());
        int top = Math.max(first.y(), second.y());
        int right = Math.min(first.x() + first.width(), second.x() + second.width());
        int bottom = Math.min(first.y() + first.height(), second.y() + second.height());
        return (long) Math.max(0, right - left) * Math.max(0, bottom - top);
    }

    private static List<RichTemplateMatch> findFeatureMatches(Mat searchImage, Mat reference, double threshold) {
        Mat graySearch = new Mat();
        Mat grayReference = new Mat();
        Mat referenceMask = new Mat();
        Mat opaqueReference = null;
        Mat emptyMask = new Mat();
        Mat referenceDescriptors = new Mat();
        Mat searchDescriptors = new Mat();
        MatOfKeyPoint referenceKeyPoints = new MatOfKeyPoint();
        MatOfKeyPoint searchKeyPoints = new MatOfKeyPoint();
        MatOfPoint2f referencePoints = new MatOfPoint2f();
        MatOfPoint2f searchPoints = new MatOfPoint2f();
        Mat inlierMask = new Mat();
        Mat homography = null;
        MatOfPoint2f referenceCorners = new MatOfPoint2f();
        MatOfPoint2f projectedCorners = new MatOfPoint2f();
        SIFT sift = null;
        DescriptorMatcher matcher = null;
        List<MatOfDMatch> nearestMatches = new ArrayList<>();
        try {
            if (searchImage.channels() == 1) searchImage.copyTo(graySearch);
            else Imgproc.cvtColor(searchImage, graySearch, Imgproc.COLOR_BGR2GRAY);
            if (reference.channels() == 4) {
                Core.extractChannel(reference, referenceMask, 3);
                Imgproc.threshold(referenceMask, referenceMask, 0, 255, Imgproc.THRESH_BINARY);
                opaqueReference = new Mat(reference.rows(), reference.cols(), CvType.CV_8UC3,
                        new Scalar(255, 255, 255));
                for (int row = 0; row < reference.rows(); row++) {
                    for (int column = 0; column < reference.cols(); column++) {
                        double[] bgra = reference.get(row, column);
                        double alpha = bgra[3] / 255.0;
                        opaqueReference.put(row, column,
                                bgra[0] * alpha + 255 * (1 - alpha),
                                bgra[1] * alpha + 255 * (1 - alpha),
                                bgra[2] * alpha + 255 * (1 - alpha));
                    }
                }
                Imgproc.cvtColor(opaqueReference, grayReference, Imgproc.COLOR_BGR2GRAY);
            } else if (reference.channels() == 1) reference.copyTo(grayReference);
            else Imgproc.cvtColor(reference, grayReference, Imgproc.COLOR_BGR2GRAY);
            sift = SIFT.create();
            matcher = DescriptorMatcher.create(DescriptorMatcher.FLANNBASED);
            sift.detectAndCompute(grayReference, referenceMask.empty() ? emptyMask : referenceMask,
                    referenceKeyPoints, referenceDescriptors);
            sift.detectAndCompute(graySearch, emptyMask, searchKeyPoints, searchDescriptors);
            if (referenceDescriptors.empty() || searchDescriptors.empty()) return List.of();
            matcher.knnMatch(referenceDescriptors, searchDescriptors, nearestMatches, 2);
            KeyPoint[] referenceKeys = referenceKeyPoints.toArray();
            KeyPoint[] searchKeys = searchKeyPoints.toArray();
            List<Point> source = new ArrayList<>();
            List<Point> destination = new ArrayList<>();
            for (MatOfDMatch nearest : nearestMatches) {
                DMatch[] pair = nearest.toArray();
                if (pair.length >= 2 && pair[0].distance < 0.75f * pair[1].distance) {
                    source.add(referenceKeys[pair[0].queryIdx].pt);
                    destination.add(searchKeys[pair[0].trainIdx].pt);
                }
            }
            if (source.size() < 8) return List.of();
            referencePoints.fromList(source);
            searchPoints.fromList(destination);
            homography = Calib3d.findHomography(referencePoints, searchPoints, Calib3d.RANSAC, 3.0, inlierMask);
            if (homography.empty()) return List.of();
            int inliers = Core.countNonZero(inlierMask);
            double inlierRatio = (double) inliers / source.size();
            double confidence = 0.75 + 0.25 * inlierRatio;
            if (inliers < 6 || inlierRatio < 0.60 || confidence < threshold
                    || !hasDistributedInliers(source, inlierMask, reference.cols(), reference.rows())) return List.of();

            referenceCorners.fromArray(new Point(0, 0), new Point(reference.cols(), 0),
                    new Point(reference.cols(), reference.rows()), new Point(0, reference.rows()));
            Core.perspectiveTransform(referenceCorners, projectedCorners, homography);
            Point[] corners = projectedCorners.toArray();
            if (corners.length != 4 || Arrays.stream(corners).anyMatch(point -> !Double.isFinite(point.x)
                    || !Double.isFinite(point.y) || point.x < 0 || point.y < 0
                    || point.x > searchImage.cols() || point.y > searchImage.rows())) return List.of();
            MatOfPoint polygon = new MatOfPoint(corners);
            try {
                double projectedArea = Math.abs(Imgproc.contourArea(polygon));
                double areaRatio = projectedArea / ((double) reference.cols() * reference.rows());
                if (!Imgproc.isContourConvex(polygon) || projectedArea < 16 || areaRatio < 0.10 || areaRatio > 10
                        || !hasPlausibleEdges(corners, (double) reference.cols() / reference.rows())) return List.of();
            } finally {
                polygon.release();
            }
            double minX = Arrays.stream(corners).mapToDouble(point -> point.x).min().orElseThrow();
            double minY = Arrays.stream(corners).mapToDouble(point -> point.y).min().orElseThrow();
            double maxX = Arrays.stream(corners).mapToDouble(point -> point.x).max().orElseThrow();
            double maxY = Arrays.stream(corners).mapToDouble(point -> point.y).max().orElseThrow();
            int x = Math.max(0, (int) Math.floor(minX));
            int y = Math.max(0, (int) Math.floor(minY));
            int width = Math.min(searchImage.cols() - x, Math.max(1, (int) Math.ceil(maxX) - x));
            int height = Math.min(searchImage.rows() - y, Math.max(1, (int) Math.ceil(maxY) - y));
            double scale = Math.sqrt((double) width * height / ((double) reference.cols() * reference.rows()));
            return List.of(new RichTemplateMatch(new ImageRectangle(x, y, width, height), confidence, scale,
                    ImageMatchingAlgorithm.FEATURE_HOMOGRAPHY));
        } finally {
            nearestMatches.forEach(Mat::release);
            safelyClear(matcher);
            safelyClear(sift);
            projectedCorners.release();
            referenceCorners.release();
            if (homography != null) homography.release();
            inlierMask.release();
            searchPoints.release();
            referencePoints.release();
            searchKeyPoints.release();
            referenceKeyPoints.release();
            searchDescriptors.release();
            referenceDescriptors.release();
            emptyMask.release();
            if (opaqueReference != null) opaqueReference.release();
            grayReference.release();
            referenceMask.release();
            graySearch.release();
        }
    }

    private static void safelyClear(org.opencv.core.Algorithm algorithm) {
        if (algorithm == null) return;
        try {
            algorithm.clear();
        } catch (Throwable ignored) {
            // Some packaged OpenCV feature algorithms do not implement clear; their native wrapper finalizer owns it.
        }
    }

    private static boolean hasDistributedInliers(List<Point> sourcePoints, Mat inlierMask,
                                                  int referenceWidth, int referenceHeight) {
        List<Point> inliers = new ArrayList<>();
        for (int index = 0; index < sourcePoints.size(); index++) {
            if (inlierMask.get(index, 0)[0] != 0) inliers.add(sourcePoints.get(index));
        }
        if (inliers.isEmpty()) return false;
        double spreadX = inliers.stream().mapToDouble(point -> point.x).max().orElse(0)
                - inliers.stream().mapToDouble(point -> point.x).min().orElse(0);
        double spreadY = inliers.stream().mapToDouble(point -> point.y).max().orElse(0)
                - inliers.stream().mapToDouble(point -> point.y).min().orElse(0);
        return spreadX >= referenceWidth * 0.20 && spreadY >= referenceHeight * 0.20;
    }

    private static boolean hasPlausibleEdges(Point[] corners, double referenceAspectRatio) {
        double top = distance(corners[0], corners[1]);
        double right = distance(corners[1], corners[2]);
        double bottom = distance(corners[2], corners[3]);
        double left = distance(corners[3], corners[0]);
        double shortest = Math.min(Math.min(top, bottom), Math.min(left, right));
        double longest = Math.max(Math.max(top, bottom), Math.max(left, right));
        if (shortest < 4 || longest / shortest > 10) return false;
        double projectedAspectRatio = ((top + bottom) / 2) / ((left + right) / 2);
        double distortion = projectedAspectRatio / referenceAspectRatio;
        return distortion >= 1.0 / 3.0 && distortion <= 3.0;
    }

    private static double distance(Point first, Point second) {
        return Math.hypot(first.x - second.x, first.y - second.y);
    }

    private static double intersectionOverUnion(ImageRectangle first, ImageRectangle second) {
        int left = Math.max(first.x(), second.x());
        int top = Math.max(first.y(), second.y());
        int right = Math.min(first.x() + first.width(), second.x() + second.width());
        int bottom = Math.min(first.y() + first.height(), second.y() + second.height());
        long intersection = (long) Math.max(0, right - left) * Math.max(0, bottom - top);
        long union = (long) first.width() * first.height() + (long) second.width() * second.height() - intersection;
        return union == 0 ? 0 : (double) intersection / union;
    }

    private record RichTemplateMatch(ImageRectangle bounds, double confidence, double scale,
                                     ImageMatchingAlgorithm algorithm) {
    }

    private static Mat decodeImage(byte[] imageBytes, int mode) {
        MatOfByte buffer = new MatOfByte(imageBytes);
        try {
            return Imgcodecs.imdecode(buffer, mode);
        } finally {
            buffer.release();
        }
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
