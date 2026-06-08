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
import org.apache.logging.log4j.Level;
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
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

class LegacyVisualProcessingProvider implements VisualProcessingProvider {
    private static final int CV_THRESH_OTSU = 8;
    private static final int CV_THRESH_BINARY = 0;

    @Override
    public int priority() {
        return -100;
    }

    @Override
    public boolean isAvailable() {
        try {
            Class.forName("org.opencv.core.Core", false, getClass().getClassLoader());
            Class.forName("nu.pattern.OpenCV", false, getClass().getClassLoader());
            return true;
        } catch (ClassNotFoundException | LinkageError error) {
            ReportManagerHelper.logDiscrete(error);
            return false;
        }
    }

    @Override
    public void initialize() {
        loadOpenCV();
    }

    @Override
    public Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot,
                                          ImageProcessingActions.VisualValidationEngine visualValidationEngine) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
        String aiAidedElementIdentificationFolderPath = ImageProcessingActions.getAiFolderPath();

        if (visualValidationEngine == ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG) {
            String referenceImagePath = aiAidedElementIdentificationFolderPath + hashedLocatorName + ".png";
            String resultingImagePath = aiAidedElementIdentificationFolderPath + hashedLocatorName + "_shutterbug";

            if (ImageProcessingActions.getReferenceImage(elementLocator) != null && elementScreenshot != null && elementScreenshot.length > 0) {
                boolean actualResult = false;
                try {
                    var snapshot = Shutterbug.shootElement(driver, elementLocator, CaptureElement.VIEWPORT, true);
                    actualResult = snapshot.equalsWithDiff(referenceImagePath, resultingImagePath, 0.1);
                } catch (IOException e) {
                    ReportManagerHelper.logDiscrete(e);
                } catch (UnableToCompareImagesException | UnsupportedCommandException exception) {
                    ReportManager.logDiscrete("Failed to locate element using \"" + ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG + "\", attempting to use \"" + ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV + "\".");
                    actualResult = compareAgainstBaseline(driver, elementLocator, elementScreenshot,
                            ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV);
                }
                return actualResult;
            } else {
                ReportManager.logDiscrete("Passing the test and saving a reference image");
                FileActions.getInstance(true).writeToFile(aiAidedElementIdentificationFolderPath, hashedLocatorName + ".png", elementScreenshot);
                return true;
            }
        }

        if (visualValidationEngine == ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV) {
            String referenceImagePath = aiAidedElementIdentificationFolderPath + hashedLocatorName + ".png";

            boolean doesReferenceFileExist = FileActions.getInstance(true).doesFileExist(referenceImagePath);
            if (!doesReferenceFileExist || !findImageWithinCurrentPage(referenceImagePath, elementScreenshot).equals(Collections.emptyList())) {
                if (!doesReferenceFileExist) {
                    ReportManager.logDiscrete("Passing the test and saving a reference image");
                    FileActions.getInstance(true).writeToFile(referenceImagePath, elementScreenshot);
                }
                return true;
            } else {
                return false;
            }
        }
        Eyes eyes = new Eyes();
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
            case EXACT_EYES -> targetMatchLevel = MatchLevel.EXACT;
            case CONTENT_EYES -> targetMatchLevel = MatchLevel.CONTENT;
            case LAYOUT_EYES -> targetMatchLevel = MatchLevel.LAYOUT;
            default -> {
            }
        }
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

    private static Mat preprocess(byte[] image) {
        //https://stackoverflow.com/questions/37302098/image-preprocessing-with-opencv-before-doing-character-recognition-tesseract
        Mat img;
        Mat imgGray = new Mat();
        Mat imgGaussianBlur = new Mat();
        Mat imgSobel = new Mat();
        Mat imgThreshold = new Mat();

        img = Imgcodecs.imdecode(new MatOfByte(image), Imgcodecs.IMREAD_COLOR);
        Imgproc.cvtColor(img, imgGray, Imgproc.COLOR_BGR2GRAY);
        Imgproc.GaussianBlur(imgGray, imgGaussianBlur, new Size(3, 3), 0);
        Imgproc.Sobel(imgGaussianBlur, imgSobel, -1, 1, 0);
        Imgproc.threshold(imgSobel, imgThreshold, 0, 255, CV_THRESH_OTSU + CV_THRESH_BINARY);

        if (SHAFT.Properties.reporting.debugMode()) {
            FileActions.getInstance(true).createFolder("target/openCV/temp/");
            String timestamp = String.valueOf(System.currentTimeMillis());
            Imgcodecs.imwrite("target/openCV/temp/" + timestamp + "_1_True_Image.png", img);
            Imgcodecs.imwrite("target/openCV/temp/" + timestamp + "_2_imgGray.png", imgGray);
            Imgcodecs.imwrite("target/openCV/temp/" + timestamp + "_3_imgGaussianBlur.png", imgGaussianBlur);
            Imgcodecs.imwrite("target/openCV/temp/" + timestamp + "_4_imgSobel.png", imgSobel);
            Imgcodecs.imwrite("target/openCV/temp/" + timestamp + "_5_imgThreshold.png", imgThreshold);
        }
        return imgThreshold;
    }

    private static List<Integer> attemptToFindImageUsingOpenCV(String referenceImagePath, byte[] currentPageScreenshot, int attemptNumber) {
        if (currentPageScreenshot == null || Arrays.equals(currentPageScreenshot, new byte[]{})) {
            //target image is empty, force fail comparison
            ReportManager.log("Failed to identify the element using AI; target screenshot is empty.");
        } else {
            Mat img_original = Imgcodecs.imdecode(new MatOfByte(currentPageScreenshot), Imgcodecs.IMREAD_COLOR);
            Mat templ_original = Imgcodecs.imread(referenceImagePath, Imgcodecs.IMREAD_COLOR);

            Mat img = preprocess(currentPageScreenshot);
            Mat templ = preprocess(FileActions.getInstance(true).readFileAsByteArray(referenceImagePath));

            // / Create the result matrix
            int resultCols = img.cols() - templ.cols() + 1;
            int resultRows = img.rows() - templ.rows() + 1;

            Mat result = new Mat(resultRows, resultCols, CvType.CV_32FC1);

            // / Do the Matching and Normalize
            try {
                // matchMethod 1 == Imgproc.TM_SQDIFF_NORMED
                int matchMethod = Imgproc.TM_CCOEFF_NORMED;
                double threshold = SHAFT.Properties.visuals.visualMatchingThreshold();

                switch (attemptNumber) {
                    case 1 -> matchMethod = Imgproc.TM_SQDIFF_NORMED;
                    case 2 -> matchMethod = Imgproc.TM_CCORR_NORMED;
                }

                Imgproc.matchTemplate(img, templ, result, matchMethod);

                // Localizing the best match with minMaxLoc
                Core.MinMaxLocResult mmr = Core.minMaxLoc(result);

                double minMaxVal;
                double matchAccuracy;

                org.opencv.core.Point matchLoc;
                //noinspection ConstantValue
                if (matchMethod == Imgproc.TM_SQDIFF || matchMethod == Imgproc.TM_SQDIFF_NORMED) {
                    matchLoc = mmr.minLoc;
                    minMaxVal = mmr.minVal;
                    matchAccuracy = 1 - minMaxVal;
                } else {
                    matchLoc = mmr.maxLoc;
                    minMaxVal = mmr.maxVal;
                    matchAccuracy = minMaxVal;
                }

                var accuracyMessage = "Match accuracy is " + (int) Math.round(matchAccuracy * 100) + "% and threshold is " + (int) Math.round(threshold * 100) + "%. Match Method: " + matchMethod + ".";
                ReportManager.logDiscrete(accuracyMessage);

                if (SHAFT.Properties.reporting.debugMode()) {
                    // debugging
                    try {
                        FileActions.getInstance(true).createFolder("target/openCV/");
                        String timestamp = String.valueOf(System.currentTimeMillis());

                        File output = new File("target/openCV/" + timestamp + "_1_templ.png");
                        ImageIO.write((BufferedImage) HighGui.toBufferedImage(templ_original), "png", output);

                        output = new File("target/openCV/" + timestamp + "_3_img.png");
                        ImageIO.write((BufferedImage) HighGui.toBufferedImage(img_original), "png", output);

                        Imgproc.rectangle(img_original, matchLoc, new Point(matchLoc.x + templ.cols(), matchLoc.y + templ.rows()),
                                new Scalar(0, 0, 0), 2, 8, 0);
                        output = new File("target/openCV/" + timestamp + "_5_output.png");
                        ImageIO.write((BufferedImage) HighGui.toBufferedImage(img_original), "png", output);
                    } catch (IOException e) {
                        ReportManagerHelper.logDiscrete(e);
                        return Collections.emptyList();
                    }
                }

                if (matchAccuracy < threshold) {
                    return Collections.emptyList();
                }

                // returning the top left corner +1 pixel
                int x = Integer.parseInt(String.valueOf(matchLoc.x + 1).split("\\.")[0]);
                int y = Integer.parseInt(String.valueOf(matchLoc.y + 1).split("\\.")[0]);

                // creating highlighted image to be attached to the report
                try {
                    Imgproc.rectangle(img_original, matchLoc, new Point(matchLoc.x + templ.cols(), matchLoc.y + templ.rows()),
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

    @Override
    public List<Integer> findImageWithinCurrentPage(String referenceImagePath, byte[] currentPageScreenshot) {
        int maxNumberOfAttempts = 3;
        int attempts = 0;
        List<Integer> foundLocation = Collections.emptyList();
        do {
            try {
                foundLocation = attemptToFindImageUsingOpenCV(referenceImagePath, currentPageScreenshot, attempts);
            } catch (Exception e) {
                ReportManagerHelper.logDiscrete(e);
            }
            attempts++;
        } while (Collections.emptyList().equals(foundLocation) && attempts < maxNumberOfAttempts);
        return foundLocation;
    }

    private static void loadOpenCV() {
        var libName = "";
        try {
            libName = org.opencv.core.Core.NATIVE_LIBRARY_NAME;
            OpenCV.loadLocally();
            ReportManager.logDiscrete("Loaded OpenCV \"" + libName + "\".");
        } catch (Throwable throwable) {
            ReportManagerHelper.logDiscrete(throwable);
            if (!libName.isEmpty()) {
                ReportManager.logDiscrete("Failed to load OpenCV \"" + libName + "\". Add `" + VisualProcessingProviders.SHAFT_VISUAL_COORDINATE + "` when visual matching is extracted, or switch element highlighting to JavaScript.");
            } else {
                ReportManager.logDiscrete("Failed to load OpenCV. Add `" + VisualProcessingProviders.SHAFT_VISUAL_COORDINATE + "` when visual matching is extracted, or switch element highlighting to JavaScript.");
            }
            SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod("JavaScript");
        }
    }


}
