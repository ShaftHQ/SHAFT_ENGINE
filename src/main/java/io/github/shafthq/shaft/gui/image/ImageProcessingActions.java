package io.github.shafthq.shaft.gui.image;

import com.applitools.eyes.LogHandler;
import com.applitools.eyes.MatchLevel;
import com.applitools.eyes.TestResults;
import com.applitools.eyes.exceptions.DiffsFoundException;
import com.applitools.eyes.images.Eyes;
import com.assertthat.selenium_shutterbug.core.CaptureElement;
import com.assertthat.selenium_shutterbug.core.Shutterbug;
import com.assertthat.selenium_shutterbug.utils.image.UnableToCompareImagesException;
import com.shaft.cli.FileActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import io.github.shafthq.shaft.driver.DriverFactoryHelper;
import io.github.shafthq.shaft.tools.io.helpers.FailureReporter;
import io.github.shafthq.shaft.tools.io.helpers.ReportManagerHelper;
import nu.pattern.OpenCV;
import org.opencv.core.Point;
import org.opencv.core.*;
import org.opencv.highgui.HighGui;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.openqa.selenium.By;
import org.openqa.selenium.UnsupportedCommandException;
import org.openqa.selenium.WebDriver;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.util.List;
import java.util.*;

public class ImageProcessingActions {
    private static final String DIRECTORY_PROCESSING = "/processingDirectory/";
    private static final String DIRECTORY_FAILED = "/failedImagesDirectory/";
    private static final int
            CV_MOP_CLOSE = 3,
            CV_THRESH_OTSU = 8,
            CV_THRESH_BINARY = 0,
            CV_ADAPTIVE_THRESH_GAUSSIAN_C = 1,
            CV_ADAPTIVE_THRESH_MEAN_C = 0,
            CV_THRESH_BINARY_INV = 1;

    private static String aiFolderPath = "";

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
            FileActions.getInstance().deleteFolder(refrenceFolder.getAbsolutePath() + DIRECTORY_PROCESSING);
            FileActions.getInstance().deleteFolder(testFolder.getAbsolutePath() + DIRECTORY_PROCESSING);
            FileActions.getInstance().deleteFolder(testFolder.getAbsolutePath() + DIRECTORY_FAILED);

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
                    FileActions.getInstance().copyFile(refrenceScreenshot.getAbsolutePath(),
                            refrenceScreenshot.getParent() + DIRECTORY_PROCESSING + fileCounter);
                    fileCounter++;
                }

                // copy and rename test screenshots to match reference screenshots in a
                // processing directory

                fileCounter = 1;
                for (File testScreenshot : testFiles) {
                    FileActions.getInstance().copyFile(testScreenshot.getAbsolutePath(),
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
                FileActions.getInstance().deleteFolder(refrenceFolder.getAbsolutePath() + DIRECTORY_PROCESSING);
                FileActions.getInstance().deleteFolder(testFolder.getAbsolutePath() + DIRECTORY_PROCESSING);

            } else {
                // fail because the number of screenshots don't match
                // referenceFiles.length == testFiles.length
                final String message = "Number of screenshots  [" + testFiles.length + "] from the test folder [" + testFolderPath
                        + "] do not match the number of screenshots [" + referenceFiles.length
                        + "] from the reference folder [" + referenceFolderPath + "].";
                FailureReporter.fail(message);
            }

        } catch (NullPointerException | IOException e) {
            ReportManagerHelper.logDiscrete(e);
            ReportManager.log("Failed to compare image files ...");
        }
    }

    public static byte[] highlightElementInScreenshot(byte[] targetScreenshot,
                                                      org.openqa.selenium.Rectangle elementLocation, Color highlightColor) {

//        loadOpenCV();
        Mat img = Imgcodecs.imdecode(new MatOfByte(targetScreenshot), Imgcodecs.IMREAD_COLOR);

        int outlineThickness = 5;
        double elementHeight = elementLocation.getHeight(),
                elementWidth = elementLocation.getWidth(),
                xPos = elementLocation.getX(),
                yPos = elementLocation.getY();

        // IOS Native | macOS Browser | Linux Browser scaled | -> Repositioning
        if (System.getProperty("targetOperatingSystem").equals("iOS")
                || System.getProperty("targetOperatingSystem").equals("Mac-64")
                || System.getProperty("targetOperatingSystem").equals("Mac")
                || (
                System.getProperty("targetOperatingSystem").equals("Linux-64")
                        && System.getProperty("screenshotParams_scalingFactor") != null
                        && !System.getProperty("screenshotParams_scalingFactor").isEmpty()
                        && !System.getProperty("screenshotParams_scalingFactor").equals("1")
        )
                || (
                System.getProperty("targetOperatingSystem").equals("Linux")
                        && System.getProperty("screenshotParams_scalingFactor") != null
                        && !System.getProperty("screenshotParams_scalingFactor").isEmpty()
                        && !System.getProperty("screenshotParams_scalingFactor").equals("1")
        )
        ) {
            elementHeight *= 2;
            elementWidth *= 2;
            xPos *= 2;
            yPos *= 2;
        }

        // IOS Browser Repositioning
        if (System.getProperty("targetOperatingSystem").equals("iOS") && System.getProperty("mobile_browserName").equals("Safari")) {
            yPos += elementHeight + 2 * outlineThickness;
        }

        // Android Browser Repositioning
        if (System.getProperty("targetOperatingSystem").equals("Android") && System.getProperty("mobile_appPackage").equals("com.android.chrome")) {
            yPos += 2 * outlineThickness;
        }

        // MacOS Browser Repositioning
        if (System.getProperty("targetOperatingSystem").equals("Mac-64")) {
            yPos += 2 * outlineThickness;
        }

        // Windows Browser Repositioning
        if (System.getProperty("targetOperatingSystem").equals("Windows-64")
                && System.getProperty("screenshotParams_scalingFactor") != null
                && !System.getProperty("screenshotParams_scalingFactor").isEmpty()) {
            double scalingFactor = Double.parseDouble(System.getProperty("screenshotParams_scalingFactor"));
            elementHeight *= scalingFactor;
            elementWidth *= scalingFactor;
            xPos *= scalingFactor;
            yPos *= scalingFactor;
        }

        Point startPoint = new Point(xPos - outlineThickness, yPos - outlineThickness);
        Point endPoint = new Point(xPos + elementWidth + outlineThickness, yPos + elementHeight + outlineThickness);

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
            ReportManagerHelper.logDiscrete(e);
        }
        return baos.toByteArray();
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

        if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("debugMode")))) {
            FileActions.getInstance().createFolder("target/openCV/temp/");
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
            Mat templ = preprocess(FileActions.getInstance().readFileAsByteArray(referenceImagePath));

            // / Create the result matrix
            int resultCols = img.cols() - templ.cols() + 1;
            int resultRows = img.rows() - templ.rows() + 1;

            Mat result = new Mat(resultRows, resultCols, CvType.CV_32FC1);

            // / Do the Matching and Normalize
            try {
                // matchMethod 1 == Imgproc.TM_SQDIFF_NORMED
                int matchMethod = Imgproc.TM_CCOEFF_NORMED;
                double threshold = 0.80;

                switch (attemptNumber) {
                    case 0 -> matchMethod = Imgproc.TM_CCOEFF_NORMED;
                    case 1 -> matchMethod = Imgproc.TM_SQDIFF_NORMED;
                    case 2 -> matchMethod = Imgproc.TM_CCORR_NORMED;
                }

                switch (attemptNumber) {
                    case 0 -> threshold = 0.80;
                    case 1 -> threshold = 0.70;
                    case 2 -> threshold = 0.60;
                }

                Imgproc.matchTemplate(img, templ, result, matchMethod);
//                    Core.normalize(result, result, 0, 1, Core.NORM_MINMAX, -1, new Mat());

                // Localizing the best match with minMaxLoc
                Core.MinMaxLocResult mmr = Core.minMaxLoc(result);

                double minMaxVal;
                double matchAccuracy;

                org.opencv.core.Point matchLoc;
                if (matchMethod == Imgproc.TM_SQDIFF || matchMethod == Imgproc.TM_SQDIFF_NORMED) {
                    matchLoc = mmr.minLoc;
                    minMaxVal = mmr.minVal;
                    matchAccuracy = 1 - minMaxVal;
                } else {
                    matchLoc = mmr.maxLoc;
                    minMaxVal = mmr.maxVal;
                    matchAccuracy = minMaxVal;
                }

                var accuracyMessage = "Match accuracy is " + (int) Math.round(matchAccuracy * 100) + "% and threshold is " + (int) Math.round(threshold * 100) + "%.";
                ReportManager.logDiscrete(accuracyMessage);

                if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("debugMode")))) {
                    // debugging
                    try {
                        FileActions.getInstance().createFolder("target/openCV/");
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

                // returning the top left corner point plus 1x and 1y
                int x = Integer.parseInt(String.valueOf(matchLoc.x + 1).split("\\.")[0]);
                int y = Integer.parseInt(String.valueOf(matchLoc.y + 1).split("\\.")[0]);

                // creating highlighted image to be attached to the report
                try {
                Imgproc.rectangle(img_original, matchLoc, new Point(matchLoc.x + templ.cols(), matchLoc.y + templ.rows()),
                        new Scalar(0, 0, 0), 2, 8, 0);
                ByteArrayOutputStream baos = new ByteArrayOutputStream();
                ImageIO.write((BufferedImage) HighGui.toBufferedImage(img_original), "png", baos);
                var screenshot = ScreenshotManager.prepareImageforReport(baos.toByteArray(), "AI identified element");
                List<List<Object>> attachments = new LinkedList<>();
                attachments.add(screenshot);
                ReportManagerHelper.log("Successfully identified the element using AI; OpenCV. "+accuracyMessage, attachments);
                } catch (IOException e) {
                    ReportManager.log("Successfully identified the element using AI; OpenCV. "+accuracyMessage);
                }
                return Arrays.asList(x, y);
            } catch (org.opencv.core.CvException e) {
                ReportManagerHelper.logDiscrete(e);
                ReportManager.log("Failed to identify the element using AI; openCV core exception.");
            }
        }
        return Collections.emptyList();
    }

    public static List<Integer> findImageWithinCurrentPage(String referenceImagePath, byte[] currentPageScreenshot) {
            int maxNumberOfAttempts = 3;
            int attempts = 0;
            List<Integer> foundLocation = Collections.emptyList();
            do {
                try {
                    foundLocation = attemptToFindImageUsingOpenCV(referenceImagePath, currentPageScreenshot, attempts);
                } catch (Exception e) {
                    //Do Nothing
                }
                attempts++;
            } while (Collections.emptyList().equals(foundLocation) && attempts < maxNumberOfAttempts);
            return foundLocation;
    }

    public static String formatElementLocatorToImagePath(Object elementLocator) {
        String elementFileName = ReportManagerHelper.getCallingMethodFullName() + "_" + elementLocator.toString();
        return elementFileName.replaceAll("[\\[\\]\\'\\/:]", "").replaceAll("[\\W\\s]", "_").replaceAll("_{2}", "_")
                .replaceAll("_{2}", "_").replaceAll("contains", "_contains").replaceAll("_$", "");
    }

    public static byte[] getReferenceImage(Object elementLocator) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
        if (aiFolderPath.isEmpty()) {
            aiFolderPath = ScreenshotManager.getAiAidedElementIdentificationFolderpath();
        }
        String referenceImagePath = aiFolderPath + hashedLocatorName + ".png";
        if (FileActions.getInstance().doesFileExist(referenceImagePath)) {
            return FileActions.getInstance().readFileAsByteArray(referenceImagePath);
        } else {
            return new byte[0];
        }
    }

    public static byte[] getShutterbugDifferencesImage(Object elementLocator) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
        String referenceImagePath = aiFolderPath + hashedLocatorName + "_shutterbug.png";
        if (FileActions.getInstance().doesFileExist(referenceImagePath)) {
            return FileActions.getInstance().readFileAsByteArray(referenceImagePath);
        } else {
            return new byte[0];
        }
    }

    public static Boolean compareAgainstBaseline(WebDriver driver, By elementLocator, byte[] elementScreenshot, VisualValidationEngine visualValidationEngine) {
        String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);

        if (visualValidationEngine == VisualValidationEngine.EXACT_SHUTTERBUG) {
            String referenceImagePath = aiFolderPath + hashedLocatorName + ".png";
            String resultingImagePath = aiFolderPath + hashedLocatorName + "_shutterbug";

            boolean doesReferenceFileExist = FileActions.getInstance().doesFileExist(referenceImagePath);

            if (doesReferenceFileExist && (elementScreenshot!=null && elementScreenshot.length>0)) {
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
            }else{
                    ReportManager.logDiscrete("Passing the test and saving a reference image");
                    FileActions.getInstance().writeToFile(aiFolderPath, hashedLocatorName + ".png", elementScreenshot);
                return true;
            }
        }

        if (visualValidationEngine == VisualValidationEngine.EXACT_OPENCV) {
            String referenceImagePath = aiFolderPath + hashedLocatorName + ".png";

            boolean doesReferenceFileExist = FileActions.getInstance().doesFileExist(referenceImagePath);
             if (!doesReferenceFileExist || !ImageProcessingActions.findImageWithinCurrentPage(referenceImagePath, elementScreenshot).equals(Collections.emptyList())) {
                    //pass: element found and matched || first time element
                    if (!doesReferenceFileExist) {
                        ReportManager.logDiscrete("Passing the test and saving a reference image");
                        FileActions.getInstance().writeToFile(aiFolderPath, hashedLocatorName + ".png", elementScreenshot);
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
        eyes.setApiKey(System.getProperty("applitoolsApiKey"));
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
            eyes.setHostOS(System.getProperty("mobile_platformName") + "_" + System.getProperty("mobile_platformVersion"));
            eyes.setHostApp("NativeMobileExecution");
        } else if (DriverFactoryHelper.isMobileWebExecution()) {
            eyes.setHostOS(System.getProperty("mobile_platformName") + "_" + System.getProperty("mobile_platformVersion"));
            eyes.setHostApp(System.getProperty("mobile_browserName"));
        } else {
            eyes.setHostOS(System.getProperty("targetOperatingSystem"));
            eyes.setHostApp(System.getProperty("targetBrowserName"));
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

            ReportManagerHelper.log(
                    "Test Screenshot [" + relatedTestFileName + "] and related Refrence Image ["
                            + relatedReferenceFileName + "] match by [" + percentage + "] percent.",
                    Arrays.asList(referenceScreenshotAttachment, testScreenshotAttachment));

            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            try {
                // add to pass/fail counter depending on assertion result, without logging
                ReportManagerHelper.setDiscreteLogging(true);
                Validations.assertThat()
                        .number(percentage)
                        .isGreaterThanOrEquals(threshhold)
                        .perform();
                ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
                passedImagesCount++;
            } catch (AssertionError e) {
                ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
                // copying image to failed images directory
                FileActions.getInstance().copyFile(screenshot.getAbsolutePath(),
                        testProcessingFolder.getParent() + DIRECTORY_FAILED + relatedTestFileName + "_testImage");
                FileActions.getInstance().copyFile(
                        refrenceProcessingFolder + FileSystems.getDefault().getSeparator() + screenshot.getName(),
                        testProcessingFolder.getParent() + DIRECTORY_FAILED + relatedTestFileName + "_refrenceImage");
                failedImagesCount++;
            }

            Validations.verifyThat()
                    .number(percentage)
                    .isGreaterThanOrEquals(threshhold)
                    .perform();
        }

        ReportManager.log("[" + passedImagesCount + "] images passed, and [" + failedImagesCount
                + "] images failed the threshold of [" + threshhold + "%] matching.");

    }

    public static void loadOpenCV() {
        var libName = org.opencv.core.Core.NATIVE_LIBRARY_NAME;
        try {
            //https://github.com/openpnp/opencv#api
            OpenCV.loadLocally();
            ReportManager.logDiscrete("Loaded OpenCV \"" + libName + "\".");
        } catch (Exception throwable) {
            ReportManagerHelper.logDiscrete(throwable);
            ReportManager.logDiscrete("Failed to load OpenCV \"" + libName + "\". Try installing the binaries manually https://opencv.org/releases/, switching element highlighting to JavaScript...");
            System.setProperty("screenshotParams_highlightMethod", "JavaScript");
        }
    }

    public enum VisualValidationEngine {
        EXACT_SHUTTERBUG,
        EXACT_OPENCV,
        EXACT_EYES,
        STRICT_EYES,
        CONTENT_EYES,
        LAYOUT_EYES
    }
}