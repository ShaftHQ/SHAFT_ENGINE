package com.shaft.gui.internal.image;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import nu.pattern.OpenCV;
import org.opencv.core.*;
import org.opencv.core.Point;
import org.opencv.highgui.HighGui;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

class OpenCVHelper {

    private static final int CV_THRESH_OTSU = 8;
    private static final int CV_THRESH_BINARY = 0;

    private OpenCVHelper() {}

    static void load() {
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

    static List<Integer> findImage(String referenceImagePath, byte[] currentPageScreenshot, int attemptNumber) {
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
                    default -> { /* keep TM_CCOEFF_NORMED */ }
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

    @SuppressWarnings("PMD.NPathComplexity")
    static byte[] highlightElement(byte[] targetScreenshot,
                                   org.openqa.selenium.Rectangle elementLocation, java.awt.Color highlightColor) {
        Mat img;
        try {
            img = Imgcodecs.imdecode(new MatOfByte(targetScreenshot), Imgcodecs.IMREAD_COLOR);
        } catch (java.lang.UnsatisfiedLinkError unsatisfiedLinkError) {
            load();
            try {
                img = Imgcodecs.imdecode(new MatOfByte(targetScreenshot), Imgcodecs.IMREAD_COLOR);
            } catch (java.lang.UnsatisfiedLinkError e) {
                ReportManager.logDiscrete("OpenCV not available for element highlighting after retry.");
                return targetScreenshot;
            }
        }

        int outlineThickness = 5;
        double elementHeight = elementLocation.getHeight(),
                elementWidth = elementLocation.getWidth(),
                xPos = elementLocation.getX(),
                yPos = elementLocation.getY();

        // IOS Native | macOS Browser | Linux Browser scaled | -> Repositioning
        if (SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(org.openqa.selenium.Platform.IOS.name())
                || SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(org.openqa.selenium.Platform.MAC.name())
                || (
                SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(org.openqa.selenium.Platform.LINUX.name())
                        && SHAFT.Properties.visuals.screenshotParamsScalingFactor() != Double.parseDouble("1")
        )
        ) {
            elementHeight *= 2;
            elementWidth *= 2;
            xPos *= 2;
            yPos *= 2;
        }

        // IOS Browser Repositioning
        if (SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(org.openqa.selenium.Platform.IOS.name()) && SHAFT.Properties.mobile.browserName().equalsIgnoreCase(org.openqa.selenium.remote.Browser.SAFARI.browserName())) {
            yPos += elementHeight + 2 * outlineThickness;
        }

        // Android Browser Repositioning
        if (SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(org.openqa.selenium.Platform.ANDROID.name()) && SHAFT.Properties.mobile.appPackage().equalsIgnoreCase("com.android.chrome")) {
            yPos += 2 * outlineThickness;
        }

        // MacOS Browser Repositioning
        if (SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(org.openqa.selenium.Platform.MAC.name())) {
            yPos += 2 * outlineThickness;
        }

        // Windows Browser Repositioning
        if (SHAFT.Properties.platform.targetPlatform().equalsIgnoreCase(org.openqa.selenium.Platform.WINDOWS.name())) {
            double scalingFactor = SHAFT.Properties.visuals.screenshotParamsScalingFactor();
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

        java.awt.Image tmpImg = HighGui.toBufferedImage(img);
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
}
