package com.shaft.gui.internal.image;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import nu.pattern.OpenCV;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.highgui.HighGui;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

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
 * Current OpenCV-backed implementation of SHAFT visual processing.
 *
 * <p>This provider remains in the engine until it moves to the optional {@code io.github.shafthq:shaft-visual} artifact.</p>
 */
public class OpenCvVisualProcessingProvider implements VisualProcessingProvider {
    private static final int CV_THRESH_OTSU = 8;
    private static final int CV_THRESH_BINARY = 0;

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
