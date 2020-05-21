package com.shaft.gui.video;

import com.shaft.tools.io.ReportManager;
import org.monte.media.Format;
import org.monte.media.FormatKeys;
import org.monte.media.FormatKeys.MediaType;
import org.monte.media.VideoFormatKeys;
import org.monte.media.math.Rational;
import org.monte.screenrecorder.ScreenRecorder;

import java.awt.*;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import static org.monte.media.VideoFormatKeys.*;

@Deprecated
public class RecordManager2 {

    private static final Boolean RECORD_VIDEO = Boolean.valueOf(System.getProperty("recordVideo").trim());
    private static final String RECORDING_FOLDER = System.getProperty("allureResultsFolderPath").trim() + "/"
            + "recordings/";
    private static ScreenRecorder screenRecorder;

    private RecordManager2() {
        throw new IllegalStateException("Utility class");
    }

    public static void startRecording() {
        // set the graphics configuration
        if (Boolean.TRUE.equals(RECORD_VIDEO) && screenRecorder == null && !GraphicsEnvironment.isHeadless()) {
            GraphicsConfiguration gc = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice()
                    .getDefaultConfiguration();

            try {
                screenRecorder = new ScreenRecorder(gc, gc.getBounds(),
                        new Format(MediaTypeKey, MediaType.FILE, MimeTypeKey, FormatKeys.MIME_QUICKTIME),
                        new Format(MediaTypeKey, MediaType.VIDEO, EncodingKey,
                                VideoFormatKeys.ENCODING_QUICKTIME_ANIMATION, CompressorNameKey,
                                ENCODING_AVI_TECHSMITH_SCREEN_CAPTURE, DepthKey, 24, FrameRateKey, Rational.valueOf(15),
                                QualityKey, 1.0f, KeyFrameIntervalKey, 15 * 60),
                        null, null, new File(RECORDING_FOLDER));
                screenRecorder.setMaxRecordingTime(3600000);
                // 3600000 milliseconds = 60 minutes = 1 hour
                screenRecorder.start();
            } catch (IOException | AWTException | NullPointerException e) {
                ReportManager.log(e);
            }
        }
    }

    public static void stopRecording() {
        if (Boolean.TRUE.equals(RECORD_VIDEO) && screenRecorder != null) {
            try {
                screenRecorder.stop();
            } catch (IOException e) {
                ReportManager.log(e);
            }
        }
    }

    public static void attachRecording() {
        if (Boolean.TRUE.equals(RECORD_VIDEO) && screenRecorder != null) {
            List<File> movies = screenRecorder.getCreatedMovieFiles();

            for (int i = 0; i < movies.size(); i++) {
                try {
                    ReportManager.attach("Video Recording", "Execution Video #" + i + 1,
                            new FileInputStream(movies.get(i)));
                } catch (FileNotFoundException e) {
                    ReportManager.log(e);
                }
            }
        }
    }

    public static Boolean getRecordVideo() {
        return RECORD_VIDEO;
    }
}
