package com.shaft.io;

import java.awt.AWTException;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsEnvironment;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import org.monte.media.Format;
import org.monte.media.FormatKeys;
import org.monte.media.FormatKeys.MediaType;
import org.monte.media.VideoFormatKeys;
import org.monte.media.math.Rational;
import org.monte.screenrecorder.ScreenRecorder;
import static org.monte.media.VideoFormatKeys.*;

public class RecordManager {

    private static final Boolean RECORD_VIDEO = Boolean.valueOf(System.getProperty("recordVideo").trim());
    private static ScreenRecorder screenRecorder;
    private static final String RECORDING_FOLDER = System.getProperty("allureResultsFolderPath").trim() + "/" + "recordings/";

    private RecordManager() {
	throw new IllegalStateException("Utility class");
    }

    protected static void startRecording() {

	if (RECORD_VIDEO && screenRecorder == null) {
	    // set the graphics configuration
	    if (!GraphicsEnvironment.isHeadless()) {
		GraphicsConfiguration gc = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration();

		try {
		    // screenRecorder = new ScreenRecorder(gc);

		    // new Format(MediaTypeKey, MediaType.VIDEO, EncodingKey, "black", FrameRateKey,
		    // Rational.valueOf(30)), null, new File(RECORDING_FOLDER));

		    screenRecorder = new ScreenRecorder(gc, gc.getBounds(), new Format(MediaTypeKey, MediaType.FILE, MimeTypeKey, FormatKeys.MIME_QUICKTIME), new Format(MediaTypeKey, MediaType.VIDEO, EncodingKey, VideoFormatKeys.ENCODING_QUICKTIME_ANIMATION, CompressorNameKey, ENCODING_AVI_TECHSMITH_SCREEN_CAPTURE, DepthKey, 24, FrameRateKey, Rational.valueOf(15), QualityKey, 1.0f, KeyFrameIntervalKey, 15 * 60), null, null, new File(RECORDING_FOLDER));

		    screenRecorder.setMaxRecordingTime(3600000); // 3600000 milliseconds = 60 minutes = 1 hour

		    screenRecorder.start();

		} catch (IOException | AWTException | NullPointerException e) {
		    ReportManager.log(e);
		}
	    }
	}
    }

    protected static void stopRecording() {
	if (RECORD_VIDEO && screenRecorder != null) {
	    try {
		screenRecorder.stop();
	    } catch (IOException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	    }
	}
    }

    protected static void attachRecording() {
	if (RECORD_VIDEO && screenRecorder != null) {
	    List<File> movies = screenRecorder.getCreatedMovieFiles();

	    for (int i = 0; i < movies.size(); i++) {
		try {
		    ReportManager.attach("Video Recording", "Execution Video #" + i + 1, new FileInputStream(movies.get(i)));
		} catch (FileNotFoundException e) {
		    // TODO Auto-generated catch block
		    e.printStackTrace();
		}
	    }
	}
    }
}
