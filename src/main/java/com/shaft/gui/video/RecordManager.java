package com.shaft.gui.video;

import com.automation.remarks.video.RecorderFactory;
import com.automation.remarks.video.recorder.IVideoRecorder;
import com.automation.remarks.video.recorder.VideoRecorder;
import com.shaft.tools.io.ReportManager;
import org.testng.Reporter;
import ws.schild.jave.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import static com.automation.remarks.video.RecordingUtils.doVideoProcessing;

public class RecordManager {
    private static final Boolean RECORD_VIDEO = Boolean.valueOf(System.getProperty("recordVideo").trim());
    private static final ThreadLocal<IVideoRecorder> recorder = new ThreadLocal<>();

    private RecordManager() {
        throw new IllegalStateException("Utility class");
    }

    //TODO: the animated GIF should follow the same path as the video
    public static synchronized void startVideoRecording() {
        if (Boolean.TRUE.equals(RECORD_VIDEO)
                && System.getProperty("executionAddress").trim().equals("local")
                && Boolean.FALSE.equals(Boolean.valueOf(System.getProperty("headlessExecution").trim()))
                && recorder.get() == null) {
            recorder.set(RecorderFactory.getRecorder(VideoRecorder.conf().recorderType()));
            recorder.get().start();
            ReportManager.logDiscrete("Starting video recording...");
        }
    }

    public static synchronized void attachVideoRecording() {
        if (Boolean.TRUE.equals(RECORD_VIDEO) && recorder.get() != null) {
            String testMethodName = Reporter.getCurrentTestResult().getMethod().getMethodName();
            String pathToRecording = doVideoProcessing(Reporter.getCurrentTestResult().isSuccess(), recorder.get().stopAndSave(System.currentTimeMillis() + "_" + testMethodName));
            ReportManager.logDiscrete("Saved video recording to [" + pathToRecording + "].");
            encodeAndAttach(pathToRecording, testMethodName);
            recorder.set(null);
        }
    }

    private static synchronized void encodeAndAttach(String pathToRecording, String testMethodName) {
        File source = new File(pathToRecording);
        File target = new File(pathToRecording.replace("avi", "mp4"));
        try {
            ReportManager.logDiscrete("Encoding video...");
            AudioAttributes audio = new AudioAttributes();
            audio.setCodec("libvorbis");
            VideoAttributes video = new VideoAttributes();
            video.setFrameRate(30);
            EncodingAttributes attrs = new EncodingAttributes();
            attrs.setFormat("mp4");
            attrs.setAudioAttributes(audio);
            attrs.setVideoAttributes(video);
            Encoder encoder = new Encoder();
            encoder.encode(new MultimediaObject(source), target, attrs);
        } catch (EncoderException e) {
            ReportManager.logDiscrete(e);
        }

        try {
            ReportManager.attach("Video Recording", testMethodName,
                    new FileInputStream(target));
        } catch (FileNotFoundException e) {
            try {
                ReportManager.attach("Video Recording", testMethodName,
                        new FileInputStream(target));
            } catch (FileNotFoundException e2) {
                ReportManager.logDiscrete(e2);
            }
        }
    }
}
