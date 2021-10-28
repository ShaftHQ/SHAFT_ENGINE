package com.shaft.gui.video;

import com.automation.remarks.video.RecorderFactory;
import com.automation.remarks.video.recorder.IVideoRecorder;
import com.automation.remarks.video.recorder.VideoRecorder;
import com.shaft.driver.DriverFactoryHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.android.AndroidStartScreenRecordingOptions;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.ios.IOSStartScreenRecordingOptions;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import ws.schild.jave.Encoder;
import ws.schild.jave.EncoderException;
import ws.schild.jave.MultimediaObject;
import ws.schild.jave.encode.AudioAttributes;
import ws.schild.jave.encode.EncodingAttributes;
import ws.schild.jave.encode.VideoAttributes;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.time.Duration;
import java.util.Base64;

import static com.automation.remarks.video.RecordingUtils.doVideoProcessing;

public class RecordManager {
    private static final Boolean RECORD_VIDEO = Boolean.valueOf(System.getProperty("videoParams_recordVideo").trim());
    private static final ThreadLocal<IVideoRecorder> recorder = new ThreadLocal<>();
    private static final ThreadLocal<WebDriver> videoDriver = new ThreadLocal<>();
    private static boolean isRecordingStarted = false;

    private RecordManager() {
        throw new IllegalStateException("Utility class");
    }

    //TODO: the animated GIF should follow the same path as the video
    public static synchronized void startVideoRecording(WebDriver driver) {
        if (Boolean.TRUE.equals(RECORD_VIDEO)
                && !isRecordingStarted
                && driver != null
                && DriverFactoryHelper.isMobileNativeExecution()) {
            videoDriver.set(driver);
            try {
                if (driver instanceof AndroidDriver androidDriver) {
                    androidDriver.startRecordingScreen(new AndroidStartScreenRecordingOptions().withVideoSize("540x960").withBitRate(2000000).withTimeLimit(Duration.ofMinutes(30)));
                } else if (driver instanceof IOSDriver iosDriver) {
                    iosDriver.startRecordingScreen(new IOSStartScreenRecordingOptions().withVideoType("libx264").withVideoQuality(IOSStartScreenRecordingOptions.VideoQuality.MEDIUM).withTimeLimit(Duration.ofMinutes(30)));
                }
                ReportManager.logDiscrete("Started recording device screen");
                isRecordingStarted = true;
            } catch (WebDriverException exception) {
                ReportManager.logDiscrete("Failed to start recording device screen");
            }
        } else {
            startVideoRecording();
        }
    }

    public static synchronized void startVideoRecording() {
        if (Boolean.TRUE.equals(RECORD_VIDEO)
                && System.getProperty("executionAddress").trim().equals("local")
                && Boolean.FALSE.equals(Boolean.valueOf(System.getProperty("headlessExecution").trim()))
                && recorder.get() == null) {
            recorder.set(RecorderFactory.getRecorder(VideoRecorder.conf().recorderType()));
            recorder.get().start();
        }
    }

    public static synchronized void attachVideoRecording() {
        String pathToRecording;
        String testMethodName = ReportManagerHelper.getTestMethodName();

        if (Boolean.TRUE.equals(RECORD_VIDEO) && recorder.get() != null) {
            pathToRecording = doVideoProcessing(ReportManagerHelper.isCurrentTestPassed(), recorder.get().stopAndSave(System.currentTimeMillis() + "_" + testMethodName));

            try {
                ReportManagerHelper.attach("Video Recording", testMethodName,
                        new FileInputStream(encodeRecording(pathToRecording)));
            } catch (FileNotFoundException e) {
                ReportManagerHelper.logDiscrete(e);
            }

            recorder.set(null);
        } else if (Boolean.TRUE.equals(RECORD_VIDEO) && videoDriver.get() != null) {
            String base64EncodedRecording = "";
            if (videoDriver.get() instanceof AndroidDriver androidDriver) {
                base64EncodedRecording = androidDriver.stopRecordingScreen();
            } else if (videoDriver.get() instanceof IOSDriver iosDriver) {
                base64EncodedRecording = iosDriver.stopRecordingScreen();
            }
            ReportManagerHelper.attach("Video Recording", testMethodName,
                    new ByteArrayInputStream(Base64.getDecoder().decode(base64EncodedRecording)));

            videoDriver.set(null);
            isRecordingStarted = false;
        }
    }

    private static synchronized File encodeRecording(String pathToRecording) {
        File source = new File(pathToRecording);
        File target = new File(pathToRecording.replace("avi", "mp4"));
        try {

            AudioAttributes audio = new AudioAttributes();
            audio.setCodec("libvorbis");
            VideoAttributes video = new VideoAttributes();
            EncodingAttributes attrs = new EncodingAttributes();
            attrs.setOutputFormat("mp4");
            attrs.setAudioAttributes(audio);
            attrs.setVideoAttributes(video);
            Encoder encoder = new Encoder();
            encoder.encode(new MultimediaObject(source), target, attrs);
        } catch (EncoderException e) {
            ReportManagerHelper.logDiscrete(e);
        }
        return target;
    }
}
