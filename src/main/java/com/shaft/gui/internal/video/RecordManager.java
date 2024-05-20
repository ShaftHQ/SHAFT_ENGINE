package com.shaft.gui.internal.video;

import com.automation.remarks.video.RecorderFactory;
import com.automation.remarks.video.recorder.IVideoRecorder;
import com.automation.remarks.video.recorder.VideoRecorder;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.android.AndroidStartScreenRecordingOptions;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.ios.IOSStartScreenRecordingOptions;
import org.apache.commons.io.FileUtils;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import ws.schild.jave.Encoder;
import ws.schild.jave.EncoderException;
import ws.schild.jave.MultimediaObject;
import ws.schild.jave.encode.AudioAttributes;
import ws.schild.jave.encode.EncodingAttributes;
import ws.schild.jave.encode.VideoAttributes;

import java.io.*;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Base64;

import static com.automation.remarks.video.RecordingUtils.doVideoProcessing;

public class RecordManager {
    private static final ThreadLocal<IVideoRecorder> recorder = new ThreadLocal<>();
    private static final ThreadLocal<WebDriver> videoDriver = new ThreadLocal<>();
    private static boolean isRecordingStarted = false;

    private RecordManager() {
        throw new IllegalStateException("Utility class");
    }

    //TODO: the animated GIF should follow the same path as the video
    @SuppressWarnings("SpellCheckingInspection")
    public static void startVideoRecording(WebDriver driver) {
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.videoParamsRecordVideo())
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

    public static void startVideoRecording() {
        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.videoParamsRecordVideo())
                && SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.headlessExecution()
                && recorder.get() == null) {
            recorder.set(RecorderFactory.getRecorder(VideoRecorder.conf().recorderType()));
            recorder.get().start();
        }
    }

    public static void attachVideoRecording(Path pathToRecording) {
        if (pathToRecording != null) {
            String testMethodName = ReportManagerHelper.getTestMethodName();
            try {
                ReportManagerHelper.attach("Video Recording", testMethodName,
                        new FileInputStream(pathToRecording.toString()));
            } catch (FileNotFoundException e) {
                ReportManagerHelper.logDiscrete(e);
            }
        }
    }

    public static void attachVideoRecording() {
        ReportManagerHelper.attach("Video Recording", ReportManagerHelper.getTestMethodName(), getVideoRecording());
    }

    public static String getVideoRecordingFilePath() {
        try {
            String tempFilePath = "target/tempVideoFile/";
            FileUtils.copyInputStreamToFile(getVideoRecording(), new File(tempFilePath));
            return tempFilePath;
        } catch (IOException e) {
            ReportManagerHelper.logDiscrete(e);
            return "";
        }
    }

    public static InputStream getVideoRecording() {
        InputStream inputStream = null;
        String pathToRecording;
        String testMethodName = ReportManagerHelper.getTestMethodName();

        if (Boolean.TRUE.equals(SHAFT.Properties.visuals.videoParamsRecordVideo()) && recorder.get() != null) {
            pathToRecording = doVideoProcessing(ReportManagerHelper.isCurrentTestPassed(), recorder.get().stopAndSave(System.currentTimeMillis() + "_" + testMethodName));
            try {
                inputStream = new FileInputStream(encodeRecording(pathToRecording));
            } catch (FileNotFoundException e) {
                ReportManagerHelper.logDiscrete(e);
//                inputStream = new ByteArrayInputStream(new byte[0]);
            }
            recorder.remove();

        } else if (Boolean.TRUE.equals(SHAFT.Properties.visuals.videoParamsRecordVideo()) && videoDriver.get() != null) {
            String base64EncodedRecording = "";
            if (videoDriver.get() instanceof AndroidDriver androidDriver) {
                base64EncodedRecording = androidDriver.stopRecordingScreen();
            } else if (videoDriver.get() instanceof IOSDriver iosDriver) {
                base64EncodedRecording = iosDriver.stopRecordingScreen();
            }
            inputStream = new ByteArrayInputStream(Base64.getDecoder().decode(base64EncodedRecording));
            videoDriver.remove();
            isRecordingStarted = false;
        }
        return inputStream;
    }

    @SuppressWarnings("SpellCheckingInspection")
    private static File encodeRecording(String pathToRecording) {
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
