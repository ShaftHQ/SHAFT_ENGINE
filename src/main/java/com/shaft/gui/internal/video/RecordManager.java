package com.shaft.gui.internal.video;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.android.AndroidStartScreenRecordingOptions;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.ios.IOSStartScreenRecordingOptions;
import org.apache.commons.io.FileUtils;
import org.apache.log4j.BasicConfigurator;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;

import java.io.*;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Base64;

public class RecordManager {
    private static final ThreadLocal<Object> recorder = new ThreadLocal<>();
    private static final ThreadLocal<WebDriver> videoDriver = new ThreadLocal<>();
    private static boolean isRecordingStarted = false;

    private RecordManager() {
        throw new IllegalStateException("Utility class");
    }

    //TODO: the animated GIF should follow the same path as the video
    @SuppressWarnings("SpellCheckingInspection")
    public static void startVideoRecording(WebDriver driver) {
        if (SHAFT.Properties.visuals.videoParamsRecordVideo()
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
        if (SHAFT.Properties.visuals.videoParamsRecordVideo()
                && SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.headlessExecution()
                && recorder.get() == null) {
            BasicConfigurator.configure();
            ThreadLocalPropertiesManager.setGlobalProperty("video.save.mode", "ALL");
            ThreadLocalPropertiesManager.setGlobalProperty("video.folder", "target/video");
            try {
                Class<?> recorderTypeClass = Class.forName("com.automation.remarks.video.enums.RecorderType");
                Object monteType = Enum.valueOf((Class<Enum>) recorderTypeClass, "MONTE");
                Class<?> factoryClass = Class.forName("com.automation.remarks.video.RecorderFactory");
                Object rec = factoryClass.getMethod("getRecorder", recorderTypeClass).invoke(null, monteType);
                recorder.set(rec);
                rec.getClass().getMethod("start").invoke(rec);
            } catch (ReflectiveOperationException e) {
                ReportManager.logDiscrete("video-recorder-testng not available: " + e.getMessage());
            }
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

        if (SHAFT.Properties.visuals.videoParamsRecordVideo() && recorder.get() != null) {
            try {
                Object rec = recorder.get();
                String rawPath = (String) rec.getClass()
                        .getMethod("stopAndSave", String.class)
                        .invoke(rec, System.currentTimeMillis() + "_" + testMethodName);
                Class<?> utilsClass = Class.forName("com.automation.remarks.video.RecordingUtils");
                pathToRecording = (String) utilsClass
                        .getMethod("doVideoProcessing", boolean.class, String.class)
                        .invoke(null, ReportManagerHelper.isCurrentTestPassed(), rawPath);
                inputStream = new FileInputStream(encodeRecording(pathToRecording));
            } catch (ReflectiveOperationException | FileNotFoundException e) {
                ReportManagerHelper.logDiscrete(e);
            }
            recorder.remove();

        } else if (SHAFT.Properties.visuals.videoParamsRecordVideo() && videoDriver.get() != null) {
            String base64EncodedRecording = "";
            try {
                if (videoDriver.get() instanceof AndroidDriver androidDriver) {
                    base64EncodedRecording = androidDriver.stopRecordingScreen();
                } else if (videoDriver.get() instanceof IOSDriver iosDriver) {
                    base64EncodedRecording = iosDriver.stopRecordingScreen();
                }
            } catch (WebDriverException e) {
                ReportManager.logDiscrete("Failed to stop recording device screen (command may not be supported on this device)");
                ReportManagerHelper.logDiscrete(e);
            }
            if (base64EncodedRecording != null && !base64EncodedRecording.isBlank()) {
                inputStream = new ByteArrayInputStream(Base64.getDecoder().decode(base64EncodedRecording));
            }
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
            Class<?> audioAttrClass = Class.forName("ws.schild.jave.encode.AudioAttributes");
            Object audio = audioAttrClass.getDeclaredConstructor().newInstance();
            audioAttrClass.getMethod("setCodec", String.class).invoke(audio, "libvorbis");

            Class<?> videoAttrClass = Class.forName("ws.schild.jave.encode.VideoAttributes");
            Object video = videoAttrClass.getDeclaredConstructor().newInstance();

            Class<?> encAttrClass = Class.forName("ws.schild.jave.encode.EncodingAttributes");
            Object attrs = encAttrClass.getDeclaredConstructor().newInstance();
            encAttrClass.getMethod("setOutputFormat", String.class).invoke(attrs, "mp4");
            encAttrClass.getMethod("setAudioAttributes", audioAttrClass).invoke(attrs, audio);
            encAttrClass.getMethod("setVideoAttributes", videoAttrClass).invoke(attrs, video);

            Class<?> multimediaClass = Class.forName("ws.schild.jave.MultimediaObject");
            Object mm = multimediaClass.getDeclaredConstructor(File.class).newInstance(source);

            Class<?> encoderClass = Class.forName("ws.schild.jave.Encoder");
            Object encoder = encoderClass.getDeclaredConstructor().newInstance();
            encoderClass.getMethod("encode", multimediaClass, File.class, encAttrClass)
                    .invoke(encoder, mm, target, attrs);
        } catch (ReflectiveOperationException e) {
            ReportManagerHelper.logDiscrete(e);
        }
        return target;
    }
}
