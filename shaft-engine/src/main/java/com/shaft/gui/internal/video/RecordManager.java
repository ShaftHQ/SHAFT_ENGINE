package com.shaft.gui.internal.video;

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
import org.openqa.selenium.remote.RemoteWebDriver;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Base64;

/**
 * Orchestrates SHAFT video recording for desktop browsers and Appium native sessions.
 *
 * <p>Desktop recording is delegated through {@link DesktopVideoRecordingProvider}; Android and iOS recording remain
 * driver-native so mobile users do not need the optional desktop video implementation.</p>
 */
public class RecordManager {
    private static final String MISSING_DESKTOP_PROVIDER_MESSAGE = "Desktop video recording is enabled, but no provider "
            + "is available. Add io.github.shafthq:shaft-video to the test runtime dependencies.";
    private static final ThreadLocal<WebDriver> videoDriver = new ThreadLocal<>();
    private static boolean isRecordingStarted = false;

    private RecordManager() {
        throw new IllegalStateException("Utility class");
    }

    //TODO: the animated GIF should follow the same path as the video
    /**
     * Starts Appium-native recording for mobile execution or falls back to desktop recording for local desktop drivers.
     *
     * @param driver the active WebDriver instance, or {@code null} to request desktop recording directly
     */
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
                ReportManager.logDiscrete("Started device screen recording.");
                isRecordingStarted = true;
            } catch (WebDriverException exception) {
                ReportManager.logDiscrete("Could not start device screen recording.");
            }
        } else if (driver == null || shouldFallbackToDesktopRecorder(driver)) {
            startVideoRecording();
        }
    }

    private static boolean shouldFallbackToDesktopRecorder(WebDriver driver) {
        return driver != null
                && SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.headlessExecution()
                && !(driver instanceof RemoteWebDriver);
    }

    /**
     * Starts desktop screen recording when video recording is enabled for local non-headless execution.
     *
     * @throws IllegalStateException when desktop recording is requested but no desktop provider is available
     */
    public static void startVideoRecording() {
        if (shouldRecordDesktop()) {
            DesktopVideoRecordingProvider provider = DesktopVideoRecordingProviderRegistry.findProvider()
                    .orElseThrow(() -> new IllegalStateException(MISSING_DESKTOP_PROVIDER_MESSAGE));
            if (!provider.isRecording()) {
                provider.startRecording();
            }
        }
    }

    private static boolean shouldRecordDesktop() {
        return SHAFT.Properties.visuals.videoParamsRecordVideo()
                && SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.headlessExecution();
    }

    /**
     * Attaches an existing video recording file to the current report.
     *
     * @param pathToRecording path to the recording file; {@code null} is ignored
     */
    public static void attachVideoRecording(Path pathToRecording) {
        if (pathToRecording != null) {
            String testMethodName = ReportManagerHelper.getTestMethodName();
            try (InputStream in = Files.newInputStream(pathToRecording)) {
                ReportManagerHelper.attach("Video Recording", testMethodName, in);
            } catch (IOException e) {
                ReportManagerHelper.logDiscrete(e);
            }
        }
    }

    /**
     * Stops the active recording, if any, and attaches it to the current report.
     */
    public static void attachVideoRecording() {
        ReportManagerHelper.attach("Video Recording", ReportManagerHelper.getTestMethodName(), getVideoRecording());
    }

    /**
     * Stops the active recording, writes it to SHAFT's temporary video path, and returns that path.
     *
     * @return the temporary file path, or an empty string when no recording can be written
     */
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

    /**
     * Stops the active desktop or Appium-native recording and returns its bytes.
     *
     * @return an input stream for the recording, or {@code null} when no recording is active or available
     */
    public static InputStream getVideoRecording() {
        InputStream desktopRecording = getDesktopVideoRecording();
        if (desktopRecording != null) {
            return desktopRecording;
        }

        InputStream inputStream = null;
        if (SHAFT.Properties.visuals.videoParamsRecordVideo() && videoDriver.get() != null) {
            String base64EncodedRecording = "";
            try {
                if (videoDriver.get() instanceof AndroidDriver androidDriver) {
                    base64EncodedRecording = androidDriver.stopRecordingScreen();
                } else if (videoDriver.get() instanceof IOSDriver iosDriver) {
                    base64EncodedRecording = iosDriver.stopRecordingScreen();
                }
            } catch (WebDriverException e) {
                ReportManager.logDiscrete("Could not stop device screen recording. The command may not be supported on this device.");
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

    private static InputStream getDesktopVideoRecording() {
        if (!SHAFT.Properties.visuals.videoParamsRecordVideo()) {
            return null;
        }

        return DesktopVideoRecordingProviderRegistry.findProvider()
                .filter(DesktopVideoRecordingProvider::isRecording)
                .map(provider -> provider.stopRecording(
                        ReportManagerHelper.isCurrentTestPassed(),
                        System.currentTimeMillis() + "_" + ReportManagerHelper.getTestMethodName()))
                .orElse(null);
    }
}
