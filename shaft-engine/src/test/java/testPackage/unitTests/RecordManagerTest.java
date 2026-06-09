package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.internal.video.RecordManager;
import com.shaft.properties.internal.Properties;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Coverage-focused unit tests for {@link RecordManager}.
 */
@Test(singleThreaded = true)
public class RecordManagerTest {
    private static final Field VIDEO_DRIVER_FIELD;
    private static final Field IS_RECORDING_STARTED_FIELD;

    static {
        try {
            VIDEO_DRIVER_FIELD = RecordManager.class.getDeclaredField("videoDriver");
            VIDEO_DRIVER_FIELD.setAccessible(true);
            IS_RECORDING_STARTED_FIELD = RecordManager.class.getDeclaredField("isRecordingStarted");
            IS_RECORDING_STARTED_FIELD.setAccessible(true);
        } catch (NoSuchFieldException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    @SuppressWarnings("unchecked")
    private ThreadLocal<WebDriver> getVideoDriverThreadLocal() throws Exception {
        return (ThreadLocal<WebDriver>) VIDEO_DRIVER_FIELD.get(null);
    }

    private void setVideoRecordingEnabled(boolean value) {
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(value);
    }

    @AfterClass(alwaysRun = true)
    public void afterClass() {
        new File("target/tempVideoFile").delete();
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws Exception {
        getVideoDriverThreadLocal().remove();
        IS_RECORDING_STARTED_FIELD.setBoolean(null, false);
        setVideoRecordingEnabled(false);
        Properties.clearForCurrentThread();
    }

    @Test(description = "RecordManager utility constructor is blocked")
    public void constructorShouldThrowIllegalStateException() throws Exception {
        Constructor<RecordManager> constructor = RecordManager.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        InvocationTargetException invocationException = null;
        try {
            constructor.newInstance();
        } catch (InvocationTargetException e) {
            invocationException = e;
        }
        SHAFT.Validations.assertThat().object(invocationException != null).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(invocationException.getCause().getClass().getSimpleName())
                .isEqualTo("IllegalStateException").perform();
    }

    @Test(description = "getVideoRecording returns null when no recording is active")
    public void getVideoRecordingReturnsNullWhenNoRecording() {
        InputStream result = RecordManager.getVideoRecording();
        SHAFT.Validations.assertThat().object(result).isNull().perform();
    }

    @Test(description = "getVideoRecording decodes Android base64 recording and resets static state")
    public void getVideoRecordingDecodesAndroidRecordingAndCleansUpState() throws Exception {
        setVideoRecordingEnabled(true);
        AndroidDriver mockDriver = mock(AndroidDriver.class);
        String encoded = Base64.getEncoder().encodeToString("video-bytes".getBytes(StandardCharsets.UTF_8));
        when(mockDriver.stopRecordingScreen()).thenReturn(encoded);
        getVideoDriverThreadLocal().set(mockDriver);
        IS_RECORDING_STARTED_FIELD.setBoolean(null, true);

        InputStream result = RecordManager.getVideoRecording();
        byte[] bytes = result.readAllBytes();
        result.close();

        SHAFT.Validations.assertThat().object(new String(bytes, StandardCharsets.UTF_8)).isEqualTo("video-bytes").perform();
        SHAFT.Validations.assertThat().object(getVideoDriverThreadLocal().get()).isNull().perform();
        SHAFT.Validations.assertThat().object(IS_RECORDING_STARTED_FIELD.getBoolean(null)).isEqualTo(false).perform();
        verify(mockDriver).stopRecordingScreen();
    }

    @Test(description = "getVideoRecording handles WebDriverException from stopRecordingScreen gracefully")
    public void getVideoRecordingHandlesStopRecordingException() throws Exception {
        setVideoRecordingEnabled(true);
        AndroidDriver mockDriver = mock(AndroidDriver.class);
        when(mockDriver.stopRecordingScreen())
                .thenThrow(new WebDriverException("Command is not supported"));

        getVideoDriverThreadLocal().set(mockDriver);
        IS_RECORDING_STARTED_FIELD.setBoolean(null, true);

        InputStream result = RecordManager.getVideoRecording();
        SHAFT.Validations.assertThat().object(result).isNull().perform();
        verify(mockDriver).stopRecordingScreen();
        SHAFT.Validations.assertThat().object(getVideoDriverThreadLocal().get()).isNull().perform();
        SHAFT.Validations.assertThat().object(IS_RECORDING_STARTED_FIELD.getBoolean(null)).isEqualTo(false).perform();
    }

    @Test(description = "startVideoRecording starts Android native recording when mobile execution is detected")
    public void startVideoRecordingStartsAndroidNativeRecording() throws Exception {
        setVideoRecordingEnabled(true);
        AndroidDriver mockDriver = mock(AndroidDriver.class);
        try (var driverFactoryHelperMock = mockStatic(DriverFactoryHelper.class)) {
            driverFactoryHelperMock.when(DriverFactoryHelper::isMobileNativeExecution).thenReturn(true);
            RecordManager.startVideoRecording(mockDriver);
        }
        verify(mockDriver).startRecordingScreen(any());
        SHAFT.Validations.assertThat().object(IS_RECORDING_STARTED_FIELD.getBoolean(null)).isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(getVideoDriverThreadLocal().get() != null).isEqualTo(true).perform();
    }

    @Test(description = "startVideoRecording swallows Android start exceptions and leaves recording flag false")
    public void startVideoRecordingHandlesAndroidStartException() throws Exception {
        setVideoRecordingEnabled(true);
        AndroidDriver mockDriver = mock(AndroidDriver.class);
        when(mockDriver.startRecordingScreen(any()))
                .thenThrow(new WebDriverException("Command is not supported"));
        try (var driverFactoryHelperMock = mockStatic(DriverFactoryHelper.class)) {
            driverFactoryHelperMock.when(DriverFactoryHelper::isMobileNativeExecution).thenReturn(true);
            RecordManager.startVideoRecording(mockDriver);
        }
        SHAFT.Validations.assertThat().object(IS_RECORDING_STARTED_FIELD.getBoolean(null)).isEqualTo(false).perform();
    }

    @Test(description = "startVideoRecording starts iOS native recording when mobile execution is detected")
    public void startVideoRecordingStartsIosNativeRecording() throws Exception {
        setVideoRecordingEnabled(true);
        IOSDriver mockDriver = mock(IOSDriver.class);
        try (var driverFactoryHelperMock = mockStatic(DriverFactoryHelper.class)) {
            driverFactoryHelperMock.when(DriverFactoryHelper::isMobileNativeExecution).thenReturn(true);
            RecordManager.startVideoRecording(mockDriver);
        }
        verify(mockDriver).startRecordingScreen(any());
        SHAFT.Validations.assertThat().object(IS_RECORDING_STARTED_FIELD.getBoolean(null)).isEqualTo(true).perform();
    }

    @Test(description = "getVideoRecordingFilePath writes decoded video to temp path")
    public void getVideoRecordingFilePathWritesTempFile() throws Exception {
        setVideoRecordingEnabled(true);
        AndroidDriver mockDriver = mock(AndroidDriver.class);
        String encoded = Base64.getEncoder().encodeToString("file-video".getBytes(StandardCharsets.UTF_8));
        when(mockDriver.stopRecordingScreen()).thenReturn(encoded);
        getVideoDriverThreadLocal().set(mockDriver);
        IS_RECORDING_STARTED_FIELD.setBoolean(null, true);

        String filePath = RecordManager.getVideoRecordingFilePath();
        SHAFT.Validations.assertThat().object(filePath).isEqualTo("target/tempVideoFile/").perform();
        SHAFT.Validations.assertThat().object(new File(filePath).exists()).isEqualTo(true).perform();
    }

    @Test(description = "attachVideoRecording path overload handles null and missing files")
    public void attachVideoRecordingPathOverloadHandlesNullAndMissingFile() {
        RecordManager.attachVideoRecording((Path) null);
        RecordManager.attachVideoRecording(Path.of("target", "missing-recording.mp4"));
    }

    @Test(description = "attachVideoRecording path overload accepts existing files")
    public void attachVideoRecordingPathOverloadAcceptsExistingFile() throws Exception {
        Path existingVideo = Path.of("target", "existing-recording.mp4");
        Files.createDirectories(existingVideo.getParent());
        Files.writeString(existingVideo, "video", StandardCharsets.UTF_8);
        RecordManager.attachVideoRecording(existingVideo);
        Files.deleteIfExists(existingVideo);
    }

    @Test(description = "attachVideoRecording no-args overload does not throw when no recording is active")
    public void attachVideoRecordingNoOpWhenNoRecording() {
        RecordManager.attachVideoRecording();
    }

    @Test(description = "startVideoRecording falls back when null driver is provided")
    public void startVideoRecordingFallsBackWhenDriverIsNull() {
        RecordManager.startVideoRecording((WebDriver) null);
    }
}
