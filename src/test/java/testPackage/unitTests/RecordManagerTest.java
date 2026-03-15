package testPackage.unitTests;

import com.shaft.gui.internal.video.RecordManager;
import io.appium.java_client.android.AndroidDriver;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.InputStream;
import java.lang.reflect.Field;

import static org.mockito.Mockito.*;
import static org.testng.Assert.*;

/**
 * Tests for RecordManager video recording lifecycle, including graceful handling
 * of unsupported stopRecordingScreen commands on certain Android/iOS devices.
 */
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

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws Exception {
        // Reset RecordManager state after each test
        getVideoDriverThreadLocal().remove();
        IS_RECORDING_STARTED_FIELD.setBoolean(null, false);
    }

    @Test(description = "getVideoRecording returns null when no recording is active")
    public void testGetVideoRecordingReturnsNullWhenNoRecording() {
        InputStream result = RecordManager.getVideoRecording();
        assertNull(result, "Should return null when no video recording is active");
    }

    @Test(description = "getVideoRecording handles WebDriverException from stopRecordingScreen gracefully")
    public void testGetVideoRecordingHandlesStopRecordingException() throws Exception {
        // Create a mock AndroidDriver that throws on stopRecordingScreen
        AndroidDriver mockDriver = mock(AndroidDriver.class);
        when(mockDriver.stopRecordingScreen())
                .thenThrow(new WebDriverException("Command is not supported"));

        // Set up RecordManager state as if recording was started
        getVideoDriverThreadLocal().set(mockDriver);
        IS_RECORDING_STARTED_FIELD.setBoolean(null, true);

        // Set video recording enabled via system property, preserving any existing value
        String previousValue = System.getProperty("videoParams_recordVideo");
        System.setProperty("videoParams_recordVideo", "true");

        try {
            // This should NOT throw - it should catch the WebDriverException gracefully
            InputStream result = RecordManager.getVideoRecording();
            assertNull(result,
                    "Should return null when stopRecordingScreen throws WebDriverException");
        } finally {
            if (previousValue != null) {
                System.setProperty("videoParams_recordVideo", previousValue);
            } else {
                System.clearProperty("videoParams_recordVideo");
            }
        }

        // Verify the mock was called (recording stop was attempted)
        verify(mockDriver).stopRecordingScreen();
    }

    @Test(description = "getVideoRecording cleans up state even when stopRecordingScreen fails")
    public void testGetVideoRecordingCleansUpOnStopFailure() throws Exception {
        AndroidDriver mockDriver = mock(AndroidDriver.class);
        when(mockDriver.stopRecordingScreen())
                .thenThrow(new WebDriverException("Command is not supported"));

        getVideoDriverThreadLocal().set(mockDriver);
        IS_RECORDING_STARTED_FIELD.setBoolean(null, true);

        String previousValue = System.getProperty("videoParams_recordVideo");
        System.setProperty("videoParams_recordVideo", "true");
        try {
            RecordManager.getVideoRecording();
        } finally {
            if (previousValue != null) {
                System.setProperty("videoParams_recordVideo", previousValue);
            } else {
                System.clearProperty("videoParams_recordVideo");
            }
        }

        // Verify state was cleaned up: videoDriver removed and isRecordingStarted reset
        assertNull(getVideoDriverThreadLocal().get(),
                "videoDriver ThreadLocal should be cleaned up after stop failure");
        assertFalse(IS_RECORDING_STARTED_FIELD.getBoolean(null),
                "isRecordingStarted should be reset to false after stop failure");
    }

    @Test(description = "attachVideoRecording does not throw when no recording is active")
    public void testAttachVideoRecordingNoOpWhenNoRecording() {
        // Should not throw any exception
        RecordManager.attachVideoRecording();
    }
}
