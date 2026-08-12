package com.shaft.gui.internal.video;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.driver.MobileRecordingActionsContract;
import com.shaft.properties.internal.Properties;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.android.AndroidStartScreenRecordingOptions;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.ios.IOSStartScreenRecordingOptions;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class RecordManagerDesktopProviderTest {
    private boolean videoRecording;
    private String videoScope;
    private String executionAddress;
    private boolean headlessExecution;
    private String targetPlatform;
    private String mobileBrowserName;

    @BeforeMethod(alwaysRun = true)
    public void captureProperties() {
        videoRecording = SHAFT.Properties.visuals.videoParamsRecordVideo();
        videoScope = SHAFT.Properties.visuals.videoParamsScope();
        executionAddress = SHAFT.Properties.platform.executionAddress();
        headlessExecution = SHAFT.Properties.web.headlessExecution();
        targetPlatform = SHAFT.Properties.platform.targetPlatform();
        mobileBrowserName = SHAFT.Properties.mobile.browserName();
    }

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        DesktopVideoRecordingProviderRegistry.resetProviderForTesting();
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(videoRecording);
        SHAFT.Properties.visuals.set().videoParamsScope(videoScope);
        SHAFT.Properties.platform.set().executionAddress(executionAddress);
        SHAFT.Properties.platform.set().targetPlatform(targetPlatform);
        SHAFT.Properties.mobile.set().browserName(mobileBrowserName);
        SHAFT.Properties.web.set().headlessExecution(headlessExecution);
        Properties.clearForCurrentThread();
    }

    @Test
    public void desktopRecordingShouldDelegateStartAndStopToRegisteredProvider() throws Exception {
        StubDesktopVideoRecordingProvider provider = new StubDesktopVideoRecordingProvider();
        DesktopVideoRecordingProviderRegistry.setProviderForTesting(provider);
        enableDesktopRecording();

        RecordManager.startVideoRecording();
        try (InputStream recording = RecordManager.getVideoRecording()) {
            Assert.assertTrue(provider.startCalled);
            Assert.assertTrue(provider.stopped);
            Assert.assertEquals(new String(recording.readAllBytes(), StandardCharsets.UTF_8), "desktop-video");
        }
    }

    @Test
    public void missingProviderShouldBeActionableWhenDesktopRecordingIsRequested() {
        DesktopVideoRecordingProviderRegistry.setProviderForTesting(null);
        enableDesktopRecording();

        IllegalStateException exception = Assert.expectThrows(IllegalStateException.class,
                RecordManager::startVideoRecording);

        Assert.assertTrue(exception.getMessage().contains("io.github.shafthq:shaft-video"));
    }

    @Test
    public void missingProviderShouldNotFailWhenRecordingIsDisabled() {
        DesktopVideoRecordingProviderRegistry.setProviderForTesting(null);
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(false);
        SHAFT.Properties.platform.set().executionAddress("local");
        SHAFT.Properties.web.set().headlessExecution(false);

        RecordManager.startVideoRecording();
    }

    @Test
    public void nativeMobileRecordingStateShouldBeThreadLocal() throws Exception {
        AndroidDriver driverOne = mock(AndroidDriver.class);
        AndroidDriver driverTwo = mock(AndroidDriver.class);
        when(driverOne.stopRecordingScreen()).thenReturn(encodedVideo("one"));
        when(driverTwo.stopRecordingScreen()).thenReturn(encodedVideo("two"));
        CountDownLatch firstRecordingStarted = new CountDownLatch(1);
        CountDownLatch releaseFirstRecording = new CountDownLatch(1);
        AtomicReference<Throwable> workerFailure = new AtomicReference<>();

        Thread firstWorker = new Thread(() -> runMobileRecordingWorker(
                driverOne, firstRecordingStarted, releaseFirstRecording, workerFailure), "first-mobile-recorder");
        Thread secondWorker = new Thread(() -> {
            try {
                Assert.assertTrue(firstRecordingStarted.await(5, TimeUnit.SECONDS),
                        "First worker should start recording before the second worker runs");
                enableNativeMobileRecording();
                RecordManager.startVideoRecording(driverTwo);
                RecordManager.getVideoRecording();
            } catch (Throwable throwable) {
                workerFailure.compareAndSet(null, throwable);
            } finally {
                Properties.clearForCurrentThread();
            }
        }, "second-mobile-recorder");

        firstWorker.start();
        secondWorker.start();
        secondWorker.join(TimeUnit.SECONDS.toMillis(5));
        releaseFirstRecording.countDown();
        firstWorker.join(TimeUnit.SECONDS.toMillis(5));

        if (workerFailure.get() != null) {
            throw new AssertionError("Mobile recording worker failed", workerFailure.get());
        }
        Assert.assertFalse(firstWorker.isAlive(), "First worker should finish");
        Assert.assertFalse(secondWorker.isAlive(), "Second worker should finish");
        verify(driverOne).startRecordingScreen(any(AndroidStartScreenRecordingOptions.class));
        verify(driverTwo).startRecordingScreen(any(AndroidStartScreenRecordingOptions.class));
    }

    @Test
    public void automaticAndExplicitRecordingShouldNotStartOverOrStopEachOther() throws Exception {
        enableNativeMobileRecording();
        AndroidDriver automatic = mock(AndroidDriver.class);
        when(automatic.getSessionId()).thenReturn(new SessionId("automatic-recording-owner"));
        when(automatic.stopRecordingScreen()).thenReturn(encodedVideo("automatic"));
        RecordManager.startVideoRecording(automatic);
        MobileRecordingActionsContract automaticFacade =
                new SHAFT.GUI.WebDriver(automatic).mobile().recording();

        Assert.expectThrows(IllegalStateException.class, automaticFacade::start);
        Assert.expectThrows(IllegalStateException.class, automaticFacade::stop);
        try (InputStream recording = RecordManager.getVideoRecording()) {
            Assert.assertEquals(recording.readAllBytes(), "automatic".getBytes(StandardCharsets.UTF_8));
        }
        verify(automatic, times(1)).startRecordingScreen(any(AndroidStartScreenRecordingOptions.class));
        verify(automatic, times(1)).stopRecordingScreen();

        AndroidDriver explicit = mock(AndroidDriver.class);
        when(explicit.getSessionId()).thenReturn(new SessionId("explicit-recording-owner"));
        when(explicit.stopRecordingScreen()).thenReturn(encodedVideo("explicit"));
        MobileRecordingActionsContract explicitFacade =
                new SHAFT.GUI.WebDriver(explicit).mobile().recording();
        explicitFacade.start();

        RecordManager.startVideoRecording(explicit);
        Assert.assertNull(RecordManager.getVideoRecording());
        Assert.assertEquals(explicitFacade.stop(), "explicit".getBytes(StandardCharsets.UTF_8));
        verify(explicit, times(1)).startRecordingScreen(any());
        verify(explicit, times(1)).stopRecordingScreen();
    }

    @Test
    public void automaticStopFailureShouldRetainOwnershipUntilAProviderRetrySucceeds() throws Exception {
        enableNativeMobileRecording();
        AndroidDriver driver = mock(AndroidDriver.class);
        when(driver.getSessionId()).thenReturn(new SessionId("automatic-stop-retry"));
        RuntimeException stopFailure = new RuntimeException("provider stop failed");
        when(driver.stopRecordingScreen()).thenThrow(stopFailure).thenReturn(encodedVideo("retried"));
        RecordManager.startVideoRecording(driver);
        MobileRecordingActionsContract explicit = new SHAFT.GUI.WebDriver(driver).mobile().recording();

        Assert.assertNull(RecordManager.getVideoRecording());
        Assert.expectThrows(IllegalStateException.class, explicit::start);
        verify(driver, times(1)).startRecordingScreen(any(AndroidStartScreenRecordingOptions.class));

        try (InputStream recording = RecordManager.getVideoRecording()) {
            Assert.assertEquals(recording.readAllBytes(), "retried".getBytes(StandardCharsets.UTF_8));
        }
        explicit.start();
        verify(driver, times(2)).startRecordingScreen(any());
        verify(driver, times(2)).stopRecordingScreen();
    }

    @Test
    public void automaticDecodeFailureShouldReleaseTheLegacyHandleForANewRecording() throws Exception {
        enableNativeMobileRecording();
        AndroidDriver driver = mock(AndroidDriver.class);
        when(driver.getSessionId()).thenReturn(new SessionId("automatic-decode-recovery"));
        when(driver.stopRecordingScreen()).thenReturn("malformed!", encodedVideo("restarted"));

        RecordManager.startVideoRecording(driver);
        Assert.assertNull(RecordManager.getVideoRecording());
        RecordManager.startVideoRecording(driver);
        try (InputStream recording = RecordManager.getVideoRecording()) {
            Assert.assertEquals(recording.readAllBytes(), "restarted".getBytes(StandardCharsets.UTF_8));
        }

        verify(driver, times(2)).startRecordingScreen(any(AndroidStartScreenRecordingOptions.class));
        verify(driver, times(2)).stopRecordingScreen();
    }

    @Test
    public void teardownAfterPersistentStopFailureShouldNotSuppressTheNextDriverRecording() throws Exception {
        enableNativeMobileRecording();
        AndroidDriver closing = mock(AndroidDriver.class);
        when(closing.getSessionId()).thenReturn(new SessionId("automatic-stop-closing"));
        when(closing.stopRecordingScreen()).thenThrow(new RuntimeException("persistent provider stop failure"));
        RecordManager.startVideoRecording(closing);
        Assert.assertNull(RecordManager.getVideoRecording());

        new DriverFactoryHelper().closeDriver(closing);

        AndroidDriver next = mock(AndroidDriver.class);
        when(next.getSessionId()).thenReturn(new SessionId("automatic-stop-next"));
        when(next.stopRecordingScreen()).thenReturn(encodedVideo("next"));
        RecordManager.startVideoRecording(next);
        try (InputStream recording = RecordManager.getVideoRecording()) {
            Assert.assertEquals(recording.readAllBytes(), "next".getBytes(StandardCharsets.UTF_8));
        }
        verify(next).startRecordingScreen(any(AndroidStartScreenRecordingOptions.class));
        verify(next).stopRecordingScreen();
    }

    @Test
    public void teardownCleanupShouldPreserveAnotherDriversAutomaticRecording() throws Exception {
        enableNativeMobileRecording();
        SHAFT.Properties.visuals.set().videoParamsScope("TestMethod");
        AndroidDriver active = mock(AndroidDriver.class);
        when(active.getSessionId()).thenReturn(new SessionId("automatic-active-sibling"));
        when(active.stopRecordingScreen()).thenReturn(encodedVideo("active-sibling"));
        RecordManager.startVideoRecording(active);

        new DriverFactoryHelper().closeDriver(mock(AndroidDriver.class));

        try (InputStream recording = RecordManager.getVideoRecording()) {
            Assert.assertEquals(recording.readAllBytes(), "active-sibling".getBytes(StandardCharsets.UTF_8));
        }
        verify(active).startRecordingScreen(any(AndroidStartScreenRecordingOptions.class));
        verify(active).stopRecordingScreen();
        SHAFT.Properties.visuals.set().videoParamsScope(videoScope);
    }

    @Test
    public void iosAutomaticAndExplicitRecordingShouldShareTheSameOwner() throws Exception {
        enableNativeMobileRecording();
        IOSDriver driver = mock(IOSDriver.class);
        when(driver.getSessionId()).thenReturn(new SessionId("ios-automatic-recording-owner"));
        when(driver.stopRecordingScreen()).thenReturn(encodedVideo("ios-automatic"));
        RecordManager.startVideoRecording(driver);
        MobileRecordingActionsContract explicit = new SHAFT.GUI.WebDriver(driver).mobile().recording();

        Assert.expectThrows(IllegalStateException.class, explicit::start);
        Assert.expectThrows(IllegalStateException.class, explicit::stop);
        try (InputStream recording = RecordManager.getVideoRecording()) {
            Assert.assertEquals(recording.readAllBytes(), "ios-automatic".getBytes(StandardCharsets.UTF_8));
        }
        verify(driver).startRecordingScreen(any(IOSStartScreenRecordingOptions.class));
        verify(driver).stopRecordingScreen();
    }

    private void enableDesktopRecording() {
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
        SHAFT.Properties.platform.set().executionAddress("local");
        SHAFT.Properties.web.set().headlessExecution(false);
    }

    private void enableNativeMobileRecording() {
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
        SHAFT.Properties.platform.set().targetPlatform("Android");
        SHAFT.Properties.mobile.set().browserName("");
    }

    private void runMobileRecordingWorker(AndroidDriver driver, CountDownLatch recordingStarted,
                                          CountDownLatch releaseRecording,
                                          AtomicReference<Throwable> workerFailure) {
        try {
            enableNativeMobileRecording();
            RecordManager.startVideoRecording(driver);
            recordingStarted.countDown();
            Assert.assertTrue(releaseRecording.await(5, TimeUnit.SECONDS),
                    "First worker should be released before timeout");
            RecordManager.getVideoRecording();
        } catch (Throwable throwable) {
            workerFailure.compareAndSet(null, throwable);
        } finally {
            Properties.clearForCurrentThread();
        }
    }

    private String encodedVideo(String content) {
        return Base64.getEncoder().encodeToString(content.getBytes(StandardCharsets.UTF_8));
    }

    private static final class StubDesktopVideoRecordingProvider implements DesktopVideoRecordingProvider {
        private boolean started;
        private boolean startCalled;
        private boolean stopped;

        @Override
        public void startRecording() {
            started = true;
            startCalled = true;
        }

        @Override
        public InputStream stopRecording(boolean testPassed, String recordingName) {
            stopped = true;
            started = false;
            return new ByteArrayInputStream("desktop-video".getBytes(StandardCharsets.UTF_8));
        }

        @Override
        public boolean isRecording() {
            return started;
        }
    }
}
