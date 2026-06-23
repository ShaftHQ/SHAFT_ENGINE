package com.shaft.gui.internal.video;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.android.AndroidStartScreenRecordingOptions;
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
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class RecordManagerDesktopProviderTest {
    private boolean videoRecording;
    private String executionAddress;
    private boolean headlessExecution;
    private String targetPlatform;
    private String mobileBrowserName;

    @BeforeMethod(alwaysRun = true)
    public void captureProperties() {
        videoRecording = SHAFT.Properties.visuals.videoParamsRecordVideo();
        executionAddress = SHAFT.Properties.platform.executionAddress();
        headlessExecution = SHAFT.Properties.web.headlessExecution();
        targetPlatform = SHAFT.Properties.platform.targetPlatform();
        mobileBrowserName = SHAFT.Properties.mobile.browserName();
    }

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        DesktopVideoRecordingProviderRegistry.resetProviderForTesting();
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(videoRecording);
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
