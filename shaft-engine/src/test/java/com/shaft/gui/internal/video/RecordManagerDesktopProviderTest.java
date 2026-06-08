package com.shaft.gui.internal.video;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public class RecordManagerDesktopProviderTest {
    private boolean videoRecording;
    private String executionAddress;
    private boolean headlessExecution;

    @BeforeMethod(alwaysRun = true)
    public void captureProperties() {
        videoRecording = SHAFT.Properties.visuals.videoParamsRecordVideo();
        executionAddress = SHAFT.Properties.platform.executionAddress();
        headlessExecution = SHAFT.Properties.web.headlessExecution();
    }

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        DesktopVideoRecordingProviderRegistry.resetProviderForTesting();
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(videoRecording);
        SHAFT.Properties.platform.set().executionAddress(executionAddress);
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

    private void enableDesktopRecording() {
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
        SHAFT.Properties.platform.set().executionAddress("local");
        SHAFT.Properties.web.set().headlessExecution(false);
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
