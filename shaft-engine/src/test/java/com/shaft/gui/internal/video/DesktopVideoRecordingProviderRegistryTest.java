package com.shaft.gui.internal.video;

import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

public class DesktopVideoRecordingProviderRegistryTest {
    @AfterMethod(alwaysRun = true)
    public void resetProvider() {
        DesktopVideoRecordingProviderRegistry.resetProviderForTesting();
    }

    @Test
    public void registeredTestProviderShouldOverrideServiceDiscovery() {
        DesktopVideoRecordingProvider provider = new StubDesktopVideoRecordingProvider();

        DesktopVideoRecordingProviderRegistry.setProviderForTesting(provider);

        Assert.assertSame(DesktopVideoRecordingProviderRegistry.findProvider().orElseThrow(), provider);
    }

    @Test
    public void serviceDiscoveryShouldFindNoProviderWithEngineAlone() {
        Assert.assertTrue(DesktopVideoRecordingProviderRegistry.findProvider().isEmpty());
    }

    @Test
    public void explicitEmptyTestProviderShouldOverrideServiceDiscovery() {
        DesktopVideoRecordingProviderRegistry.setProviderForTesting(null);

        Assert.assertTrue(DesktopVideoRecordingProviderRegistry.findProvider().isEmpty());
    }

    private static final class StubDesktopVideoRecordingProvider implements DesktopVideoRecordingProvider {
        @Override
        public void startRecording() {
            // No-op test provider.
        }

        @Override
        public InputStream stopRecording(boolean testPassed, String recordingName) {
            return new ByteArrayInputStream(new byte[0]);
        }

        @Override
        public boolean isRecording() {
            return false;
        }
    }
}
