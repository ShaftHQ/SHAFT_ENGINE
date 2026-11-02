package com.shaft.gui.internal.video;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.lang.reflect.Method;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

public class DesktopVideoRecordingProviderRegistrationTest {
    @TempDir
    Path temp;

    @AfterEach
    public void tearDown() {
        DesktopVideoRecordingProviderRegistry.resetProviderForTesting();
    }

    @Test
    public void serviceLoaderFindsAutomationRemarksProvider() {
        assertEquals(AutomationRemarksDesktopVideoRecordingProvider.class,
                DesktopVideoRecordingProviderRegistry.findProvider().orElseThrow().getClass());
    }

    @Test
    public void idleProviderShouldNotStopOrReportRecording() {
        AutomationRemarksDesktopVideoRecordingProvider provider = new AutomationRemarksDesktopVideoRecordingProvider();

        assertFalse(provider.isRecording());
        assertNull(provider.stopRecording(true, "idle"));
    }

    @Test
    public void encodeRecordingShouldReturnMp4TargetWhenEncoderCannotReadSource() throws Exception {
        AutomationRemarksDesktopVideoRecordingProvider provider = new AutomationRemarksDesktopVideoRecordingProvider();
        Method method = AutomationRemarksDesktopVideoRecordingProvider.class
                .getDeclaredMethod("encodeRecording", String.class);
        method.setAccessible(true);

        File target = (File) method.invoke(provider, temp.resolve("missing.avi").toString());

        assertEquals("missing.mp4", target.getName());
    }
}
