package com.shaft.gui.internal.video;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class DesktopVideoRecordingProviderRegistrationTest {
    @AfterEach
    public void tearDown() {
        DesktopVideoRecordingProviderRegistry.resetProviderForTesting();
    }

    @Test
    public void serviceLoaderFindsAutomationRemarksProvider() {
        assertEquals(AutomationRemarksDesktopVideoRecordingProvider.class,
                DesktopVideoRecordingProviderRegistry.findProvider().orElseThrow().getClass());
    }
}
