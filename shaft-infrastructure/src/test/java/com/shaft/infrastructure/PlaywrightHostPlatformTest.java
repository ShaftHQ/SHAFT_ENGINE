package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PlaywrightHostPlatformTest {
    @Test
    void exactUbuntuTwentyFourX64HostIsAccepted() {
        PlaywrightHostPlatform host = PlaywrightHostPlatform.resolve(
                SetupPlatform.LINUX, SetupArchitecture.X64,
                "NAME=Ubuntu\nID=ubuntu\nVERSION_ID=\"24.04\"\n");

        assertEquals("ubuntu24.04-x64", host.token());
        assertEquals(5, host.requiredArtifacts().size());
        assertTrue(host.requiredArtifacts().contains("chromium-headless-shell"));
    }

    @Test
    void unsupportedLinuxDistributionFailsClosed() {
        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> PlaywrightHostPlatform.resolve(SetupPlatform.LINUX, SetupArchitecture.X64,
                        "ID=debian\nVERSION_ID=\"13\"\n"));

        assertTrue(failure.getMessage().contains("Unsupported Playwright host"));
    }

    @Test
    void ubuntuArm64FailsBeforePlanning() {
        assertThrows(IllegalArgumentException.class,
                () -> PlaywrightHostPlatform.resolve(SetupPlatform.LINUX, SetupArchitecture.ARM64,
                        "ID=ubuntu\nVERSION_ID=\"24.04\"\n"));
    }

    @Test
    void macOsFifteenIsArchitectureExact() {
        assertEquals("mac15", PlaywrightHostPlatform.resolve(
                SetupPlatform.MACOS, SetupArchitecture.X64, "15.7.1").token());
        assertEquals("mac15-arm64", PlaywrightHostPlatform.resolve(
                SetupPlatform.MACOS, SetupArchitecture.ARM64, "15.7.1").token());
    }

    @Test
    void unsupportedMacOsMajorFailsClosed() {
        assertThrows(IllegalArgumentException.class, () -> PlaywrightHostPlatform.resolve(
                SetupPlatform.MACOS, SetupArchitecture.ARM64, "14.7.8"));
    }
}
