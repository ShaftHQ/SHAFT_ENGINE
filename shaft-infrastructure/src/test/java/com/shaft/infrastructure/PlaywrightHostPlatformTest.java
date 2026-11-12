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
        assertTrue(failure.getMessage().contains("debian"));
        assertTrue(failure.getMessage().contains("13"));

        IllegalArgumentException ubuntu26 = assertThrows(IllegalArgumentException.class,
                () -> PlaywrightHostPlatform.resolve(SetupPlatform.LINUX, SetupArchitecture.X64,
                        "ID=ubuntu\nVERSION_ID=\"26.04\"\n"));
        assertTrue(ubuntu26.getMessage().contains("ubuntu"));
        assertTrue(ubuntu26.getMessage().contains("26.04"));

        IllegalArgumentException missingVersion = assertThrows(IllegalArgumentException.class,
                () -> PlaywrightHostPlatform.resolve(SetupPlatform.LINUX, SetupArchitecture.X64,
                        "ID=debian\n"));
        assertTrue(missingVersion.getMessage().contains("absent"));
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
    void currentMacOsAndDarwinTwentyFourUseTheMac15Artifacts() {
        assertEquals("mac15-arm64", PlaywrightHostPlatform.resolve(
                SetupPlatform.MACOS, SetupArchitecture.ARM64, "16.0").token());
        assertEquals("mac15-arm64", PlaywrightHostPlatform.resolve(
                SetupPlatform.MACOS, SetupArchitecture.ARM64, "24.6.0").token());
        assertEquals("mac15", PlaywrightHostPlatform.resolve(
                SetupPlatform.MACOS, SetupArchitecture.X64, "26.0").token());
    }

    @Test
    void unsupportedMacOsMajorFailsClosed() {
        assertThrows(IllegalArgumentException.class, () -> PlaywrightHostPlatform.resolve(
                SetupPlatform.MACOS, SetupArchitecture.ARM64, "14.7.8"));
        assertThrows(IllegalArgumentException.class, () -> PlaywrightHostPlatform.resolve(
                SetupPlatform.MACOS, SetupArchitecture.ARM64, "23.6.0"));
    }
}
