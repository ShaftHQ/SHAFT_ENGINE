package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.HexFormat;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PlaywrightSetupPlannerTest {
    @Test
    void windowsPlanBindsTheCanonicalBrowserArtifactManifest() throws Exception {
        byte[] manifest;
        try (var input = PlaywrightSetupPlannerTest.class.getResourceAsStream(
                "/com/shaft/infrastructure/playwright/browser-artifacts.json")) {
            manifest = java.util.Objects.requireNonNull(input, "Playwright browser manifest").readAllBytes();
        }
        String canonical = new String(manifest, StandardCharsets.UTF_8)
                .replace("\r\n", "\n").replace('\r', '\n');
        String manifestDigest = "sha256:" + HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256")
                .digest(canonical.getBytes(StandardCharsets.UTF_8)));
        SetupPlan plan = PlaywrightSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED);

        assertEquals(PlaywrightArtifactManifest.load().playwrightVersion(), PlaywrightSetupPlanner.PLAYWRIGHT_VERSION);
        assertTrue(PlaywrightSetupPlanner.PLAYWRIGHT_VERSION.matches("\\d+\\.\\d+\\.\\d+"));
        assertTrue(canonical.contains("\"hostPlatform\": \"win64\""));
        assertTrue(canonical.contains("\"name\": \"chromium-headless-shell\""));
        assertTrue(canonical.contains("\"name\": \"winldd\""));
        assertTrue(plan.actions().stream().skip(1)
                .allMatch(action -> action.dependencyLockChecksum().equals(manifestDigest)));
    }

    @Test
    void externalPlanPreservesExactArtifactsButDiagnosesOnly() {
        SetupPlan managed = PlaywrightSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.ARM64,
                SetupMode.MANAGED);
        SetupPlan external = PlaywrightSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.ARM64,
                SetupMode.EXTERNAL);

        assertTrue(managed.actions().stream().allMatch(action -> action.kind() == SetupActionKind.INSTALL));
        assertTrue(external.actions().stream().allMatch(action -> action.kind() == SetupActionKind.DIAGNOSE));
        assertEquals(managed.actions().stream().map(SetupAction::source).toList(),
                external.actions().stream().map(SetupAction::source).toList());
        assertEquals(managed.actions().stream().map(SetupAction::checksum).toList(),
                external.actions().stream().map(SetupAction::checksum).toList());
    }

    @Test
    void ubuntuTwentyFourPlanUsesTheReviewedLinuxArtifacts() {
        SetupPlan plan = PlaywrightSetupPlanner.plan(PlaywrightHostPlatform.UBUNTU_24_04_X64,
                SetupMode.MANAGED);

        assertEquals(SetupPlatform.LINUX, plan.platform());
        assertEquals(SetupArchitecture.X64, plan.architecture());
        assertTrue(plan.actions().stream().anyMatch(action -> action.source().toString()
                .endsWith("firefox-ubuntu-24.04.zip")));
        assertTrue(plan.actions().stream().noneMatch(action -> action.version().contains("winldd")));
    }

    @Test
    void macOsPlansAreBoundToArchitectureSpecificReviewedArtifacts() {
        SetupPlan x64 = PlaywrightSetupPlanner.plan(PlaywrightHostPlatform.MAC15,
                SetupArchitecture.X64, SetupMode.MANAGED);
        SetupPlan arm64 = PlaywrightSetupPlanner.plan(PlaywrightHostPlatform.MAC15_ARM64,
                SetupArchitecture.ARM64, SetupMode.MANAGED);

        assertTrue(x64.actions().stream().anyMatch(action -> action.source().toString()
                .endsWith("webkit-mac-15.zip")));
        assertTrue(arm64.actions().stream().anyMatch(action -> action.source().toString()
                .endsWith("webkit-mac-15-arm64.zip")));
        assertTrue(x64.actions().stream().map(SetupAction::checksum).noneMatch(
                arm64.actions().stream().map(SetupAction::checksum).toList()::contains));
    }

    @Test
    void everyHostPublishesThePinnedPayloadDirectories() {
        String chromium = "chromium-" + PlaywrightSetupPlanner.CHROMIUM_REVISION;
        String headlessShell = "chromium_headless_shell-" + PlaywrightSetupPlanner.CHROMIUM_REVISION;
        String firefox = "firefox-" + PlaywrightSetupPlanner.FIREFOX_REVISION;
        String webkit = "webkit-" + PlaywrightSetupPlanner.WEBKIT_REVISION;
        String ffmpeg = "ffmpeg-" + PlaywrightSetupPlanner.FFMPEG_REVISION;

        assertEquals(List.of(chromium + "/INSTALLATION_COMPLETE", chromium + "/chrome-win64/chrome.exe",
                        headlessShell + "/INSTALLATION_COMPLETE",
                        headlessShell + "/chrome-headless-shell-win64/chrome-headless-shell.exe",
                        "winldd-1007/INSTALLATION_COMPLETE", "winldd-1007/PrintDeps.exe"),
                PlaywrightHostPlatform.WIN64.requiredPaths(SetupTarget.PLAYWRIGHT_CHROMIUM));
        assertEquals(List.of(firefox + "/INSTALLATION_COMPLETE", firefox + "/firefox/firefox.exe"),
                PlaywrightHostPlatform.WIN64.requiredPaths(SetupTarget.PLAYWRIGHT_FIREFOX));
        assertEquals(List.of(webkit + "/INSTALLATION_COMPLETE", webkit + "/Playwright.exe"),
                PlaywrightHostPlatform.WIN64.requiredPaths(SetupTarget.PLAYWRIGHT_WEBKIT));
        assertEquals(List.of(ffmpeg + "/INSTALLATION_COMPLETE", ffmpeg + "/ffmpeg-win64.exe"),
                PlaywrightHostPlatform.WIN64.requiredPaths(SetupTarget.FFMPEG));

        assertEquals(List.of(chromium + "/INSTALLATION_COMPLETE", chromium + "/chrome-linux64/chrome",
                        headlessShell + "/INSTALLATION_COMPLETE",
                        headlessShell + "/chrome-headless-shell-linux64/chrome-headless-shell"),
                PlaywrightHostPlatform.UBUNTU_24_04_X64.requiredPaths(SetupTarget.PLAYWRIGHT_CHROMIUM));
        assertEquals(List.of(firefox + "/INSTALLATION_COMPLETE", firefox + "/firefox/firefox"),
                PlaywrightHostPlatform.UBUNTU_24_04_X64.requiredPaths(SetupTarget.PLAYWRIGHT_FIREFOX));
        assertEquals(List.of(webkit + "/INSTALLATION_COMPLETE", webkit + "/pw_run.sh"),
                PlaywrightHostPlatform.UBUNTU_24_04_X64.requiredPaths(SetupTarget.PLAYWRIGHT_WEBKIT));
        assertEquals(List.of(ffmpeg + "/INSTALLATION_COMPLETE", ffmpeg + "/ffmpeg-linux"),
                PlaywrightHostPlatform.UBUNTU_24_04_X64.requiredPaths(SetupTarget.FFMPEG));

        assertTrue(PlaywrightHostPlatform.MAC15.requiredPaths(SetupTarget.PLAYWRIGHT_CHROMIUM).contains(
                chromium + "/chrome-mac-x64/Google Chrome for Testing.app/Contents/MacOS/Google Chrome for Testing"));
        assertTrue(PlaywrightHostPlatform.MAC15_ARM64.requiredPaths(SetupTarget.PLAYWRIGHT_CHROMIUM).contains(
                chromium + "/chrome-mac-arm64/Google Chrome for Testing.app/Contents/MacOS/Google Chrome for Testing"));
        assertEquals(List.of(firefox + "/INSTALLATION_COMPLETE",
                        firefox + "/firefox/Nightly.app/Contents/MacOS/firefox"),
                PlaywrightHostPlatform.MAC15.requiredPaths(SetupTarget.PLAYWRIGHT_FIREFOX));
        assertEquals(List.of(webkit + "/INSTALLATION_COMPLETE", webkit + "/pw_run.sh"),
                PlaywrightHostPlatform.MAC15.requiredPaths(SetupTarget.PLAYWRIGHT_WEBKIT));
        assertEquals(List.of(ffmpeg + "/INSTALLATION_COMPLETE", ffmpeg + "/ffmpeg-mac"),
                PlaywrightHostPlatform.MAC15.requiredPaths(SetupTarget.FFMPEG));
        assertEquals(PlaywrightHostPlatform.MAC15.requiredPaths(SetupTarget.PLAYWRIGHT_WEBKIT),
                PlaywrightHostPlatform.MAC15_ARM64.requiredPaths(SetupTarget.PLAYWRIGHT_WEBKIT));
    }
}
