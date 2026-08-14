package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.HexFormat;

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

        assertEquals("1.62.0", PlaywrightSetupPlanner.PLAYWRIGHT_VERSION);
        assertTrue(canonical.contains("\"hostPlatform\": \"win64\""));
        assertTrue(canonical.contains("\"name\": \"chromium-headless-shell\""));
        assertTrue(canonical.contains("46cc69ef55ba29268ffe32dda4192a9d2165be42c3f4e923241153d519493aea"));
        assertTrue(canonical.contains("\"name\": \"winldd\""));
        assertTrue(canonical.contains("0069f0d11d4ad6df068a068c003d22fe7dbec192a47bba64b2e115e9c8ce41d8"));
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
}
