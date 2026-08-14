package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PlaywrightArtifactManifestTest {
    @Test
    void canonicalWindowsManifestHasEveryUpstreamInstallArtifact() {
        PlaywrightArtifactManifest manifest = PlaywrightArtifactManifest.load();
        List<PlaywrightArtifactManifest.Artifact> artifacts = manifest.requirePlatform("win64");

        assertEquals("1.62.0", manifest.playwrightVersion());
        assertEquals(List.of("chromium", "chromium-headless-shell", "firefox", "webkit", "ffmpeg", "winldd"),
                artifacts.stream().map(PlaywrightArtifactManifest.Artifact::name).toList());
        assertTrue(artifacts.stream().allMatch(artifact -> artifact.source().getScheme().equals("https")));
        assertTrue(artifacts.stream().allMatch(artifact -> artifact.checksum().matches("sha256:[0-9a-f]{64}")));
        assertTrue(artifacts.stream().allMatch(artifact -> artifact.size() > 0 && artifact.size() <= 512L * 1024 * 1024));
    }

    @Test
    void canonicalUbuntu24ManifestHasTheExactCurrentRunnerArtifacts() {
        List<PlaywrightArtifactManifest.Artifact> artifacts = PlaywrightArtifactManifest.load()
                .requirePlatform("ubuntu24.04-x64");

        assertEquals(List.of("chromium", "chromium-headless-shell", "firefox", "webkit", "ffmpeg"),
                artifacts.stream().map(PlaywrightArtifactManifest.Artifact::name).toList());
        assertEquals("sha256:49208d3ed74e525575eca41d053082902b88933e31943a091d25080bdfcfcb25",
                artifacts.stream().filter(artifact -> artifact.name().equals("firefox")).findFirst().orElseThrow()
                        .checksum());
        assertEquals("sha256:357cd5d52f0770f01a86c1e0e2e193b5cfdb760452fc766e26eb5c8ee3951345",
                artifacts.stream().filter(artifact -> artifact.name().equals("webkit")).findFirst().orElseThrow()
                        .checksum());
    }

    @Test
    void parserRejectsUnknownMetadataAndDuplicateArtifactNames() throws Exception {
        String canonical;
        try (var input = PlaywrightArtifactManifestTest.class.getResourceAsStream(
                "/com/shaft/infrastructure/playwright/browser-artifacts.json")) {
            canonical = new String(java.util.Objects.requireNonNull(input).readAllBytes(), StandardCharsets.UTF_8);
        }

        assertThrows(IllegalArgumentException.class,
                () -> PlaywrightArtifactManifest.parse(canonical.replaceFirst("\\{", "{\"unknown\":true,")));
        assertThrows(IllegalArgumentException.class,
                () -> PlaywrightArtifactManifest.parse(canonical.replaceFirst(
                        "\"name\": \"chromium-headless-shell\"", "\"name\": \"chromium\"")));
    }

    @Test
    void parserRejectsUnapprovedSourceAndMalformedChecksum() throws Exception {
        String canonical;
        try (var input = PlaywrightArtifactManifestTest.class.getResourceAsStream(
                "/com/shaft/infrastructure/playwright/browser-artifacts.json")) {
            canonical = new String(java.util.Objects.requireNonNull(input).readAllBytes(), StandardCharsets.UTF_8);
        }

        assertThrows(IllegalArgumentException.class,
                () -> PlaywrightArtifactManifest.parse(canonical.replace(
                        "https://cdn.playwright.dev/", "https://example.invalid/")));
        assertThrows(IllegalArgumentException.class,
                () -> PlaywrightArtifactManifest.parse(canonical.replaceFirst(
                        "045621e45a9dd27002c7fc1d8e10fe9f5f71f4cadbf44ec6f397f56f0179725c", "bad")));
    }
}
