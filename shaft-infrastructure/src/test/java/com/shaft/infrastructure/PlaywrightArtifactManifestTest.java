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

        assertEquals("1.64.0", manifest.playwrightVersion());
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
        assertEquals("sha256:564f74fa3c46cb83c6252145cf1569c95236c8c404d6a5a80cc736cf32265f0c",
                artifacts.stream().filter(artifact -> artifact.name().equals("firefox")).findFirst().orElseThrow()
                        .checksum());
        assertEquals("sha256:63b31a5d68d908d9aee2e445b88edc4c1e1d1eba488f5bd7e35668f91315b968",
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
                        "220021ecfdad08c116fc71a153e65e3861ba841c531afc4f7acc7bd5d44b41db", "bad")));
    }
}
