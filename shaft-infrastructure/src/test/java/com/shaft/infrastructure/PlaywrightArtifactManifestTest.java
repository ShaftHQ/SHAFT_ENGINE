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

        assertEquals("1.65.1", manifest.playwrightVersion());
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
        assertEquals("sha256:189ce3e09c61776010ab8a7cbffaa016b8b1914d3c9b50cb5ab6d0c5db4c368f",
                artifacts.stream().filter(artifact -> artifact.name().equals("firefox")).findFirst().orElseThrow()
                        .checksum());
        assertEquals("sha256:4375ca56620d92f4bc8710b6f0cddc8afd2f50c4cca616a50355d63d715c7803",
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
                        "3e2514fddb71f6727e2079369e03a6a4e105adcb28f05a6bfd875b782bddc431", "bad")));
    }
}
