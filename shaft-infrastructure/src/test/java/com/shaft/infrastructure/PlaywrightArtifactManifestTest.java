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

        assertEquals("1.63.0", manifest.playwrightVersion());
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
        assertEquals("sha256:b0905e84427cc162b9a6e4392be14e5e54e0ade911c83639962c8078b273565e",
                artifacts.stream().filter(artifact -> artifact.name().equals("firefox")).findFirst().orElseThrow()
                        .checksum());
        assertEquals("sha256:8c129d989a1c48d826ca11b45acbba919039de811623dc3819ccbd95b69eeb62",
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
                        "415968b02065d4a9e2c10b85f0ae9f489b8fba500e94d9d0a7b7c4852a7234c1", "bad")));
    }
}
