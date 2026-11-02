package com.shaft.infrastructure;

import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.DeserializationFeature;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/** Strict parser for the release-reviewed Playwright browser archive manifest. */
final class PlaywrightArtifactManifest {
    private static final JsonMapper JSON = JsonMapper.builder()
            .enable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES).build();
    private static final long MAXIMUM_ARTIFACT_SIZE = 512L * 1024 * 1024;
    private static final Set<String> APPROVED_HOSTS = Set.of(
            "cdn.playwright.dev", "playwright.download.prss.microsoft.com");
    private final String playwrightVersion;
    private final List<Platform> platforms;

    private PlaywrightArtifactManifest(Document document) {
        if (document.schemaVersion() != 1) {
            throw new IllegalArgumentException("Unsupported Playwright artifact manifest schema: "
                    + document.schemaVersion());
        }
        playwrightVersion = requireText(document.playwrightVersion(), "playwrightVersion");
        platforms = List.copyOf(Objects.requireNonNull(document.platforms(), "platforms"));
        if (platforms.isEmpty()) throw new IllegalArgumentException("Playwright artifact platforms must not be empty.");
        Set<String> hostPlatforms = new HashSet<>();
        for (Platform platform : platforms) {
            String hostPlatform = requireText(platform.hostPlatform(), "hostPlatform");
            if (!hostPlatforms.add(hostPlatform)) {
                throw new IllegalArgumentException("Duplicate Playwright host platform: " + hostPlatform);
            }
            validateArtifacts(platform.artifacts(), hostPlatform);
        }
    }

    static PlaywrightArtifactManifest load() {
        try (var input = PlaywrightArtifactManifest.class.getResourceAsStream(
                "/com/shaft/infrastructure/playwright/browser-artifacts.json")) {
            return parse(new String(Objects.requireNonNull(input, "Playwright browser artifact manifest")
                    .readAllBytes(), StandardCharsets.UTF_8));
        } catch (IOException failure) {
            throw new IllegalStateException("Unable to read the Playwright browser artifact manifest.", failure);
        }
    }

    static PlaywrightArtifactManifest parse(String json) {
        try {
            return new PlaywrightArtifactManifest(JSON.readValue(Objects.requireNonNull(json, "json"), Document.class));
        } catch (IllegalArgumentException failure) {
            throw failure;
        } catch (RuntimeException failure) {
            throw new IllegalArgumentException("Invalid Playwright browser artifact manifest.", failure);
        }
    }

    String playwrightVersion() {
        return playwrightVersion;
    }

    List<Artifact> requirePlatform(String hostPlatform) {
        return platforms.stream().filter(platform -> platform.hostPlatform().equals(hostPlatform))
                .findFirst().orElseThrow(() -> new IllegalArgumentException(
                        "Unsupported Playwright host platform: " + hostPlatform)).artifacts().stream()
                .map(ArtifactDocument::toArtifact).toList();
    }

    private static void validateArtifacts(List<ArtifactDocument> artifacts, String hostPlatform) {
        Objects.requireNonNull(artifacts, "artifacts");
        if (artifacts.isEmpty()) {
            throw new IllegalArgumentException("Playwright artifacts must not be empty for " + hostPlatform);
        }
        Set<String> names = new HashSet<>();
        for (ArtifactDocument artifact : artifacts) {
            String name = requireText(artifact.name(), "artifact name");
            if (!names.add(name)) throw new IllegalArgumentException("Duplicate Playwright artifact: " + name);
            requireText(artifact.revision(), "artifact revision");
            URI source = URI.create(requireText(artifact.url(), "artifact url"));
            if (!"https".equals(source.getScheme()) || source.getUserInfo() != null || source.getPort() != -1
                    || !APPROVED_HOSTS.contains(source.getHost())) {
                throw new IllegalArgumentException("Unapproved Playwright artifact source: " + source);
            }
            if (artifact.sha256() == null || !artifact.sha256().matches("[0-9a-f]{64}")) {
                throw new IllegalArgumentException("Invalid Playwright artifact SHA-256 for " + name);
            }
            if (artifact.size() <= 0 || artifact.size() > MAXIMUM_ARTIFACT_SIZE) {
                throw new IllegalArgumentException("Invalid Playwright artifact size for " + name);
            }
        }
    }

    private static String requireText(String value, String name) {
        if (value == null || value.isBlank()) throw new IllegalArgumentException(name + " must not be blank.");
        return value;
    }

    record Artifact(String name, String revision, URI source, String checksum, long size) { }

    record Document(int schemaVersion, String playwrightVersion, List<Platform> platforms) { }

    record Platform(String hostPlatform, List<ArtifactDocument> artifacts) { }

    record ArtifactDocument(String name, String revision, String url, String sha256, long size) {
        Artifact toArtifact() {
            return new Artifact(name, revision, URI.create(url), "sha256:" + sha256, size);
        }
    }
}
