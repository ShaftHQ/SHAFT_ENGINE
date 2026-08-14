package com.shaft.infrastructure;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.List;
import java.util.Set;

/** Release-coupled planner for the managed Playwright browser payloads. */
public final class PlaywrightSetupPlanner {
    public static final String PLAYWRIGHT_VERSION = "1.62.0";
    public static final String CHROMIUM_REVISION = "1234";
    public static final String FIREFOX_REVISION = "1538";
    public static final String WEBKIT_REVISION = "2336";
    public static final String FFMPEG_REVISION = "1011";

    private PlaywrightSetupPlanner() { }

    /**
     * Creates the exact Playwright browser plan shipped with this release.
     *
     * @param platform target operating system
     * @param architecture target CPU architecture
     * @param mode requested ownership mode
     * @return immutable release-pinned setup plan
     */
    public static SetupPlan plan(SetupPlatform platform, SetupArchitecture architecture, SetupMode mode) {
        return plan(PlaywrightHostPlatform.current(platform, architecture), architecture, mode);
    }

    static SetupPlan plan(PlaywrightHostPlatform host, SetupMode mode) {
        SetupArchitecture architecture = switch (host) {
            case UBUNTU_24_04_X64, MAC15 -> SetupArchitecture.X64;
            case MAC15_ARM64 -> SetupArchitecture.ARM64;
            case WIN64 -> SetupArchitecture.current();
        };
        return plan(host, architecture, mode);
    }

    static SetupPlan plan(PlaywrightHostPlatform host, SetupArchitecture architecture, SetupMode mode) {
        SetupActionKind kind = mode == SetupMode.EXTERNAL ? SetupActionKind.DIAGNOSE : SetupActionKind.INSTALL;
        String manifestDigest = manifestDigest();
        SetupPlatform platform = host.platform();
        SetupAction node = ReportingSetupPlanner.plan(platform, architecture, mode).actions().getFirst();
        List<PlaywrightArtifactManifest.Artifact> artifacts = PlaywrightArtifactManifest.load()
                .requirePlatform(host.token());
        PlaywrightArtifactManifest.Artifact chromiumArtifact = artifact(artifacts, "chromium");
        PlaywrightArtifactManifest.Artifact firefoxArtifact = artifact(artifacts, "firefox");
        PlaywrightArtifactManifest.Artifact webkitArtifact = artifact(artifacts, "webkit");
        PlaywrightArtifactManifest.Artifact ffmpegArtifact = artifact(artifacts, "ffmpeg");
        SetupAction chromium = action(SetupTarget.PLAYWRIGHT_CHROMIUM, kind,
                "chromium-" + CHROMIUM_REVISION,
                chromiumArtifact.source().toString(), chromiumArtifact.checksum(), manifestDigest);
        SetupAction firefox = action(SetupTarget.PLAYWRIGHT_FIREFOX, kind,
                "firefox-" + FIREFOX_REVISION,
                firefoxArtifact.source().toString(), firefoxArtifact.checksum(), manifestDigest);
        SetupAction webkit = action(SetupTarget.PLAYWRIGHT_WEBKIT, kind,
                "webkit-" + WEBKIT_REVISION,
                webkitArtifact.source().toString(), webkitArtifact.checksum(), manifestDigest);
        SetupAction ffmpeg = action(SetupTarget.FFMPEG, kind, "ffmpeg-" + FFMPEG_REVISION,
                ffmpegArtifact.source().toString(), ffmpegArtifact.checksum(), manifestDigest);
        return SetupPlan.create(SetupProfile.PLAYWRIGHT, platform, architecture, mode,
                List.of(node, chromium, firefox, webkit, ffmpeg));
    }

    private static SetupAction action(SetupTarget target, SetupActionKind kind, String version,
                                      String source, String checksum, String manifestDigest) {
        return new SetupAction(target, kind, PLAYWRIGHT_VERSION + ':' + version, URI.create(source),
                checksum.startsWith("sha256:") ? checksum : "sha256:" + checksum,
                manifestDigest, false, Set.of());
    }

    private static PlaywrightArtifactManifest.Artifact artifact(
            List<PlaywrightArtifactManifest.Artifact> artifacts, String name) {
        return artifacts.stream().filter(candidate -> name.equals(candidate.name())).findFirst()
                .orElseThrow(() -> new IllegalStateException("Missing Playwright artifact: " + name));
    }

    private static String manifestDigest() {
        try (var input = PlaywrightSetupPlanner.class.getResourceAsStream(
                "/com/shaft/infrastructure/playwright/browser-artifacts.json")) {
            byte[] bytes = java.util.Objects.requireNonNull(input, "Playwright browser artifact manifest")
                    .readAllBytes();
            String canonical = new String(bytes, StandardCharsets.UTF_8)
                    .replace("\r\n", "\n").replace('\r', '\n');
            return "sha256:" + HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256")
                    .digest(canonical.getBytes(StandardCharsets.UTF_8)));
        } catch (java.io.IOException | NoSuchAlgorithmException failure) {
            throw new IllegalStateException("Unable to verify the Playwright browser artifact manifest.", failure);
        }
    }
}
