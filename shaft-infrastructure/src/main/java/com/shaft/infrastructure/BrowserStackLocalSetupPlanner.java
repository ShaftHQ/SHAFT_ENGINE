package com.shaft.infrastructure;

import java.net.URI;
import java.util.List;
import java.util.Set;

/** Release-coupled plans for a pinned BrowserStack Local binary. */
final class BrowserStackLocalSetupPlanner {
    static final String VERSION = "8.9";
    private static final String BASE =
            "https://bstack-local-prod.s3.amazonaws.com/binaries/release/v8.9/";

    private BrowserStackLocalSetupPlanner() {
        throw new IllegalStateException("Utility class");
    }

    static SetupPlan plan(SetupPlatform platform, SetupArchitecture architecture, SetupMode mode) {
        Asset asset = asset(platform, architecture);
        SetupActionKind kind = mode == SetupMode.EXTERNAL ? SetupActionKind.DIAGNOSE : SetupActionKind.INSTALL;
        return SetupPlan.create(SetupProfile.BROWSERSTACK_LOCAL, platform, architecture, mode, List.of(
                new SetupAction(SetupTarget.BROWSERSTACK_LOCAL, kind, VERSION, asset.source(),
                        "sha256:" + asset.sha256(), asset.bytes(), false, Set.of())));
    }

    static Asset asset(SetupPlatform platform, SetupArchitecture architecture) {
        if (platform == SetupPlatform.LINUX && architecture == SetupArchitecture.ARM64) {
            throw new IllegalArgumentException(
                    "BrowserStack Local v8.9 has no versioned Linux ARM64 archive; use x64 or a later pin.");
        }
        return switch (platform) {
            case WINDOWS -> new Asset(
                    URI.create(BASE + "BrowserStackLocal-win32.zip"),
                    "50af684795f0b3d6a1e7c429be45aea9d5828ec68890f49b4770bcc5bb73581f",
                    9_583_769, "BrowserStackLocal.exe");
            case LINUX -> new Asset(
                    URI.create(BASE + "BrowserStackLocal-linux-x64.zip"),
                    "6f10351faef2af198fb8188c5ce9b4ec449cf9ca5f119f0d999b5de2aa03166d",
                    12_300_194, "BrowserStackLocal");
            case MACOS -> new Asset(
                    URI.create(BASE + "BrowserStackLocal-darwin-x64.zip"),
                    "4e0177235b549afe1f9bbe5b2b797d884f2b96806049fa2c089377bea3cf2fed",
                    10_284_773, "BrowserStackLocal");
        };
    }

    record Asset(URI source, String sha256, long bytes, String executableName) { }
}
