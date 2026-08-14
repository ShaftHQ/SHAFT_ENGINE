package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Map;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/** Exact Playwright browser host identity supported by this release. */
enum PlaywrightHostPlatform {
    WIN64("win64", SetupPlatform.WINDOWS, Set.of(SetupArchitecture.X64, SetupArchitecture.ARM64),
            Set.of("chromium", "chromium-headless-shell", "firefox", "webkit", "ffmpeg", "winldd")),
    UBUNTU_24_04_X64("ubuntu24.04-x64", SetupPlatform.LINUX, Set.of(SetupArchitecture.X64),
            Set.of("chromium", "chromium-headless-shell", "firefox", "webkit", "ffmpeg")),
    MAC15("mac15", SetupPlatform.MACOS, Set.of(SetupArchitecture.X64),
            Set.of("chromium", "chromium-headless-shell", "firefox", "webkit", "ffmpeg")),
    MAC15_ARM64("mac15-arm64", SetupPlatform.MACOS, Set.of(SetupArchitecture.ARM64),
            Set.of("chromium", "chromium-headless-shell", "firefox", "webkit", "ffmpeg"));

    private final String token;
    private final SetupPlatform platform;
    private final Set<SetupArchitecture> architectures;
    private final Set<String> requiredArtifacts;

    PlaywrightHostPlatform(String token, SetupPlatform platform, Set<SetupArchitecture> architectures,
                           Set<String> requiredArtifacts) {
        this.token = token;
        this.platform = platform;
        this.architectures = Set.copyOf(architectures);
        this.requiredArtifacts = Set.copyOf(requiredArtifacts);
    }

    String token() { return token; }
    SetupPlatform platform() { return platform; }
    Set<String> requiredArtifacts() { return requiredArtifacts; }
    boolean requiresExecutablePermission() { return this != WIN64; }

    List<String> requiredPaths(SetupTarget target) {
        return switch (this) {
            case WIN64 -> switch (target) {
                case PLAYWRIGHT_CHROMIUM -> List.of("chromium-1234/INSTALLATION_COMPLETE",
                        "chromium-1234/chrome-win64/chrome.exe",
                        "chromium_headless_shell-1234/INSTALLATION_COMPLETE",
                        "chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe",
                        "winldd-1007/INSTALLATION_COMPLETE", "winldd-1007/PrintDeps.exe");
                case PLAYWRIGHT_FIREFOX -> List.of("firefox-1538/INSTALLATION_COMPLETE",
                        "firefox-1538/firefox/firefox.exe");
                case PLAYWRIGHT_WEBKIT -> List.of("webkit-2336/INSTALLATION_COMPLETE",
                        "webkit-2336/Playwright.exe");
                case FFMPEG -> List.of("ffmpeg-1011/INSTALLATION_COMPLETE", "ffmpeg-1011/ffmpeg-win64.exe");
                default -> List.of();
            };
            case UBUNTU_24_04_X64 -> switch (target) {
                case PLAYWRIGHT_CHROMIUM -> List.of("chromium-1234/INSTALLATION_COMPLETE",
                        "chromium-1234/chrome-linux64/chrome",
                        "chromium_headless_shell-1234/INSTALLATION_COMPLETE",
                        "chromium_headless_shell-1234/chrome-headless-shell-linux64/chrome-headless-shell");
                case PLAYWRIGHT_FIREFOX -> List.of("firefox-1538/INSTALLATION_COMPLETE",
                        "firefox-1538/firefox/firefox");
                case PLAYWRIGHT_WEBKIT -> List.of("webkit-2336/INSTALLATION_COMPLETE", "webkit-2336/pw_run.sh");
                case FFMPEG -> List.of("ffmpeg-1011/INSTALLATION_COMPLETE", "ffmpeg-1011/ffmpeg-linux");
                default -> List.of();
            };
            case MAC15, MAC15_ARM64 -> switch (target) {
                case PLAYWRIGHT_CHROMIUM -> List.of("chromium-1234/INSTALLATION_COMPLETE",
                        "chromium-1234/" + macArchitectureDirectory("chrome")
                                + "/Google Chrome for Testing.app/Contents/MacOS/Google Chrome for Testing",
                        "chromium_headless_shell-1234/INSTALLATION_COMPLETE",
                        "chromium_headless_shell-1234/" + macArchitectureDirectory("chrome-headless-shell")
                                + "/chrome-headless-shell");
                case PLAYWRIGHT_FIREFOX -> List.of("firefox-1538/INSTALLATION_COMPLETE",
                        "firefox-1538/firefox/Nightly.app/Contents/MacOS/firefox");
                case PLAYWRIGHT_WEBKIT -> List.of("webkit-2336/INSTALLATION_COMPLETE", "webkit-2336/pw_run.sh");
                case FFMPEG -> List.of("ffmpeg-1011/INSTALLATION_COMPLETE", "ffmpeg-1011/ffmpeg-mac");
                default -> List.of();
            };
        };
    }

    private String macArchitectureDirectory(String prefix) {
        return prefix + (this == MAC15_ARM64 ? "-mac-arm64" : "-mac-x64");
    }

    static PlaywrightHostPlatform current(SetupPlatform platform, SetupArchitecture architecture) {
        if (platform == SetupPlatform.WINDOWS) return resolve(platform, architecture, "");
        if (platform == SetupPlatform.MACOS) {
            return resolve(platform, architecture, System.getProperty("os.version", ""));
        }
        try {
            return resolve(platform, architecture, Files.readString(Path.of("/etc/os-release")));
        } catch (IOException failure) {
            throw new IllegalArgumentException("Unable to identify the Linux host for Playwright setup.", failure);
        }
    }

    static PlaywrightHostPlatform resolve(SetupPlatform platform, SetupArchitecture architecture,
                                          String osRelease) {
        if (platform == SetupPlatform.WINDOWS
                && WIN64.architectures.contains(architecture)) return WIN64;
        if (platform == SetupPlatform.LINUX && architecture == SetupArchitecture.X64) {
            Map<String, String> values = Arrays.stream(osRelease.split("\\R"))
                    .map(String::trim)
                    .filter(line -> !line.isEmpty() && !line.startsWith("#") && line.contains("="))
                    .map(line -> line.split("=", 2))
                    .collect(Collectors.toMap(parts -> parts[0], parts -> unquote(parts[1]), (left, right) -> right));
            if ("ubuntu".equals(values.get("ID")) && "24.04".equals(values.get("VERSION_ID"))) {
                return UBUNTU_24_04_X64;
            }
        }
        if (platform == SetupPlatform.MACOS && osRelease.matches("15(?:\\..*)?")) {
            return architecture == SetupArchitecture.X64 ? MAC15 : MAC15_ARM64;
        }
        throw new IllegalArgumentException("Unsupported Playwright host: " + platform + '-' + architecture);
    }

    private static String unquote(String value) {
        String trimmed = value.trim();
        if (trimmed.length() >= 2 && ((trimmed.startsWith("\"") && trimmed.endsWith("\""))
                || (trimmed.startsWith("'") && trimmed.endsWith("'")))) {
            return trimmed.substring(1, trimmed.length() - 1);
        }
        return trimmed;
    }
}
