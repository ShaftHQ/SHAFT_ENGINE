package com.shaft.infrastructure;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/** Versioned built-in inventory of SHAFT setup targets and profiles. */
public record SetupCatalog(int schemaVersion, List<SetupTargetDefinition> targets,
                           List<SetupProfileDefinition> profiles) {
    public SetupCatalog {
        targets = List.copyOf(Objects.requireNonNull(targets, "targets"));
        profiles = List.copyOf(Objects.requireNonNull(profiles, "profiles"));
        if (schemaVersion < 1) {
            throw new IllegalArgumentException("Setup catalog schema version must be positive.");
        }
        Set<SetupTarget> declaredTargets = targets.stream()
                .map(SetupTargetDefinition::target)
                .collect(Collectors.toUnmodifiableSet());
        if (declaredTargets.size() != targets.size()) {
            throw new IllegalArgumentException("Setup catalog contains duplicate targets.");
        }
        Set<SetupProfile> declaredProfiles = profiles.stream()
                .map(SetupProfileDefinition::profile)
                .collect(Collectors.toUnmodifiableSet());
        if (declaredProfiles.size() != profiles.size()) {
            throw new IllegalArgumentException("Setup catalog contains duplicate profiles.");
        }
        if (profiles.stream().flatMap(profile -> profile.targets().stream())
                .anyMatch(target -> !declaredTargets.contains(target))) {
            throw new IllegalArgumentException("Setup profile references an undeclared target.");
        }
    }

    /** Loads the catalog shipped with this SHAFT release. */
    public static SetupCatalog builtIn() {
        List<SetupTargetDefinition> targets = List.of(
                target(SetupTarget.NODE, "Portable Node.js", "Shared local runtime for npm-backed SHAFT tools.", SetupCapability.INSTALLABLE),
                target(SetupTarget.ALLURE, "Allure Report", "Pinned Allure command-line report generator.", SetupCapability.INSTALLABLE),
                target(SetupTarget.PLAYWRIGHT_CHROMIUM, "Playwright Chromium", "Browser revision matching SHAFT's Playwright dependency.", SetupCapability.INSTALLABLE),
                target(SetupTarget.PLAYWRIGHT_FIREFOX, "Playwright Firefox", "Browser revision matching SHAFT's Playwright dependency.", SetupCapability.INSTALLABLE),
                target(SetupTarget.PLAYWRIGHT_WEBKIT, "Playwright WebKit", "Browser revision matching SHAFT's Playwright dependency.", SetupCapability.INSTALLABLE),
                target(SetupTarget.LIGHTHOUSE, "Lighthouse", "Pinned local Lighthouse and Puppeteer packages.", SetupCapability.INSTALLABLE),
                target(SetupTarget.APPIUM_SERVER, "Appium server", "Pinned local Appium server.", SetupCapability.INSTALLABLE, SetupCapability.STARTABLE),
                target(SetupTarget.APPIUM_INSPECTOR_PLUGIN, "Appium Inspector plugin", "Pinned Appium Inspector plugin for server-side inspection.", SetupCapability.INSTALLABLE),
                target(SetupTarget.APPIUM_UIAUTOMATOR2_DRIVER, "Appium UiAutomator2 driver", "Pinned Android automation driver.", SetupCapability.INSTALLABLE),
                target(SetupTarget.APPIUM_XCUITEST_DRIVER, "Appium XCUITest driver", "Pinned iOS automation driver.", SetupCapability.INSTALLABLE),
                target(SetupTarget.APPIUM_WINDOWS_DRIVER, "Appium Windows driver", "Pinned Windows automation driver.", SetupCapability.INSTALLABLE),
                target(SetupTarget.APPIUM_FLUTTER_DRIVER, "Appium Flutter driver", "Optional Flutter automation driver.", SetupCapability.INSTALLABLE),
                target(SetupTarget.ANDROID_SDK, "Android SDK", "Command-line tools, platform-tools, platforms, images, and build-tools.", SetupCapability.INSTALLABLE),
                target(SetupTarget.ANDROID_EMULATOR, "Android Emulator", "SHAFT-owned Android virtual device lifecycle.", SetupCapability.INSTALLABLE, SetupCapability.STARTABLE),
                target(SetupTarget.IOS_SIMULATOR, "iOS Simulator", "Lifecycle for a simulator supplied by Xcode.", SetupCapability.STARTABLE),
                target(SetupTarget.WINAPPDRIVER, "WinAppDriver", "Approved privileged Windows application driver setup.", SetupCapability.INSTALLABLE, SetupCapability.STARTABLE, SetupCapability.PRIVILEGED),
                target(SetupTarget.SELENIUM_GRID, "Selenium Grid", "Pinned SHAFT-owned local Grid containers.", SetupCapability.INSTALLABLE, SetupCapability.STARTABLE),
                target(SetupTarget.OCR_TESSDATA, "OCR language data", "Verified Tesseract language models.", SetupCapability.INSTALLABLE),
                target(SetupTarget.HEALENIUM, "Healenium", "Pinned local Healenium service containers.", SetupCapability.INSTALLABLE, SetupCapability.STARTABLE),
                target(SetupTarget.REPORT_PORTAL, "ReportPortal", "Explicit local-development ReportPortal containers.", SetupCapability.INSTALLABLE, SetupCapability.STARTABLE),
                target(SetupTarget.BROWSERSTACK_LOCAL, "BrowserStack Local", "Vendor-supported private-network tunnel lifecycle.", SetupCapability.INSTALLABLE, SetupCapability.STARTABLE),
                target(SetupTarget.AGENT_CLI, "Agent command-line tools", "Explicit user-cache installation of supported agent clients.", SetupCapability.INSTALLABLE),
                target(SetupTarget.OLLAMA, "Ollama", "Existing local Ollama runtime and approved model pulls.", SetupCapability.HOST_PREREQUISITE, SetupCapability.PREWARMABLE, SetupCapability.STARTABLE),
                target(SetupTarget.JAVA, "Java", "Host Java runtime required to run SHAFT.", SetupCapability.HOST_PREREQUISITE),
                target(SetupTarget.MAVEN, "Maven", "Host Maven runtime used for Java projects.", SetupCapability.HOST_PREREQUISITE),
                target(SetupTarget.PYTHON, "Python", "Host Python runtime used by supported installer and maintenance flows.", SetupCapability.HOST_PREREQUISITE),
                target(SetupTarget.SELENIUM_BROWSER, "Selenium browser", "Compatible browser and Selenium-managed driver cache.", SetupCapability.HOST_PREREQUISITE, SetupCapability.PREWARMABLE),
                target(SetupTarget.XCODE, "Xcode", "Apple host toolchain, runtimes, signing, and device trust.", SetupCapability.HOST_PREREQUISITE, SetupCapability.PRIVILEGED),
                target(SetupTarget.DOCKER, "Docker", "Host container engine and Compose plugin.", SetupCapability.HOST_PREREQUISITE),
                target(SetupTarget.LM_STUDIO, "LM Studio", "Existing loopback LM Studio service.", SetupCapability.HOST_PREREQUISITE),
                target(SetupTarget.FFMPEG, "FFmpeg", "Pinned media payload installed with Playwright or provided by the SHAFT video module.",
                        SetupCapability.INSTALLABLE, SetupCapability.PROVIDED),
                target(SetupTarget.OPENCV, "OpenCV", "Native visual payload provided by optional SHAFT modules.", SetupCapability.PROVIDED));
        List<SetupProfileDefinition> profiles = List.of(
                profile(SetupProfile.WEB_LOCAL, "Local web", SetupTarget.SELENIUM_BROWSER),
                profile(SetupProfile.PLAYWRIGHT, "Playwright", SetupTarget.NODE,
                        SetupTarget.PLAYWRIGHT_CHROMIUM, SetupTarget.PLAYWRIGHT_FIREFOX,
                        SetupTarget.PLAYWRIGHT_WEBKIT, SetupTarget.FFMPEG),
                profile(SetupProfile.LIGHTHOUSE, "Lighthouse", SetupTarget.NODE, SetupTarget.LIGHTHOUSE),
                profile(SetupProfile.MOBILE_ANDROID, "Android mobile", SetupTarget.NODE, SetupTarget.APPIUM_SERVER,
                        SetupTarget.APPIUM_INSPECTOR_PLUGIN, SetupTarget.APPIUM_UIAUTOMATOR2_DRIVER,
                        SetupTarget.ANDROID_SDK, SetupTarget.ANDROID_EMULATOR),
                profile(SetupProfile.MOBILE_IOS, "iOS mobile", SetupTarget.NODE, SetupTarget.APPIUM_SERVER,
                        SetupTarget.APPIUM_INSPECTOR_PLUGIN, SetupTarget.APPIUM_XCUITEST_DRIVER,
                        SetupTarget.XCODE, SetupTarget.IOS_SIMULATOR),
                profile(SetupProfile.MOBILE_WINDOWS, "Windows desktop", SetupTarget.NODE, SetupTarget.APPIUM_SERVER,
                        SetupTarget.APPIUM_INSPECTOR_PLUGIN, SetupTarget.APPIUM_WINDOWS_DRIVER,
                        SetupTarget.WINAPPDRIVER),
                profile(SetupProfile.SELENIUM_GRID, "Selenium Grid", SetupTarget.DOCKER, SetupTarget.SELENIUM_GRID),
                profile(SetupProfile.REPORTING, "Reporting", SetupTarget.NODE, SetupTarget.ALLURE),
                profile(SetupProfile.OCR, "OCR", SetupTarget.OCR_TESSDATA),
                profile(SetupProfile.HEALENIUM, "Healenium", SetupTarget.DOCKER, SetupTarget.HEALENIUM),
                profile(SetupProfile.REPORT_PORTAL, "ReportPortal", SetupTarget.DOCKER, SetupTarget.REPORT_PORTAL),
                profile(SetupProfile.BROWSERSTACK_LOCAL, "BrowserStack Local", SetupTarget.BROWSERSTACK_LOCAL),
                profile(SetupProfile.AGENT_TOOLS, "Agent tools", SetupTarget.JAVA, SetupTarget.MAVEN,
                        SetupTarget.PYTHON, SetupTarget.NODE, SetupTarget.AGENT_CLI),
                profile(SetupProfile.LOCAL_AI, "Local AI", SetupTarget.OLLAMA, SetupTarget.LM_STUDIO));
        return new SetupCatalog(1, targets, profiles);
    }

    private static SetupTargetDefinition target(SetupTarget target, String name, String description,
                                                 SetupCapability... capabilities) {
        return new SetupTargetDefinition(target, name, Set.of(capabilities), description);
    }

    private static SetupProfileDefinition profile(SetupProfile profile, String name, SetupTarget... targets) {
        return new SetupProfileDefinition(profile, name, List.of(targets));
    }
}
