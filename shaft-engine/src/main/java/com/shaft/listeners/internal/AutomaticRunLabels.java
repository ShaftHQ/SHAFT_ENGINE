package com.shaft.listeners.internal;

import com.shaft.properties.internal.Properties;
import io.qameta.allure.model.Label;
import io.qameta.allure.model.TestResult;

import java.util.List;
import java.util.Locale;

/**
 * Default Allure labels derived from the run configuration (#7053): target operating system ({@code os}),
 * software-under-test type ({@code sut}) and execution location ({@code location}). Each value is added as a
 * dedicated label and as a {@code key:value} tag so merged reports can filter and group runs. Labels a test
 * already set by hand are kept and not duplicated. Disable with {@code allure.automaticRunLabels=false}.
 */
public final class AutomaticRunLabels {
    private AutomaticRunLabels() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Applies the labels computed from the current SHAFT properties.
     *
     * @param result the Allure result about to be stopped
     */
    public static void apply(TestResult result) {
        if (!Properties.allure.automaticRunLabels()) {
            return;
        }
        String os = operatingSystem(Properties.platform.targetPlatform());
        boolean mobileBrowser = !Properties.mobile.browserName().isBlank();
        boolean nativeApp = !Properties.mobile.app().isBlank() || !Properties.mobile.appPackage().isBlank()
                || !Properties.mobile.bundleId().isBlank();
        apply(result, os, softwareUnderTest(os, mobileBrowser, nativeApp),
                executionLocation(Properties.platform.executionAddress(), os));
    }

    static void apply(TestResult result, String os, String sut, String location) {
        add(result, "os", os);
        add(result, "sut", sut);
        add(result, "location", location);
    }

    /**
     * Normalizes {@code targetOperatingSystem} to a display name.
     *
     * @param targetPlatform the raw property value
     * @return Android, iOS, Windows, macOS, Linux, or the trimmed raw value ({@code Unknown} when blank)
     */
    public static String operatingSystem(String targetPlatform) {
        String p = String.valueOf(targetPlatform).trim().toLowerCase(Locale.ROOT);
        if (p.contains("android")) {
            return "Android";
        }
        if (p.contains("ios")) {
            return "iOS";
        }
        if (p.contains("windows")) {
            return "Windows";
        }
        if (p.contains("mac")) {
            return "macOS";
        }
        if (p.contains("linux")) {
            return "Linux";
        }
        return p.isEmpty() || "null".equals(p) ? "Unknown" : String.valueOf(targetPlatform).trim();
    }

    /**
     * Classifies the software under test from the configured target.
     *
     * @param os            normalized operating system
     * @param mobileBrowser whether a mobile {@code browserName} is configured
     * @param nativeApp     whether a mobile app, package or bundle id is configured
     * @return MobileWeb, Native, or Web
     */
    public static String softwareUnderTest(String os, boolean mobileBrowser, boolean nativeApp) {
        boolean mobile = "Android".equals(os) || "iOS".equals(os);
        if (mobile && mobileBrowser && !nativeApp) {
            return "MobileWeb";
        }
        return mobile ? "Native" : "Web";
    }

    /**
     * Classifies where the test runs from {@code executionAddress}.
     *
     * @param executionAddress the raw property value
     * @param os               normalized operating system
     * @return BrowserStack, LambdaTest, Docker, LocalSimulator, LocalEmulator, Local, or Remote
     */
    public static String executionLocation(String executionAddress, String os) {
        String a = String.valueOf(executionAddress).trim().toLowerCase(Locale.ROOT);
        if (a.contains("browserstack")) {
            return "BrowserStack";
        }
        if (a.contains("lambdatest")) {
            return "LambdaTest";
        }
        if (a.equals("dockerized") || a.equals("testcontainers")) {
            return "Docker";
        }
        if (a.isEmpty() || a.equals("local") || a.contains("localhost") || a.contains("127.0.0.1")) {
            if ("iOS".equals(os)) {
                return "LocalSimulator";
            }
            return "Android".equals(os) ? "LocalEmulator" : "Local";
        }
        return "Remote";
    }

    private static void add(TestResult result, String key, String value) {
        List<Label> labels = result.getLabels();
        if (labels.stream().noneMatch(label -> key.equals(label.getName()))) {
            labels.add(new Label().setName(key).setValue(value));
        }
        String tag = key + ":" + value;
        if (labels.stream().noneMatch(label -> "tag".equals(label.getName()) && tag.equals(label.getValue()))) {
            labels.add(new Label().setName("tag").setValue(tag));
        }
    }
}
