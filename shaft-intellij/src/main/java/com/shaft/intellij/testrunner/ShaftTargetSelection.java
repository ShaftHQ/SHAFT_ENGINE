package com.shaft.intellij.testrunner;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * Lean target picker model for run overrides (W2.7 / #7591): Platform × Execution × Device/OS × App.
 * Invalid combinations are rejected before launch; project files stay untouched (VM args only).
 */
public final class ShaftTargetSelection {
    public static final String INHERIT = ShaftRunOverridesLabels.PROJECT_DEFAULT;

    public enum Platform {
        INHERIT_PLATFORM(INHERIT),
        WEB("Web"),
        ANDROID("Android"),
        IOS("iOS");

        private final String label;

        Platform(String label) {
            this.label = label;
        }

        public String label() {
            return label;
        }

        @Override
        public String toString() {
            return label;
        }
    }

    public enum Execution {
        INHERIT_EXECUTION(INHERIT),
        LOCAL("Local"),
        BROWSERSTACK("BrowserStack"),
        LAMBDATEST("LambdaTest"),
        SAUCELABS("SauceLabs");

        private final String label;

        Execution(String label) {
            this.label = label;
        }

        public String label() {
            return label;
        }

        @Override
        public String toString() {
            return label;
        }
    }

    private Platform platform = Platform.INHERIT_PLATFORM;
    private Execution execution = Execution.INHERIT_EXECUTION;
    private String deviceOs = "";
    private String appAlias = "";
    private int retryCount = 0;

    public Platform getPlatform() {
        return platform;
    }

    public void setPlatform(@Nullable Platform platform) {
        this.platform = platform == null ? Platform.INHERIT_PLATFORM : platform;
    }

    public Execution getExecution() {
        return execution;
    }

    public void setExecution(@Nullable Execution execution) {
        this.execution = execution == null ? Execution.INHERIT_EXECUTION : execution;
    }

    public String getDeviceOs() {
        return deviceOs;
    }

    public void setDeviceOs(@Nullable String deviceOs) {
        this.deviceOs = deviceOs == null ? "" : deviceOs.trim();
    }

    public String getAppAlias() {
        return appAlias;
    }

    public void setAppAlias(@Nullable String appAlias) {
        this.appAlias = appAlias == null ? "" : appAlias.trim();
    }

    public int getRetryCount() {
        return retryCount;
    }

    public void setRetryCount(int retryCount) {
        this.retryCount = Math.max(0, retryCount);
    }

    /**
     * @return human-readable blockers; empty when the selection may launch
     */
    public @NotNull List<String> validationErrors() {
        List<String> errors = new ArrayList<>();
        addWebPlatformErrors(errors);
        addMobilePlatformErrors(errors);
        if (retryCount < 0) {
            errors.add("Retry count cannot be negative.");
        }
        return errors;
    }

    private void addWebPlatformErrors(@NotNull List<String> errors) {
        if (platform != Platform.WEB) {
            return;
        }
        if (!deviceOs.isBlank() && looksLikeMobileOs(deviceOs)) {
            errors.add("Web platform cannot use a mobile device/OS (" + deviceOs + ").");
        }
        if (!appAlias.isBlank()) {
            errors.add("Web platform does not use an App alias; clear App or switch to Android/iOS.");
        }
    }

    private void addMobilePlatformErrors(@NotNull List<String> errors) {
        boolean mobile = platform == Platform.ANDROID || platform == Platform.IOS;
        if (!mobile) {
            return;
        }
        boolean cloud = execution == Execution.BROWSERSTACK
                || execution == Execution.LAMBDATEST
                || execution == Execution.SAUCELABS;
        if (execution == Execution.LOCAL && deviceOs.isBlank()) {
            errors.add("Local " + platform.label() + " runs need a Device/OS (for example ANDROID or iOS).");
        }
        if (cloud && appAlias.isBlank()) {
            errors.add("Cloud " + platform.label() + " runs need an App alias from the registry (or a temporary id).");
        }
    }

    public boolean isValid() {
        return validationErrors().isEmpty();
    }

    /**
     * Emits {@code -D} tokens that do not touch project files. Blank/inherit fields are omitted.
     */
    public @NotNull List<String> toVmArgs() {
        List<String> args = new ArrayList<>();
        if (platform == Platform.ANDROID) {
            args.add("-DtargetOperatingSystem=ANDROID");
            args.add("-Dmobile_automationName=UIAutomator2");
        } else if (platform == Platform.IOS) {
            args.add("-DtargetOperatingSystem=IOS");
            args.add("-Dmobile_automationName=XCUITest");
        }
        if (execution == Execution.LOCAL) {
            if (platform == Platform.ANDROID || platform == Platform.IOS) {
                args.add("-DexecutionAddress=localhost:4723");
            } else if (platform == Platform.WEB) {
                args.add("-DexecutionAddress=local");
            }
        } else if (execution != Execution.INHERIT_EXECUTION) {
            args.add("-DexecutionAddress=" + execution.name().toLowerCase(Locale.ROOT));
        }
        if (!deviceOs.isBlank()) {
            args.add("-DtargetOperatingSystem=" + deviceOs);
        }
        if (!appAlias.isBlank()) {
            args.add("-Dmobile_app=" + appAlias);
        }
        if (retryCount > 0) {
            args.add("-DretryCount=" + retryCount);
        }
        return args;
    }

    private static boolean looksLikeMobileOs(String value) {
        String v = value.toLowerCase(Locale.ROOT);
        return v.contains("android") || v.contains("ios") || v.contains("iphone") || v.contains("ipad");
    }
}
