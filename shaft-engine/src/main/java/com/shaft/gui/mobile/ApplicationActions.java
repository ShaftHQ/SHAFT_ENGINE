package com.shaft.gui.mobile;

import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobileApplicationActionsContract;
import com.shaft.gui.driver.MobileApplicationState;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.tools.io.internal.TraceEventRecorder;
import io.appium.java_client.InteractsWithApps;
import io.appium.java_client.appmanagement.ApplicationState;
import io.appium.java_client.windows.WindowsDriver;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

/** Appium-backed mobile application lifecycle actions. */
public final class ApplicationActions implements MobileApplicationActionsContract {
    private final MobileActions mobile;

    ApplicationActions(MobileActions mobile) {
        this.mobile = Objects.requireNonNull(mobile, "mobile");
    }

    @Override
    public ApplicationActions install(String appPathOrUrl) {
        performSensitive("install", "<app-source>", appPathOrUrl, () -> {
            String source = requireText(appPathOrUrl, "app path or URL");
            appDriver().installApp(source);
        });
        return this;
    }

    @Override
    public ApplicationActions install(Path appPath) {
        String submittedPath = appPath == null ? "" : appPath.toString();
        performSensitive("install", "<app-source>", submittedPath, () -> {
            Path source = Objects.requireNonNull(appPath, "appPath").toAbsolutePath().normalize();
            FailureTraceReporter.registerSensitiveSourceValue(source.toString());
            if (!Files.isRegularFile(source)) {
                throw new IllegalArgumentException("The local app path must identify an existing regular file: " + source);
            }
            appDriver().installApp(source.toString());
        });
        return this;
    }

    @Override
    public boolean isInstalled(String appId) {
        return query("is-installed", appLocator(appId), () -> {
            String identifier = requireAppId(appId);
            return appDriver().isAppInstalled(identifier);
        });
    }

    @Override
    public MobileApplicationState state(String appId) {
        return query("state", appLocator(appId), () -> {
            String identifier = requireAppId(appId);
            ApplicationState state = appDriver().queryAppState(identifier);
            if (state == null) {
                throw new IllegalStateException("The Appium provider returned no application state.");
            }
            return MobileApplicationState.valueOf(state.name());
        });
    }

    @Override
    public ApplicationActions activate(String appId) {
        perform("activate", appLocator(appId), () -> {
            String identifier = requireAppId(appId);
            appDriver().activateApp(identifier);
        });
        return this;
    }

    @Override
    public boolean terminate(String appId) {
        return query("terminate", appLocator(appId), () -> {
            String identifier = requireAppId(appId);
            return appDriver().terminateApp(identifier);
        });
    }

    @Override
    public boolean remove(String appId) {
        return query("remove", appLocator(appId), () -> {
            String identifier = requireAppId(appId);
            return appDriver().removeApp(identifier);
        });
    }

    @Override
    public ApplicationActions background(Duration duration) {
        perform("background", duration == null ? "<duration>" : duration.toString(), () -> {
            Duration backgroundDuration = Objects.requireNonNull(duration, "duration");
            if (backgroundDuration.isNegative()) {
                throw new IllegalArgumentException("Background duration must not be negative.");
            }
            appDriver().runAppInBackground(backgroundDuration);
        });
        return this;
    }

    @Override
    public ApplicationActions launchConfiguredApp() {
        perform("launch-configured", "<configured-app>", () -> windowsDriver().launchApp());
        return this;
    }

    @Override
    public ApplicationActions closeConfiguredApp() {
        perform("close-configured", "<configured-app>", () -> windowsDriver().closeApp());
        return this;
    }

    @Override
    public MobileActionsContract and() {
        return mobile;
    }

    private InteractsWithApps appDriver() {
        if (mobile.driver() instanceof InteractsWithApps apps) {
            return apps;
        }
        throw new UnsupportedOperationException(
                "This live Appium session does not support application lifecycle commands.");
    }

    private WindowsDriver windowsDriver() {
        if (mobile.driver() instanceof WindowsDriver windows) {
            return windows;
        }
        throw new UnsupportedOperationException(
                "Configured-app launch and close are available only for a live Windows Appium session.");
    }

    private void perform(String operation, String locator, Runnable action) {
        query(operation, locator, () -> {
            action.run();
            return null;
        });
    }

    private void performSensitive(String operation, String locator, String sensitiveValue, Runnable action) {
        querySensitive(operation, locator, sensitiveValue, () -> {
            action.run();
            return null;
        });
    }

    private <T> T query(String operation, String locator, Supplier<T> action) {
        return querySensitive(operation, locator, null, action);
    }

    private <T> T querySensitive(String operation, String locator, String sensitiveValue, Supplier<T> action) {
        TraceEventRecorder.Event event = TraceEventRecorder.start("mobile/app", operation, locator, mobile.traceDriver());
        FailureTraceReporter.registerSensitiveSourceValue(sensitiveValue);
        try {
            T result = action.get();
            TraceEventRecorder.finish(event, "passed", "Mobile app action completed.", null,
                    result == null ? Map.of() : Map.of("result", String.valueOf(result)), List.of());
            return result;
        } catch (RuntimeException exception) {
            if (sensitiveValue != null && !sensitiveValue.isEmpty()) {
                FailureTraceReporter.registerSensitiveThrowable(exception);
                FailureTraceReporter.registerSensitiveValue(sensitiveValue);
            }
            TraceEventRecorder.finish(event, "failed", "Mobile app action failed.", exception, Map.of(), List.of());
            throw exception;
        }
    }

    private static String requireAppId(String appId) {
        return requireText(appId, "app package or bundle ID");
    }

    private static String appLocator(String appId) {
        return appId == null || appId.isBlank() ? "<app-id>" : appId;
    }

    private static String requireText(String value, String label) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("The " + label + " must not be blank.");
        }
        return value;
    }
}
