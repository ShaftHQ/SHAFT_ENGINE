package com.shaft.validation.internal;

import com.shaft.gui.driver.MobileApplicationState;
import com.shaft.gui.driver.MobileAssertions;
import com.shaft.gui.driver.MobileBatteryInfo;
import com.shaft.gui.driver.MobileEvidenceBundle;
import com.shaft.gui.mobile.internal.MobileLogSource;
import com.shaft.gui.mobile.internal.MobilePerformanceState;
import com.shaft.gui.mobile.internal.MobileRecordingState;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.validation.ValidationEnums;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.HasDeviceTime;
import io.appium.java_client.InteractsWithApps;
import io.appium.java_client.LocksDevice;
import io.appium.java_client.appmanagement.ApplicationState;
import io.appium.java_client.battery.BatteryInfo;
import io.appium.java_client.battery.HasBattery;
import io.appium.java_client.remote.SupportsContextSwitching;
import io.appium.java_client.remote.SupportsRotation;
import org.openqa.selenium.WebDriver;

import java.util.Objects;
import java.util.function.Supplier;

/** WebDriver/Appium implementation of focused mobile-session validations. */
public final class WebDriverMobileValidationsBuilder implements MobileAssertions {
    protected final ValidationEnums.ValidationCategory validationCategory;
    protected final WebDriver driver;
    private final String reportMessagePrefix;

    public WebDriverMobileValidationsBuilder(ValidationEnums.ValidationCategory validationCategory,
                                             WebDriver driver,
                                             StringBuilder reportMessageBuilder) {
        this.validationCategory = Objects.requireNonNull(validationCategory, "validationCategory");
        this.driver = Objects.requireNonNull(driver, "driver");
        this.reportMessagePrefix = Objects.requireNonNull(reportMessageBuilder, "reportMessageBuilder").toString();
    }

    @Override
    public NativeValidationsBuilder currentContextValue() {
        return value("current context", () -> {
            String context = contexts().getContext();
            if (context == null) {
                throw new IllegalStateException("The Appium provider returned no current context.");
            }
            return context;
        });
    }

    @Override
    public NativeValidationsBuilder contextCountValue() {
        return value("context count", () -> {
            var handles = contexts().getContextHandles();
            if (handles == null) {
                throw new IllegalStateException("The Appium provider returned no context handles.");
            }
            return handles.size();
        });
    }

    @Override
    public NativeValidationsBuilder appInstalledValue(String appId) {
        return value("application installed state", () -> {
            String applicationId = requireAppId(appId);
            return apps().isAppInstalled(applicationId);
        });
    }

    @Override
    public NativeValidationsBuilder appStateValue(String appId) {
        return value("application lifecycle state", () -> {
            String applicationId = requireAppId(appId);
            ApplicationState state = apps().queryAppState(applicationId);
            if (state == null) {
                throw new IllegalStateException("The Appium provider returned no application state.");
            }
            return MobileApplicationState.valueOf(state.name());
        });
    }

    @Override
    public NativeValidationsBuilder deviceLockedValue() {
        return value("device locked state", () -> locks().isDeviceLocked());
    }

    @Override
    public NativeValidationsBuilder deviceOrientationValue() {
        return value("device orientation", () -> {
            var orientation = rotation().getOrientation();
            if (orientation == null) {
                throw new IllegalStateException("The Appium provider returned no device orientation.");
            }
            return orientation;
        });
    }

    @Override
    public NativeValidationsBuilder deviceTimeValue() {
        return value("device time", () -> Objects.requireNonNull(deviceTime().getDeviceTime(),
                "The Appium provider returned no device time."));
    }

    @Override
    public NativeValidationsBuilder batteryValue() {
        return value("battery reading", () -> {
            AppiumDriver liveDriver = liveAppiumDriver();
            if (!(liveDriver instanceof HasBattery<?> provider)) {
                throw unsupported("battery information");
            }
            BatteryInfo battery = Objects.requireNonNull(provider.getBatteryInfo(),
                    "The Appium provider returned no battery information.");
            Object state = battery.getState();
            return new MobileBatteryInfo(battery.getLevel(), state == null ? null : String.valueOf(state));
        });
    }

    @Override
    public NativeValidationsBuilder logMessageCountValue() {
        return value("device log message count", () -> logSnapshot().messages().size());
    }

    @Override
    public NativeValidationsBuilder logErrorCountValue() {
        return value("device log error count", () -> logSnapshot().errors().size());
    }

    @Override
    public NativeValidationsBuilder performanceSampleCountValue() {
        return value("performance sample count", () -> MobilePerformanceState
                .historyIfPresent(liveAppiumDriver())
                .orElseThrow(() -> unsupported("retained performance history"))
                .size());
    }

    @Override
    public NativeValidationsBuilder recordingInProgressValue() {
        return value("recording in-progress state", () -> recordingSnapshot().recordingInProgress());
    }

    @Override
    public NativeValidationsBuilder retainedRecordingAvailableValue() {
        return value("retained recording availability", () -> recordingSnapshot().savedRecording().isPresent());
    }

    @Override
    public NativeValidationsBuilder retainedRecordingSizeValue() {
        return value("retained recording byte size", () -> recordingSnapshot().savedRecording()
                .orElseThrow(() -> unsupported("a retained saved recording"))
                .sizeBytes());
    }

    @Override
    public NativeValidationsBuilder evidenceArtifactCountValue(MobileEvidenceBundle bundle) {
        MobileEvidenceBundle required = Objects.requireNonNull(bundle, "evidence bundle");
        return value("evidence artifact count", () -> required.artifacts().size());
    }

    @Override
    public NativeValidationsBuilder evidenceOmissionCountValue(MobileEvidenceBundle bundle) {
        MobileEvidenceBundle required = Objects.requireNonNull(bundle, "evidence bundle");
        return value("evidence omission count", () -> required.omissions().size());
    }

    private NativeValidationsBuilder value(String name, Supplier<Object> reader) {
        return new NativeValidationsBuilder(validationCategory, driver, reader, name,
                new StringBuilder(reportMessagePrefix).append(name).append(' '));
    }

    private SupportsContextSwitching contexts() {
        if (liveAppiumDriver() instanceof SupportsContextSwitching provider) {
            return provider;
        }
        throw unsupported("native or web context inspection");
    }

    private MobileLogSource.Snapshot logSnapshot() {
        return MobileLogSource.snapshotIfPresent(liveAppiumDriver())
                .orElseThrow(() -> unsupported("buffered device-log state"));
    }

    private MobileRecordingState.Snapshot recordingSnapshot() {
        return MobileRecordingState.snapshotIfPresent(liveAppiumDriver())
                .orElseThrow(() -> unsupported("screen-recording state"));
    }

    private InteractsWithApps apps() {
        if (liveAppiumDriver() instanceof InteractsWithApps provider) {
            return provider;
        }
        throw unsupported("application state inspection");
    }

    private LocksDevice locks() {
        if (liveAppiumDriver() instanceof LocksDevice provider) {
            return provider;
        }
        throw unsupported("device lock inspection");
    }

    private SupportsRotation rotation() {
        if (liveAppiumDriver() instanceof SupportsRotation provider) {
            return provider;
        }
        throw unsupported("screen orientation inspection");
    }

    private HasDeviceTime deviceTime() {
        if (liveAppiumDriver() instanceof HasDeviceTime provider) {
            return provider;
        }
        throw unsupported("device time inspection");
    }

    private AppiumDriver liveAppiumDriver() {
        if (!(driver instanceof AppiumDriver appiumDriver) || appiumDriver.getSessionId() == null) {
            throw new UnsupportedOperationException("Mobile validations require a live Appium session.");
        }
        return appiumDriver;
    }

    private static String requireAppId(String appId) {
        if (appId != null) {
            FailureTraceReporter.registerSensitiveSourceValue(appId);
            FailureTraceReporter.registerSensitiveValue(appId);
        }
        if (appId == null) {
            throw new NullPointerException("The application identifier must not be null.");
        }
        if (appId.isBlank()) {
            throw new IllegalArgumentException("The application identifier must not be blank.");
        }
        return appId;
    }

    private static UnsupportedOperationException unsupported(String feature) {
        return new UnsupportedOperationException("The live Appium session does not support " + feature + ".");
    }
}
