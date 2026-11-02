package com.shaft.gui.mobile;

import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobileBatteryInfo;
import com.shaft.gui.driver.MobileClipboardActionsContract;
import com.shaft.gui.driver.MobileDeviceActionsContract;
import com.shaft.gui.driver.MobileKeyboardActionsContract;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.tools.io.internal.TraceEventRecorder;
import io.appium.java_client.HasDeviceTime;
import io.appium.java_client.HasOnScreenKeyboard;
import io.appium.java_client.HidesKeyboard;
import io.appium.java_client.LocksDevice;
import io.appium.java_client.battery.BatteryInfo;
import io.appium.java_client.battery.HasBattery;
import io.appium.java_client.clipboard.HasClipboard;
import io.appium.java_client.remote.SupportsRotation;
import org.openqa.selenium.ScreenOrientation;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

/** Common Appium device controls backed by exact live capability interfaces. */
final class DeviceActions implements MobileDeviceActionsContract {
    private final MobileActions mobile;

    DeviceActions(MobileActions mobile) {
        this.mobile = Objects.requireNonNull(mobile, "mobile");
    }

    @Override
    public MobileDeviceActionsContract lock() {
        return perform("lock", "", () -> locksDevice().lockDevice());
    }

    @Override
    public MobileDeviceActionsContract lock(Duration duration) {
        return perform("lock", "<duration>", () -> {
            requireNonNegative(duration, "lock duration");
            locksDevice().lockDevice(duration);
        });
    }

    @Override
    public MobileDeviceActionsContract unlock() {
        return perform("unlock", "", () -> locksDevice().unlockDevice());
    }

    @Override
    public boolean isLocked() {
        return query("is-locked", "", () -> locksDevice().isDeviceLocked());
    }

    @Override
    public ScreenOrientation orientation() {
        return query("orientation", "", () -> rotation().getOrientation());
    }

    @Override
    public MobileDeviceActionsContract orientation(ScreenOrientation orientation) {
        return perform("orientation", orientation == null ? "<orientation>" : orientation.name(), () ->
                rotation().rotate(Objects.requireNonNull(orientation, "orientation")));
    }

    @Override
    public String time() {
        return query("time", "", () -> deviceTime().getDeviceTime());
    }

    @Override
    public String time(String format) {
        return query("time", "<format>", () -> deviceTime().getDeviceTime(requireText(format, "time format")));
    }

    @Override
    public MobileBatteryInfo battery() {
        return query("battery", "", () -> {
            if (!(mobile.driver() instanceof HasBattery<?> hasBattery)) {
                throw unsupported("battery information");
            }
            BatteryInfo battery = Objects.requireNonNull(hasBattery.getBatteryInfo(), "Appium returned no battery information.");
            Object state = battery.getState();
            return new MobileBatteryInfo(battery.getLevel(), state == null ? null : String.valueOf(state));
        });
    }

    @Override
    public MobileKeyboardActionsContract keyboard() {
        return new Keyboard(this);
    }

    @Override
    public MobileClipboardActionsContract clipboard() {
        return new Clipboard(this);
    }

    @Override
    public MobileActionsContract and() {
        return mobile;
    }

    private LocksDevice locksDevice() {
        if (mobile.driver() instanceof LocksDevice locksDevice) {
            return locksDevice;
        }
        throw unsupported("device locking");
    }

    private SupportsRotation rotation() {
        if (mobile.driver() instanceof SupportsRotation supportsRotation) {
            return supportsRotation;
        }
        throw unsupported("screen orientation");
    }

    private HasDeviceTime deviceTime() {
        if (mobile.driver() instanceof HasDeviceTime hasDeviceTime) {
            return hasDeviceTime;
        }
        throw unsupported("device time");
    }

    private HasOnScreenKeyboard onScreenKeyboard() {
        if (mobile.driver() instanceof HasOnScreenKeyboard keyboard) {
            return keyboard;
        }
        throw unsupported("on-screen keyboard state");
    }

    private HidesKeyboard hidesKeyboard() {
        if (mobile.driver() instanceof HidesKeyboard keyboard) {
            return keyboard;
        }
        throw unsupported("keyboard hiding");
    }

    private HasClipboard clipboardProvider() {
        if (mobile.driver() instanceof HasClipboard clipboard) {
            return clipboard;
        }
        throw unsupported("text clipboard");
    }

    private MobileDeviceActionsContract perform(String operation, String locator, Runnable action) {
        query(operation, locator, () -> {
            action.run();
            return null;
        });
        return this;
    }

    private <T> T query(String operation, String locator, Supplier<T> action) {
        TraceEventRecorder.Event event = TraceEventRecorder.start("mobile/device", operation, locator, mobile.traceDriver());
        try {
            T result = action.get();
            TraceEventRecorder.finish(event, "passed", "Mobile device action completed.", null,
                    result == null ? Map.of() : Map.of("result", String.valueOf(result)), List.of());
            return result;
        } catch (RuntimeException exception) {
            TraceEventRecorder.finish(event, "failed", "Mobile device action failed.", exception, Map.of(), List.of());
            throw exception;
        }
    }

    private <T> T querySensitive(String operation, String sensitiveValue, Supplier<T> action) {
        TraceEventRecorder.Event event = TraceEventRecorder.start(
                "mobile/device", operation, "<clipboard-text>", mobile.traceDriver());
        FailureTraceReporter.registerSensitiveSourceValue(sensitiveValue);
        try {
            T result = action.get();
            if (result instanceof String text) {
                FailureTraceReporter.registerSensitiveSourceValue(text);
                FailureTraceReporter.registerSensitiveValue(text);
            }
            TraceEventRecorder.finish(event, "passed", "Mobile device action completed.", null, Map.of(), List.of());
            return result;
        } catch (RuntimeException exception) {
            if (sensitiveValue != null && !sensitiveValue.isEmpty()) {
                FailureTraceReporter.registerSensitiveValue(sensitiveValue);
            }
            TraceEventRecorder.finish(event, "failed", "Mobile device action failed.", exception, Map.of(), List.of());
            throw exception;
        }
    }

    private static <T> T sensitiveProviderCall(Supplier<T> action) {
        try {
            return action.get();
        } catch (RuntimeException exception) {
            FailureTraceReporter.registerSensitiveThrowable(exception);
            throw exception;
        }
    }

    private static Duration requireNonNegative(Duration duration, String label) {
        Objects.requireNonNull(duration, label);
        if (duration.isNegative()) {
            throw new IllegalArgumentException("The " + label + " must not be negative.");
        }
        return duration;
    }

    private static String requireText(String value, String label) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("The " + label + " must not be blank.");
        }
        return value;
    }

    private static UnsupportedOperationException unsupported(String feature) {
        return new UnsupportedOperationException("The live Appium session does not support " + feature + ".");
    }

    private record Keyboard(DeviceActions owner) implements MobileKeyboardActionsContract {
        @Override public boolean isShown() {
            return owner.query("keyboard-is-shown", "", () -> owner.onScreenKeyboard().isKeyboardShown());
        }

        @Override public MobileKeyboardActionsContract hide() {
            owner.perform("keyboard-hide", "", () -> owner.hidesKeyboard().hideKeyboard());
            return this;
        }

        @Override public MobileDeviceActionsContract and() {
            return owner;
        }
    }

    private record Clipboard(DeviceActions owner) implements MobileClipboardActionsContract {
        @Override public String text() {
            return owner.querySensitive("clipboard-text", null, () -> {
                HasClipboard provider = owner.clipboardProvider();
                return sensitiveProviderCall(provider::getClipboardText);
            });
        }

        @Override public MobileClipboardActionsContract text(String text) {
            owner.querySensitive("clipboard-set-text", text, () -> {
                String submittedText = Objects.requireNonNull(text, "clipboard text");
                HasClipboard provider = owner.clipboardProvider();
                sensitiveProviderCall(() -> {
                    provider.setClipboardText(submittedText);
                    return null;
                });
                return null;
            });
            return this;
        }

        @Override public MobileDeviceActionsContract and() {
            return owner;
        }
    }
}
