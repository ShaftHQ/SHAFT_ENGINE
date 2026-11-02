package com.shaft.gui.mobile;

import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobileBiometricActionsContract;
import com.shaft.gui.driver.MobileFingerprintActionsContract;
import com.shaft.gui.driver.MobileTouchIdActionsContract;
import io.appium.java_client.android.AuthenticatesByFinger;
import io.appium.java_client.ios.PerformsTouchID;
import com.shaft.tools.io.internal.TraceEventRecorder;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

/** Appium biometric simulation implementation for the categorized mobile facade. */
final class BiometricActions implements MobileBiometricActionsContract {
    private final MobileActions mobile;

    BiometricActions(MobileActions mobile) {
        this.mobile = Objects.requireNonNull(mobile, "mobile");
    }

    @Override
    public MobileFingerprintActionsContract fingerprint() {
        fingerprintProvider();
        return new FingerprintActions(this);
    }

    @Override
    public MobileTouchIdActionsContract touchId() {
        touchIdProvider();
        return new TouchIdActions(this);
    }

    @Override
    public MobileActionsContract and() {
        return mobile;
    }

    private AuthenticatesByFinger fingerprintProvider() {
        if (mobile.driver() instanceof AuthenticatesByFinger provider) {
            return provider;
        }
        throw new UnsupportedOperationException(
                "The live Appium session does not support Android emulator fingerprint simulation.");
    }

    private PerformsTouchID touchIdProvider() {
        if (mobile.driver() instanceof PerformsTouchID provider) {
            return provider;
        }
        throw new UnsupportedOperationException(
                "The live Appium session does not support iOS Simulator Touch ID simulation.");
    }

    private <T> T query(String operation, Supplier<T> action) {
        TraceEventRecorder.Event event = TraceEventRecorder.startForBackend(
                "mobile/biometrics", operation, "<biometric>", AutomationBackend.APPIUM);
        try {
            T result = action.get();
            TraceEventRecorder.finish(event, "passed", "Mobile biometric action completed.",
                    null, Map.of(), List.of());
            return result;
        } catch (RuntimeException exception) {
            TraceEventRecorder.finish(event, "failed", "Mobile biometric action failed.",
                    exception, Map.of(), List.of());
            throw exception;
        }
    }

    private static final class FingerprintActions implements MobileFingerprintActionsContract {
        private final BiometricActions owner;

        private FingerprintActions(BiometricActions owner) {
            this.owner = owner;
        }

        @Override
        public MobileFingerprintActionsContract authenticate(int fingerprintId) {
            return owner.query("fingerprint-authenticate", () -> {
                if (fingerprintId < 1 || fingerprintId > 10) {
                    throw new IllegalArgumentException("fingerprint ID must be between 1 and 10.");
                }
                owner.fingerprintProvider().fingerPrint(fingerprintId);
                return this;
            });
        }

        @Override
        public MobileBiometricActionsContract and() {
            return owner;
        }
    }

    private static final class TouchIdActions implements MobileTouchIdActionsContract {
        private final BiometricActions owner;

        private TouchIdActions(BiometricActions owner) {
            this.owner = owner;
        }

        @Override
        public MobileTouchIdActionsContract match() {
            return owner.query("touch-id-match", () -> {
                owner.touchIdProvider().performTouchID(true);
                return this;
            });
        }

        @Override
        public MobileTouchIdActionsContract reject() {
            return owner.query("touch-id-reject", () -> {
                owner.touchIdProvider().performTouchID(false);
                return this;
            });
        }

        @Override
        public MobileTouchIdActionsContract enroll() {
            return owner.query("touch-id-enroll", () -> {
                owner.touchIdProvider().toggleTouchIDEnrollment(true);
                return this;
            });
        }

        @Override
        public MobileTouchIdActionsContract unenroll() {
            return owner.query("touch-id-unenroll", () -> {
                owner.touchIdProvider().toggleTouchIDEnrollment(false);
                return this;
            });
        }

        @Override
        public MobileBiometricActionsContract and() {
            return owner;
        }
    }
}
