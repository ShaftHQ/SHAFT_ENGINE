package com.shaft.gui.mobile;

import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobileLogActionsContract;
import com.shaft.gui.driver.MobileLogError;
import com.shaft.gui.driver.MobileLogMessage;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.mobile.internal.MobileLogSource;
import com.shaft.tools.io.internal.TraceEventRecorder;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

/** Appium continuous device-log implementation for the categorized mobile facade. */
final class LogActions implements MobileLogActionsContract {
    private final MobileActions mobile;

    LogActions(MobileActions mobile) {
        this.mobile = Objects.requireNonNull(mobile, "mobile");
    }

    @Override
    public MobileLogActionsContract start() {
        query("start", () -> {
            MobileLogSource.start(mobile.driver());
            return null;
        });
        return this;
    }

    @Override
    public List<MobileLogMessage> messages() {
        return query("messages", () -> MobileLogSource.messages(mobile.driver()));
    }

    @Override
    public List<MobileLogError> errors() {
        return query("errors", () -> MobileLogSource.errors(mobile.driver()));
    }

    @Override
    public MobileLogActionsContract clear() {
        query("clear", () -> {
            MobileLogSource.clear(mobile.driver());
            return null;
        });
        return this;
    }

    @Override
    public MobileLogActionsContract stop() {
        query("stop", () -> {
            MobileLogSource.stop(mobile.driver());
            return null;
        });
        return this;
    }

    @Override
    public MobileActionsContract and() {
        return mobile;
    }

    private <T> T query(String operation, Supplier<T> action) {
        TraceEventRecorder.Event event = TraceEventRecorder.startForBackend(
                "mobile/logs", operation, "<device-logs>", AutomationBackend.APPIUM);
        try {
            T result = action.get();
            TraceEventRecorder.finish(event, "passed", "Mobile log action completed.", null, Map.of(), List.of());
            return result;
        } catch (RuntimeException exception) {
            TraceEventRecorder.finish(event, "failed", "Mobile log action failed.", exception, Map.of(), List.of());
            throw exception;
        }
    }
}
