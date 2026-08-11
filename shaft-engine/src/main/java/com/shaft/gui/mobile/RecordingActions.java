package com.shaft.gui.mobile;

import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobileRecordingActionsContract;
import com.shaft.gui.driver.MobileRecordingOptions;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.mobile.internal.MobileRecordingState;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.tools.io.internal.TraceArchiveWriter;
import com.shaft.tools.io.internal.TraceEventRecorder;
import io.appium.java_client.screenrecording.BaseStartScreenRecordingOptions;
import io.appium.java_client.screenrecording.CanRecordScreen;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;

/** Appium implementation of explicit, bounded mobile screen recording. */
final class RecordingActions implements MobileRecordingActionsContract {
    private final MobileActions mobile;

    RecordingActions(MobileActions mobile) {
        this.mobile = Objects.requireNonNull(mobile, "mobile");
    }

    @Override
    public MobileRecordingActionsContract start() {
        startWithTrace(MobileRecordingOptions.defaults());
        return this;
    }

    @Override
    public MobileRecordingActionsContract start(MobileRecordingOptions options) {
        startWithTrace(options);
        return this;
    }

    private void startWithTrace(MobileRecordingOptions options) {
        query("start", () -> {
            MobileRecordingOptions required = Objects.requireNonNull(options, "recording options");
            startRaw(required);
            return required;
        }, required -> Map.of("configuredSeconds", Long.toString(required.timeLimit().toSeconds())));
    }

    private void startRaw(MobileRecordingOptions required) {
        MobileRecordingState.start(mobile.driver(), MobileRecordingState.Owner.EXPLICIT, required.maxBytes(),
                () -> provider().startRecordingScreen(
                        new CommonStartOptions().withTimeLimit(required.timeLimit())));
    }

    @Override
    public byte[] stop() {
        return query("stop", this::stopRaw,
                recording -> Map.of("decodedBytes", Integer.toString(recording.length)));
    }

    private byte[] stopRaw() {
        return MobileRecordingState.stop(mobile.driver(), MobileRecordingState.Owner.EXPLICIT,
                () -> provider().stopRecordingScreen());
    }

    @Override
    public Path stopAndSave(Path exactTarget) {
        registerSensitive(exactTarget == null ? null : exactTarget.toString());
        SavedRecording saved = query("stop-and-save", () -> {
            Path target = Objects.requireNonNull(exactTarget, "recording target").toAbsolutePath().normalize();
            registerSensitive(target.toString());
            byte[] recording = stopRaw();
            try {
                TraceArchiveWriter.writeBytes(target, recording);
                return new SavedRecording(target, recording.length);
            } catch (IOException exception) {
                throw new UncheckedIOException("Could not save the mobile screen recording.", exception);
            }
        }, recording -> Map.of("decodedBytes", Integer.toString(recording.decodedBytes())));
        return saved.target();
    }

    @Override
    public MobileActionsContract and() {
        return mobile;
    }

    private CanRecordScreen provider() {
        if (mobile.driver() instanceof CanRecordScreen provider) {
            return provider;
        }
        throw new UnsupportedOperationException(
                "The live Appium session does not support screen recording.");
    }

    private <T> T query(String operation, Supplier<T> action, Function<T, Map<String, String>> metadata) {
        TraceEventRecorder.Event event = TraceEventRecorder.startForBackend(
                "mobile/recording", operation, "<screen-recording>", AutomationBackend.APPIUM);
        try {
            T result = action.get();
            TraceEventRecorder.finish(event, "passed", "Mobile recording action completed.", null,
                    metadata.apply(result), List.of());
            return result;
        } catch (RuntimeException exception) {
            FailureTraceReporter.registerSensitiveThrowable(exception);
            TraceEventRecorder.finish(event, "failed", "Mobile recording action failed.", exception,
                    Map.of(), List.of());
            throw exception;
        }
    }

    private static void registerSensitive(String value) {
        if (value == null || value.isBlank()) {
            return;
        }
        FailureTraceReporter.registerSensitiveSourceValue(value);
        FailureTraceReporter.registerSensitiveValue(value);
    }

    private static final class CommonStartOptions extends BaseStartScreenRecordingOptions<CommonStartOptions> { }

    private record SavedRecording(Path target, int decodedBytes) { }
}
