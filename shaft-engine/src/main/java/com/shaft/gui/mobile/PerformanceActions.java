package com.shaft.gui.mobile;

import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobilePerformanceActionsContract;
import com.shaft.gui.driver.MobilePerformanceSample;
import com.shaft.gui.mobile.internal.MobilePerformanceState;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.tools.io.internal.TraceEventRecorder;
import io.appium.java_client.android.HasSupportedPerformanceDataType;

import java.time.Instant;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;

/** Appium implementation of the categorized mobile performance-data facade. */
final class PerformanceActions implements MobilePerformanceActionsContract {
    private static final int LEGACY_FALLBACK_READ_ATTEMPTS = 5;
    private final MobileActions mobile;

    PerformanceActions(MobileActions mobile) {
        this.mobile = Objects.requireNonNull(mobile, "mobile");
    }

    @Override
    public List<String> supportedTypes() {
        return query("supported-types", () -> {
            List<String> types = Objects.requireNonNull(provider().getSupportedPerformanceDataTypes(),
                    "supported performance data types").stream()
                    .map(type -> {
                        registerSensitive(type);
                        return required(type, "performance data type");
                    })
                    .toList();
            if (new HashSet<>(types).size() != types.size()) {
                throw new IllegalArgumentException("supported performance data types must be unique");
            }
            return types;
        }, types -> Map.of("typeCount", Integer.toString(types.size())));
    }

    @Override
    public MobilePerformanceSample sample(String applicationId, String dataType) {
        registerSensitive(applicationId);
        registerSensitive(dataType);
        return query("sample", () -> {
            HasSupportedPerformanceDataType provider = provider();
            String requestedApplication = required(applicationId, "application id");
            String requestedType = required(dataType, "performance data type");
            List<List<Object>> table = Objects.requireNonNull(provider.getPerformanceData(
                    requestedApplication, requestedType, LEGACY_FALLBACK_READ_ATTEMPTS), "performance data");
            if (table.isEmpty()) {
                throw new IllegalArgumentException("performance data must contain a header row");
            }
            List<Object> header = Objects.requireNonNull(table.getFirst(), "performance data header row");
            List<String> columns = header.stream().map(value -> {
                if (!(value instanceof String column)) {
                    throw new IllegalArgumentException("performance data column names must be strings");
                }
                return column;
            }).toList();
            MobilePerformanceSample sample = new MobilePerformanceSample(Instant.now(), requestedApplication,
                    requestedType, columns, table.subList(1, table.size()));
            registerSensitiveSample(sample);
            MobilePerformanceState.append(mobile.driver(), sample);
            return sample;
        }, sample -> Map.of("columnCount", Integer.toString(sample.columns().size()),
                "rowCount", Integer.toString(sample.rows().size())));
    }

    @Override
    public List<MobilePerformanceSample> history() {
        return query("history", () -> MobilePerformanceState.history(mobile.driver()),
                samples -> Map.of("sampleCount", Integer.toString(samples.size())));
    }

    @Override
    public MobilePerformanceActionsContract clear() {
        query("clear", () -> {
            return MobilePerformanceState.clearAndCount(mobile.driver());
        }, count -> Map.of("clearedCount", Integer.toString(count)));
        return this;
    }

    @Override
    public MobileActionsContract and() {
        return mobile;
    }

    private HasSupportedPerformanceDataType provider() {
        if (mobile.driver() instanceof HasSupportedPerformanceDataType provider) {
            return provider;
        }
        throw new UnsupportedOperationException(
                "The live Appium session does not support performance data.");
    }

    private <T> T query(String operation, Supplier<T> action, Function<T, Map<String, String>> metadata) {
        TraceEventRecorder.Event event = TraceEventRecorder.startForBackend(
                "mobile/performance", operation, "<performance-data>", AutomationBackend.APPIUM);
        try {
            T result = action.get();
            TraceEventRecorder.finish(event, "passed", "Mobile performance action completed.", null,
                    metadata.apply(result), List.of());
            return result;
        } catch (RuntimeException exception) {
            FailureTraceReporter.registerSensitiveThrowable(exception);
            TraceEventRecorder.finish(event, "failed", "Mobile performance action failed.", exception,
                    Map.of(), List.of());
            throw exception;
        }
    }

    private static void registerSensitiveSample(MobilePerformanceSample sample) {
        sample.columns().forEach(PerformanceActions::registerSensitive);
        for (List<Object> row : sample.rows()) {
            row.stream().filter(Objects::nonNull).map(String::valueOf)
                    .forEach(PerformanceActions::registerSensitive);
        }
    }

    private static void registerSensitive(String value) {
        if (value == null || value.isBlank()) {
            return;
        }
        FailureTraceReporter.registerSensitiveSourceValue(value);
        FailureTraceReporter.registerSensitiveValue(value);
    }

    private static String required(String value, String name) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(name + " must not be blank");
        }
        return value;
    }
}
