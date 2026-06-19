package com.shaft.tools.io.internal;

import com.shaft.listeners.internal.TestExecutionInfo;
import io.qameta.allure.model.Status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

/**
 * Runner-neutral per-thread report context and log buffer.
 */
public final class ReportContext {
    private static final ThreadLocal<TestExecutionInfo> currentTest = new ThreadLocal<>();
    private static final ThreadLocal<List<String>> output = ThreadLocal.withInitial(ArrayList::new);
    private static final ThreadLocal<Consumer<String>> logSink = new ThreadLocal<>();
    private static final ThreadLocal<Status> status = new ThreadLocal<>();

    private ReportContext() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Starts report context for the current test.
     *
     * @param info current test metadata
     */
    public static void start(TestExecutionInfo info) {
        currentTest.set(info);
        output.get().clear();
        logSink.remove();
        status.remove();
    }

    /**
     * Updates current test metadata without clearing buffered logs.
     *
     * @param info current test metadata
     */
    public static void update(TestExecutionInfo info) {
        currentTest.set(info);
    }

    /**
     * Appends a formatted log line to the current runner buffer and sink.
     *
     * @param log formatted log line
     */
    public static void append(String log) {
        output.get().add(log);
        Consumer<String> sink = logSink.get();
        if (sink != null) {
            sink.accept(log);
        }
    }

    /**
     * Returns a stable copy of the current runner log buffer.
     *
     * @return buffered report output
     */
    public static List<String> snapshotOutput() {
        return Collections.unmodifiableList(new ArrayList<>(output.get()));
    }

    /**
     * Stores a runner-specific sink such as TestNG Reporter.
     *
     * @param sink log sink, or {@code null} to clear
     */
    public static void setLogSink(Consumer<String> sink) {
        if (sink == null) {
            logSink.remove();
        } else {
            logSink.set(sink);
        }
    }

    /**
     * Sets the current test status for Allure step status mapping.
     *
     * @param currentStatus current Allure status
     */
    public static void setStatus(Status currentStatus) {
        if (currentStatus == null) {
            status.remove();
        } else {
            status.set(currentStatus);
        }
    }

    /**
     * Gets the current test status.
     *
     * @return current status, or {@code null}
     */
    public static Status getStatus() {
        return status.get();
    }

    /**
     * Gets current test class name.
     *
     * @return class name, or empty string
     */
    public static String getTestClassName() {
        TestExecutionInfo info = currentTest.get();
        return info == null || info.className() == null ? "" : info.className();
    }

    /**
     * Gets current test method name.
     *
     * @return method name, or empty string
     */
    public static String getTestMethodName() {
        TestExecutionInfo info = currentTest.get();
        return info == null || info.methodName() == null ? "" : info.methodName();
    }

    /**
     * Clears current thread report context.
     */
    public static void clear() {
        currentTest.remove();
        output.remove();
        logSink.remove();
        status.remove();
    }
}
