package com.shaft.tools.io.internal;

import io.opentelemetry.api.GlobalOpenTelemetry;
import io.opentelemetry.api.OpenTelemetry;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.StatusCode;
import io.opentelemetry.api.trace.Tracer;
import io.opentelemetry.context.Context;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StatusDetails;

import java.util.ArrayDeque;
import java.util.Deque;

/**
 * Emits one OpenTelemetry span per test and per reported step (#6404) through the OpenTelemetry API only.
 * Users bring the SDK and exporter (or the Java agent); without one the global API is a no-op.
 */
public final class OpenTelemetryTracing {
    private static final String SCOPE = "io.github.shafthq.shaft";
    private static final ThreadLocal<Deque<Span>> SPANS = ThreadLocal.withInitial(ArrayDeque::new);
    private static volatile OpenTelemetry openTelemetry;

    private OpenTelemetryTracing() {
    }

    /**
     * Overrides the OpenTelemetry instance (tests); {@code null} restores {@link GlobalOpenTelemetry}.
     *
     * @param instance the instance to use
     */
    public static void use(OpenTelemetry instance) {
        openTelemetry = instance;
        SPANS.remove();
    }

    /**
     * Starts a span nested under the current test or step span.
     *
     * @param kind {@code test} or {@code step}
     * @param name span name
     */
    public static void start(String kind, String name) {
        Deque<Span> stack = SPANS.get();
        Context parent = stack.isEmpty() ? Context.root() : Context.root().with(stack.peek());
        Span span = tracer().spanBuilder(name == null ? kind : name).setParent(parent)
                .setAttribute("shaft.kind", kind).startSpan();
        stack.push(span);
    }

    /**
     * Ends the innermost span with the Allure status and error details.
     *
     * @param status  Allure status
     * @param details Allure status details, may be {@code null}
     */
    public static void stop(Status status, StatusDetails details) {
        Span span = SPANS.get().poll();
        if (span == null) {
            return;
        }
        span.setAttribute("shaft.status", status == null ? "unknown" : status.value());
        if (status == Status.FAILED || status == Status.BROKEN) {
            span.setStatus(StatusCode.ERROR, details == null || details.getMessage() == null ? "" : details.getMessage());
        } else if (status == Status.PASSED) {
            span.setStatus(StatusCode.OK);
        }
        span.end();
    }

    private static Tracer tracer() {
        OpenTelemetry instance = openTelemetry;
        return (instance == null ? GlobalOpenTelemetry.get() : instance).getTracer(SCOPE);
    }
}
