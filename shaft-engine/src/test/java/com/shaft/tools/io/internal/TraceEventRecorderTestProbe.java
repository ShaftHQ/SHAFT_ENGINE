package com.shaft.tools.io.internal;

public final class TraceEventRecorderTestProbe {
    private TraceEventRecorderTestProbe() {
    }

    public static String json() {
        return TraceEventRecorder.toJson(TraceEventRecorder.snapshot());
    }

    public static String latestBackend() {
        var events = TraceEventRecorder.snapshot();
        return events.isEmpty() ? "" : events.getLast().backend().name();
    }
}
