package com.shaft.intellij.cloud;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftCloudSessionAwarenessTest {
    @Test
    void warnsWhenLicenseFull() {
        var snap = new ShaftCloudSessionAwareness.Snapshot(
                ShaftCloudSessionAwareness.SessionState.RUNNING, 1, 1, "abc", "https://logs", "https://video");
        assertTrue(ShaftCloudSessionAwareness.wouldExceedLicense(snap));
        String warn = ShaftCloudSessionAwareness.preLaunchWarning(snap);
        assertTrue(warn.contains("license looks full"));
        assertTrue(warn.contains("session abc"));
        assertTrue(ShaftCloudSessionAwareness.recoveryHint(snap).contains("stop orphan abc"));
        assertTrue(ShaftCloudSessionAwareness.recoveryHint(snap).contains("logs:"));
        assertTrue(ShaftCloudSessionAwareness.recoveryHint(snap).contains("video:"));
    }

    @Test
    void warningWithoutSessionId() {
        var snap = new ShaftCloudSessionAwareness.Snapshot(
                ShaftCloudSessionAwareness.SessionState.RUNNING, 2, 1, "  ", null, null);
        assertTrue(ShaftCloudSessionAwareness.preLaunchWarning(snap).contains("an existing session"));
        assertTrue(ShaftCloudSessionAwareness.recoveryHint(snap).contains("stop the orphan cloud session"));
    }

    @Test
    void noWarningWhenSlotsAvailable() {
        var snap = new ShaftCloudSessionAwareness.Snapshot(
                ShaftCloudSessionAwareness.SessionState.NONE, 0, 2, null, null, null);
        assertFalse(ShaftCloudSessionAwareness.wouldExceedLicense(snap));
        assertEquals("", ShaftCloudSessionAwareness.preLaunchWarning(snap));
    }

    @Test
    void snapshotRejectsInvalidCounts() {
        assertThrows(IllegalArgumentException.class, () ->
                new ShaftCloudSessionAwareness.Snapshot(
                        ShaftCloudSessionAwareness.SessionState.NONE, -1, 1, null, null, null));
        assertThrows(IllegalArgumentException.class, () ->
                new ShaftCloudSessionAwareness.Snapshot(
                        ShaftCloudSessionAwareness.SessionState.NONE, 0, 0, null, null, null));
    }

    @Test
    void parsesProviderJsonVariants() {
        var running = ShaftCloudSessionAwareness.fromProviderJson(
                "{\"status\":\"running\",\"sessionId\":\"s1\",\"logsUrl\":\"http://l\",\"videoUrl\":\"http://v\"}", 1);
        assertEquals(ShaftCloudSessionAwareness.SessionState.RUNNING, running.state());
        assertEquals("s1", running.sessionId());
        assertEquals(1, running.runningCount());

        var queued = ShaftCloudSessionAwareness.fromProviderJson("{\"state\":\"queued\",\"id\":\"q1\"}", 3);
        assertEquals(ShaftCloudSessionAwareness.SessionState.QUEUED, queued.state());
        assertEquals("q1", queued.sessionId());

        var none = ShaftCloudSessionAwareness.fromProviderJson("{\"active\":0,\"status\":\"none\"}", 2);
        assertEquals(ShaftCloudSessionAwareness.SessionState.NONE, none.state());

        var blank = ShaftCloudSessionAwareness.fromProviderJson("  ", 0);
        assertEquals(ShaftCloudSessionAwareness.SessionState.UNKNOWN, blank.state());
        assertEquals(1, blank.licenseSlots());

        var missing = ShaftCloudSessionAwareness.fromProviderJson(null, 5);
        assertEquals(ShaftCloudSessionAwareness.SessionState.UNKNOWN, missing.state());

        var multi = ShaftCloudSessionAwareness.fromProviderJson(
                "[{\"status\":\"running\"},{\"status\":\"running\"}]", 2);
        assertEquals(2, multi.runningCount());
    }
}
