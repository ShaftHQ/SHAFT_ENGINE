package com.shaft.intellij.cloud;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftCloudSessionAwarenessTest {
    @Test
    void warnsWhenLicenseFull() {
        var snap = new ShaftCloudSessionAwareness.Snapshot(
                ShaftCloudSessionAwareness.SessionState.RUNNING, 1, 1, "abc", "https://logs", "https://video");
        assertTrue(ShaftCloudSessionAwareness.wouldExceedLicense(snap));
        String warn = ShaftCloudSessionAwareness.preLaunchWarning(snap);
        assertTrue(warn.contains("license looks full"));
        assertTrue(ShaftCloudSessionAwareness.recoveryHint(snap).contains("stop orphan abc"));
    }

    @Test
    void parsesProviderJson() {
        var snap = ShaftCloudSessionAwareness.fromProviderJson(
                "{\"status\":\"running\",\"sessionId\":\"s1\",\"logsUrl\":\"http://l\",\"videoUrl\":\"http://v\"}", 1);
        assertEquals(ShaftCloudSessionAwareness.SessionState.RUNNING, snap.state());
        assertEquals("s1", snap.sessionId());
        assertFalse(ShaftCloudSessionAwareness.preLaunchWarning(
                new ShaftCloudSessionAwareness.Snapshot(
                        ShaftCloudSessionAwareness.SessionState.NONE, 0, 2, null, null, null)).length() > 0);
    }
}
