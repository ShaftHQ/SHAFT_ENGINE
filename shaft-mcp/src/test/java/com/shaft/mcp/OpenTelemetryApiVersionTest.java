package com.shaft.mcp;

import io.opentelemetry.api.OpenTelemetry;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * GHSA-ccrj-gw8r-854m, Dependabot alert #394: opentelemetry-api at or below 1.66.0.
 * The patched release is 1.67.0. A null Implementation-Version fails this test.
 */
class OpenTelemetryApiVersionTest {
    @Test
    void classpathOpenTelemetryApiIsAtLeastThePatchedRelease() {
        String version = OpenTelemetry.class.getPackage().getImplementationVersion();
        assertNotNull(version, "opentelemetry-api Implementation-Version is null");
        assertTrue(compare(version, "1.67.0") >= 0,
                "opentelemetry-api " + version + " is below 1.67.0");
    }

    private static int compare(String left, String right) {
        String[] a = left.split("[.-]");
        String[] b = right.split("[.-]");
        int length = Math.max(a.length, b.length);
        for (int i = 0; i < length; i++) {
            int av = i < a.length && a[i].chars().allMatch(Character::isDigit) ? Integer.parseInt(a[i]) : 0;
            int bv = i < b.length && b[i].chars().allMatch(Character::isDigit) ? Integer.parseInt(b[i]) : 0;
            if (av != bv) {
                return Integer.compare(av, bv);
            }
        }
        return 0;
    }
}
