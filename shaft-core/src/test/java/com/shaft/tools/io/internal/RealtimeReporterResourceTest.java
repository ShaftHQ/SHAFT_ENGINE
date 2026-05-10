package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

class RealtimeReporterResourceTest {

    @Test
    void dashboardHtmlShouldBeOnShaftCoreClasspath() {
        // Verifies the fix for: dashboard.html was in shaft-web, so getResourceAsStream
        // silently returned null for shaft-api/shaft-db consumers. Moving it to shaft-core
        // ensures it is always on the classpath for any consumer of shaft-core.
        var resource = RealtimeReporter.class.getResourceAsStream("/realtime/dashboard.html");
        Assert.assertNotNull(resource,
                "realtime/dashboard.html must be resolvable from shaft-core's classpath");
    }
}
