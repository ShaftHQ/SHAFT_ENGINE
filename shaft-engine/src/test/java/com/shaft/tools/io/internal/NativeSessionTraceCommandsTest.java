package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import io.appium.java_client.android.AndroidDriver;
import org.mockito.Mockito;
import org.openqa.selenium.remote.SessionId;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

/**
 * Issue #6827: tracing an action in a native app session must not ask Appium for a URL or run the
 * browser-only {@code document.documentElement.outerHTML} script.
 */
public class NativeSessionTraceCommandsTest {
    private String originalPlatform;
    private boolean originalDomSnapshots;
    private boolean originalTraceEnabled;

    @BeforeMethod
    public void useNativeAndroid() {
        originalPlatform = SHAFT.Properties.platform.targetPlatform();
        originalDomSnapshots = SHAFT.Properties.reporting.traceIncludeDomSnapshots();
        originalTraceEnabled = SHAFT.Properties.reporting.traceEnabled();
        SHAFT.Properties.platform.set().targetPlatform("Android");
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeDomSnapshots(true);
    }

    @AfterMethod(alwaysRun = true)
    public void restore() {
        TraceEventRecorder.clear();
        SHAFT.Properties.platform.set().targetPlatform(originalPlatform);
        SHAFT.Properties.reporting.set().traceEnabled(originalTraceEnabled).traceIncludeDomSnapshots(originalDomSnapshots);
    }

    @Test
    public void tracingANativeActionSendsNoBrowserOnlyCommand() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("native-trace"));

        var event = TraceEventRecorder.start("element", "click", "accessibility id=submit", driver);
        TraceEventRecorder.finish(event, "failed", "boom", new IllegalStateException("boom"), Map.of(), List.of());

        Mockito.verify(driver, Mockito.never()).getCurrentUrl();
        Mockito.verify(driver, Mockito.never()).executeScript(Mockito.anyString(), Mockito.any(Object[].class));
    }
}
