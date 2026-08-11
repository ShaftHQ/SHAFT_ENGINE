package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.MobileContextActionsContract;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import org.mockito.Mockito;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.LinkedHashSet;
import java.util.List;

public class MobileContextNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
        ReportContext.clear();
    }

    @Test
    public void contextQueriesShouldRetainResultsInOneMobileEvent() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("context-query"));
        Mockito.when(driver.getContext()).thenReturn("NATIVE_APP");
        Mockito.when(driver.getContextHandles()).thenReturn(new LinkedHashSet<>(List.of("NATIVE_APP", "WEBVIEW_app")));
        MobileContextActionsContract context = new SHAFT.GUI.WebDriver(driver).mobile().context();

        Assert.assertEquals(context.current(), "NATIVE_APP");
        assertSingleEvent("current", "passed");
        Assert.assertEquals(TraceEventRecorder.snapshot().getFirst().metadata().get("result"), "NATIVE_APP");
        TraceEventRecorder.clear();

        Assert.assertEquals(context.handles(), List.of("NATIVE_APP", "WEBVIEW_app"));
        assertSingleEvent("handles", "passed");
        Assert.assertTrue(TraceEventRecorder.snapshot().getFirst().metadata().get("result").contains("WEBVIEW_app"));
    }

    @Test
    public void validationAndMissingWebViewShouldFailBeforeContextMutation() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("context-negative"));
        Mockito.when(driver.getContext()).thenReturn("NATIVE_APP");
        Mockito.when(driver.getContextHandles()).thenReturn(new LinkedHashSet<>(List.of("NATIVE_APP")));
        MobileContextActionsContract context = new SHAFT.GUI.WebDriver(driver).mobile().context();

        Assert.expectThrows(IllegalArgumentException.class, () -> context.switchTo(" "));
        assertSingleEvent("switch-to", "failed");
        Assert.assertTrue(TraceEventRecorder.snapshot().getFirst().exceptionMessage().contains("must not be blank"));
        TraceEventRecorder.clear();

        Assert.expectThrows(UnsupportedOperationException.class, context::webView);
        assertSingleEvent("switch-to-webview", "failed");
        Mockito.verify(driver, Mockito.never()).context(Mockito.anyString());
    }

    @Test
    public void unsupportedAndStaleContextsShouldFailClosedWithOneEvent() {
        AppiumDriver generic = Mockito.mock(AppiumDriver.class);
        Mockito.when(generic.getSessionId()).thenReturn(new SessionId("context-generic"));
        MobileContextActionsContract unsupported = new SHAFT.GUI.WebDriver(generic).mobile().context();

        Assert.expectThrows(UnsupportedOperationException.class, unsupported::current);
        assertSingleEvent("current", "failed");
        TraceEventRecorder.clear();

        AndroidDriver staleDriver = Mockito.mock(AndroidDriver.class);
        Mockito.when(staleDriver.getSessionId()).thenReturn(new SessionId("context-stale"), null);
        MobileContextActionsContract stale = new SHAFT.GUI.WebDriver(staleDriver).mobile().context();

        Assert.expectThrows(UnsupportedOperationException.class, () -> stale.switchTo("WEBVIEW_app"));
        assertSingleEvent("switch-to", "failed");
        Mockito.verify(staleDriver, Mockito.never()).context(Mockito.anyString());
    }

    @Test
    public void providerSwitchFailureShouldPreserveRequestedAndBeforeAfterEvidence() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        IllegalStateException providerFailure = new IllegalStateException("provider rejected context");
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("context-provider"));
        Mockito.when(driver.getContext()).thenReturn("NATIVE_APP");
        Mockito.doThrow(providerFailure).when(driver).context("WEBVIEW_app");
        MobileContextActionsContract context = new SHAFT.GUI.WebDriver(driver).mobile().context();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, () -> context.switchTo("WEBVIEW_app"));

        Assert.assertSame(thrown, providerFailure);
        assertSingleEvent("switch-to", "failed");
        var metadata = TraceEventRecorder.snapshot().getFirst().metadata();
        Assert.assertEquals(metadata.get("requestedContext"), "WEBVIEW_app");
        Assert.assertEquals(metadata.get("contextBefore"), "NATIVE_APP");
        Assert.assertEquals(metadata.get("contextAfter"), "NATIVE_APP");
    }

    @Test
    public void successfulSwitchShouldNotBecomeFailureWhenBestEffortAfterProbeFails() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("context-post-probe"));
        Mockito.when(driver.getContext()).thenReturn("NATIVE_APP")
                .thenThrow(new IllegalStateException("post-switch probe unavailable"));
        MobileContextActionsContract context = new SHAFT.GUI.WebDriver(driver).mobile().context();

        Assert.assertSame(context.switchTo("WEBVIEW_app"), context);

        assertSingleEvent("switch-to", "passed");
        Assert.assertEquals(TraceEventRecorder.snapshot().getFirst().metadata().get("requestedContext"), "WEBVIEW_app");
        Assert.assertEquals(TraceEventRecorder.snapshot().getFirst().metadata().get("contextAfter"), "unavailable");
        Mockito.verify(driver).context("WEBVIEW_app");
    }

    private static void assertSingleEvent(String operation, String status) {
        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "mobile/context");
        Assert.assertEquals(events.getFirst().name(), operation);
        Assert.assertEquals(events.getFirst().status(), status);
        Assert.assertEquals(events.getFirst().backend(), AutomationBackend.APPIUM);
    }
}
