package com.shaft.gui.browser.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.latest.network.model.RequestId;
import org.openqa.selenium.devtools.latest.network.model.MonotonicTime;
import org.openqa.selenium.devtools.latest.network.model.WebSocketCreated;
import org.openqa.selenium.devtools.latest.network.model.WebSocketClosed;
import org.openqa.selenium.devtools.latest.network.model.WebSocketFrame;
import org.openqa.selenium.devtools.latest.network.model.WebSocketFrameError;
import org.openqa.selenium.devtools.latest.network.model.WebSocketFrameReceived;
import org.openqa.selenium.devtools.latest.network.model.WebSocketFrameSent;
import org.mockito.Mockito;

import java.util.Base64;

public class CdpWebSocketTraceSourceTest {
    private BrowserObservabilityRecorder.ObservationSession owner;

    private void startOwner() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        owner = BrowserObservabilityRecorder.startSession();
    }

    @AfterMethod
    public void clearOwner() {
        BrowserObservabilityRecorder.clear();
        Properties.clearForCurrentThread();
    }

    @Test
    public void websocketLifecycleShouldRetainBoundedRedactedTextAndBinaryDigest() {
        startOwner();
        CdpWebSocketTraceSource source = new CdpWebSocketTraceSource();
        source.created("socket-1", "wss://example.test/socket");
        source.frame("socket-1", "sent", 1,
                "{\"password\":\"secret-value\",\"data\":\"" + "x".repeat(3_000) + "\"}");
        source.frame("socket-1", "received", 2, Base64.getEncoder().encodeToString(new byte[]{1, 2, 3}));
        source.failed("socket-1", "raw provider secret");
        source.closed("socket-1");

        var entries = BrowserObservabilityRecorder.snapshotWebSockets(owner);
        Assert.assertEquals(entries.size(), 5);
        Assert.assertEquals(entries.get(0).type(), "created");
        Assert.assertEquals(entries.get(1).direction(), "sent");
        Assert.assertTrue(entries.get(1).text().length() <= 2_048);
        Assert.assertFalse(entries.get(1).text().contains("secret-value"), entries.get(1).text());
        Assert.assertEquals(entries.get(2).sizeBytes(), 3L);
        Assert.assertTrue(entries.get(2).text().isEmpty());
        Assert.assertEquals(entries.get(2).sha256(),
                "039058c6f2c0cb492c533b0a4d14ef77cc0f78abccced5287d84a1a2011cfb81");
        Assert.assertEquals(entries.get(3).reason(), "CDP WebSocket frame processing failed.");
        Assert.assertFalse(entries.get(3).reason().contains("provider"));
        Assert.assertEquals(entries.get(4).type(), "closed");
    }

    @Test
    public void websocketLimitAndCloseShouldBeExplicitAndFailClosed() {
        startOwner();
        CdpWebSocketTraceSource source = new CdpWebSocketTraceSource();
        source.created("socket", "wss://example.test/socket");
        for (int index = 0; index <= 1_000; index++) {
            source.frame("socket", "received", 1, "frame-" + index);
        }

        var retained = BrowserObservabilityRecorder.snapshotWebSockets(owner);
        Assert.assertEquals(retained.size(), 1_000);
        Assert.assertEquals(retained.getFirst().text(), "frame-1");
        Assert.assertEquals(retained.getLast().text(), "frame-1000");
        Assert.assertEquals(BrowserObservabilityRecorder.drainWarnings(owner).stream()
                .filter(value -> value.contains("oldest WebSocket")).count(), 1L);

        source.close();
        source.frame("socket", "received", 1, "late-frame");
        Assert.assertEquals(BrowserObservabilityRecorder.snapshotWebSockets(owner).size(), 1_000);
    }

    @Test
    public void activeSocketMapAndMalformedBinaryShouldFailClosed() {
        startOwner();
        CdpWebSocketTraceSource source = new CdpWebSocketTraceSource();
        for (int index = 0; index <= 1_000; index++) {
            source.created("socket-" + index, "wss://example.test/" + index);
        }
        Assert.assertEquals(source.activeSocketCount(), 1_000);
        Assert.assertTrue(source.hasActiveSocket("socket-999"));
        Assert.assertFalse(source.hasActiveSocket("socket-1000"));
        Assert.assertEquals(BrowserObservabilityRecorder.drainWarnings(owner).stream()
                .filter(value -> value.contains("active-socket trace limit")).count(), 1L);
        source.frame("socket-999", "received", 2, "not-base64");
        var malformed = BrowserObservabilityRecorder.snapshotWebSockets(owner).getLast();
        Assert.assertEquals(malformed.status(), "malformed");
        Assert.assertEquals(malformed.reason(), "CDP WebSocket binary frame was malformed.");
        Assert.assertTrue(malformed.text().isEmpty());
        Assert.assertTrue(malformed.sha256().isEmpty());
        source.closed("socket-0");
        source.created("replacement", "wss://example.test/replacement");
        Assert.assertEquals(source.activeSocketCount(), 1_000);
    }

    @Test
    public void overBudgetFramesShouldNotFabricateByteCounts() {
        startOwner();
        CdpWebSocketTraceSource source = new CdpWebSocketTraceSource();
        source.created("large", "wss://example.test/large");
        source.frame("large", "sent", 1, "é".repeat(70_000));
        source.frame("large", "received", 2, "A".repeat(70_000));

        var entries = BrowserObservabilityRecorder.snapshotWebSockets(owner);
        Assert.assertEquals(entries.get(1).status(), "omitted-budget");
        Assert.assertEquals(entries.get(1).sizeBytes(), 0L);
        Assert.assertEquals(entries.get(2).status(), "omitted-budget");
        Assert.assertEquals(entries.get(2).sizeBytes(), 0L);
    }

    @Test
    public void activeSocketFramesShouldFollowObservationSessionRollover() {
        startOwner();
        CdpWebSocketTraceSource source = new CdpWebSocketTraceSource();
        source.created("long-lived", "wss://example.test/long-lived");
        BrowserObservabilityRecorder.ObservationSession firstOwner = owner;
        owner = BrowserObservabilityRecorder.startSession();

        source.frame("long-lived", "received", 1, "after-rollover");

        Assert.assertTrue(BrowserObservabilityRecorder.snapshotWebSockets(firstOwner).isEmpty());
        Assert.assertEquals(BrowserObservabilityRecorder.snapshotWebSockets(owner).getFirst().text(),
                "after-rollover");
    }

    @Test
    public void driverLifecycleShouldAttachOnceAndCloseTheExactSource() throws Exception {
        startOwner();
        DevTools devTools = Mockito.mock(DevTools.class);
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class));
        Mockito.when(((HasDevTools) driver).getDevTools()).thenReturn(devTools);
        com.shaft.driver.internal.DriverFactory.DriverFactoryHelper helper =
                new com.shaft.driver.internal.DriverFactory.DriverFactoryHelper();
        helper.setDriver(driver);
        var start = helper.getClass().getDeclaredMethod("startBrowserObservability");
        start.setAccessible(true);

        start.invoke(helper);
        start.invoke(helper);

        Assert.assertTrue(CdpWebSocketTraceSource.isAttached(driver));
        Mockito.verify(devTools, Mockito.times(1)).createSessionIfThereIsNotOne();
        @SuppressWarnings("rawtypes")
        var listeners = org.mockito.ArgumentCaptor.forClass(java.util.function.Consumer.class);
        Mockito.verify(devTools, Mockito.times(5)).addListener(Mockito.any(), listeners.capture());
        @SuppressWarnings("unchecked")
        java.util.function.Consumer<WebSocketCreated> createdListener = listeners.getAllValues().getFirst();
        createdListener.accept(new WebSocketCreated(new RequestId("wired"), "wss://example.test/wired",
                java.util.Optional.empty()));
        @SuppressWarnings("unchecked")
        java.util.function.Consumer<WebSocketFrameSent> sentListener = listeners.getAllValues().get(1);
        sentListener.accept(new WebSocketFrameSent(new RequestId("wired"), new MonotonicTime(1),
                new WebSocketFrame(1, false, "sent-frame")));
        @SuppressWarnings("unchecked")
        java.util.function.Consumer<WebSocketFrameReceived> receivedListener = listeners.getAllValues().get(2);
        receivedListener.accept(new WebSocketFrameReceived(new RequestId("wired"), new MonotonicTime(2),
                new WebSocketFrame(1, false, "received-frame")));
        @SuppressWarnings("unchecked")
        java.util.function.Consumer<WebSocketFrameError> errorListener = listeners.getAllValues().get(3);
        errorListener.accept(new WebSocketFrameError(new RequestId("wired"), new MonotonicTime(3), "provider detail"));
        @SuppressWarnings("unchecked")
        java.util.function.Consumer<WebSocketClosed> closedListener = listeners.getAllValues().get(4);
        closedListener.accept(new WebSocketClosed(new RequestId("wired"), new MonotonicTime(4)));

        var observations = BrowserObservabilityRecorder.snapshotWebSockets(owner);
        Assert.assertEquals(observations.stream().map(BrowserObservabilityRecorder.WebSocketSnapshotEntry::type)
                .toList(), java.util.List.of("created", "frame", "frame", "error", "closed"));
        Assert.assertEquals(observations.get(1).direction(), "sent");
        Assert.assertEquals(observations.get(1).text(), "sent-frame");
        Assert.assertEquals(observations.get(2).direction(), "received");
        Assert.assertEquals(observations.get(2).text(), "received-frame");
        Assert.assertEquals(observations.get(3).status(), "failed");
        Assert.assertEquals(observations.get(4).requestId(), "wired");
        receivedListener.accept(new WebSocketFrameReceived(new RequestId("wired"), new MonotonicTime(5),
                new WebSocketFrame(1, false, "after-close")));
        Assert.assertEquals(BrowserObservabilityRecorder.snapshotWebSockets(owner).size(), 5);
        helper.closeDriver(driver);
        Assert.assertFalse(CdpWebSocketTraceSource.isAttached(driver));
        Assert.assertFalse(CdpWebSocketTraceSource.attach(driver));
        Mockito.verify(devTools, Mockito.times(5)).addListener(Mockito.any(), Mockito.any(java.util.function.Consumer.class));
    }
}
