package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftMcpHeartbeatTest {

    @Test
    void connectionStateTracksConnectedStatus() {
        ShaftMcpConnectionState connectionState = new ShaftMcpConnectionState();

        assertTrue(connectionState.isConnected(), "Should start connected");

        connectionState.setConnected(false);
        assertFalse(connectionState.isConnected(), "Should be disconnected after setConnected(false)");

        connectionState.setConnected(true);
        assertTrue(connectionState.isConnected(), "Should be connected after setConnected(true)");
    }

    @Test
    void connectionStateNotifiesListenersOnChange() throws Exception {
        ShaftMcpConnectionState connectionState = new ShaftMcpConnectionState();
        CountDownLatch changeLatch = new CountDownLatch(1);

        connectionState.addStateChangeListener(changeLatch::countDown);

        connectionState.setConnected(false);
        assertTrue(changeLatch.await(2, TimeUnit.SECONDS), "Should notify listener on state change");
    }

    @Test
    void connectionStateDoesNotNotifyWhenStateUnchanged() throws Exception {
        ShaftMcpConnectionState connectionState = new ShaftMcpConnectionState();
        CountDownLatch noChangeLatch = new CountDownLatch(1);

        connectionState.addStateChangeListener(noChangeLatch::countDown);
        connectionState.setConnected(false);
        assertFalse(noChangeLatch.await(500, TimeUnit.MILLISECONDS),
                "Should not notify when state doesn't actually change");
    }

    @Test
    void connectionStateListenerCanBeRemoved() throws Exception {
        ShaftMcpConnectionState connectionState = new ShaftMcpConnectionState();
        CountDownLatch latch = new CountDownLatch(1);
        Runnable listener = latch::countDown;

        connectionState.addStateChangeListener(listener);
        connectionState.removeStateChangeListener(listener);

        connectionState.setConnected(false);
        assertFalse(latch.await(500, TimeUnit.MILLISECONDS),
                "Should not notify removed listener");
    }

    @Test
    void connectionStateMultipleListenersAllNotified() throws Exception {
        ShaftMcpConnectionState connectionState = new ShaftMcpConnectionState();
        CountDownLatch latch1 = new CountDownLatch(1);
        CountDownLatch latch2 = new CountDownLatch(1);

        connectionState.addStateChangeListener(latch1::countDown);
        connectionState.addStateChangeListener(latch2::countDown);

        connectionState.setConnected(false);
        assertTrue(latch1.await(2, TimeUnit.SECONDS), "First listener should be notified");
        assertTrue(latch2.await(2, TimeUnit.SECONDS), "Second listener should be notified");
    }
}
