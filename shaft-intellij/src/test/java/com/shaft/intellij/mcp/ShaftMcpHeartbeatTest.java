package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftMcpHeartbeatTest {

    /**
     * Regression for issue #3399: while the shared long-lived MCP client exists, the heartbeat
     * must only peek at its liveness — never spawn a fresh probe process every 30 seconds.
     */
    @Test
    void heartbeatPeeksSharedClientInsteadOfSpawningAProbeProcess() {
        ShaftMcpConnectionState connectionState = new ShaftMcpConnectionState();
        AtomicInteger probeSpawns = new AtomicInteger();
        ShaftMcpHeartbeat heartbeat = new ShaftMcpHeartbeat(connectionState,
                () -> Optional.of(true),
                () -> {
                    probeSpawns.incrementAndGet();
                    return CompletableFuture.completedFuture(ShaftMcpToolResult.success("pong"));
                });

        int nextDelay = heartbeat.performPing();

        assertEquals(0, probeSpawns.get(), "a live shared client must be peeked, not probed");
        assertTrue(connectionState.isConnected());
        assertEquals(30_000, nextDelay);

        connectionState.setConnected(true);
        ShaftMcpHeartbeat deadSharedClient = new ShaftMcpHeartbeat(connectionState,
                () -> Optional.of(false),
                () -> {
                    probeSpawns.incrementAndGet();
                    return CompletableFuture.completedFuture(ShaftMcpToolResult.success("pong"));
                });
        deadSharedClient.performPing();
        assertEquals(0, probeSpawns.get());
        assertFalse(connectionState.isConnected(),
                "a dead shared client must surface as disconnected without spawning anything");
    }

    /**
     * Regression for issue #3399: consecutive spawn-probe failures (no shared client exists) back
     * off exponentially up to five minutes instead of burning a fresh OS process every 30 seconds.
     */
    @Test
    void heartbeatSpawnProbeFailuresBackOffExponentiallyAndRecoverOnSuccess() {
        ShaftMcpConnectionState connectionState = new ShaftMcpConnectionState();
        List<Boolean> outcomes = new CopyOnWriteArrayList<>(
                List.of(false, false, false, false, false, false, true));
        AtomicInteger calls = new AtomicInteger();
        ShaftMcpHeartbeat heartbeat = new ShaftMcpHeartbeat(connectionState,
                Optional::empty,
                () -> {
                    boolean success = outcomes.get(Math.min(calls.getAndIncrement(), outcomes.size() - 1));
                    return CompletableFuture.completedFuture(success
                            ? ShaftMcpToolResult.success("pong")
                            : ShaftMcpToolResult.failure("boom"));
                });

        assertEquals(60_000, heartbeat.performPing());
        assertEquals(120_000, heartbeat.performPing());
        assertEquals(240_000, heartbeat.performPing());
        assertEquals(300_000, heartbeat.performPing(), "backoff caps at five minutes");
        assertEquals(300_000, heartbeat.performPing());
        assertFalse(connectionState.isConnected());

        assertEquals(300_000, heartbeat.performPing());
        assertEquals(30_000, heartbeat.performPing(), "a success resets the cadence");
        assertTrue(connectionState.isConnected());
    }

    @Test
    void heartbeatFallsBackToSpawnProbeWhenPeekingFails() {
        ShaftMcpConnectionState connectionState = new ShaftMcpConnectionState();
        AtomicInteger probeSpawns = new AtomicInteger();
        ShaftMcpHeartbeat heartbeat = new ShaftMcpHeartbeat(connectionState,
                () -> {
                    throw new IllegalStateException("service container unavailable");
                },
                () -> {
                    probeSpawns.incrementAndGet();
                    return CompletableFuture.completedFuture(ShaftMcpToolResult.success("pong"));
                });

        assertEquals(30_000, heartbeat.performPing());
        assertEquals(1, probeSpawns.get());
        assertTrue(connectionState.isConnected());
    }

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
        connectionState.setConnected(false);
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
