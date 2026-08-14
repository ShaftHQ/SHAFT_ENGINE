package com.shaft.gui.browser.internal;

import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.latest.network.Network;
import org.openqa.selenium.devtools.latest.network.model.WebSocketFrame;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** Passive, session-owned CDP WebSocket lifecycle and bounded-frame trace source. */
public final class CdpWebSocketTraceSource implements AutoCloseable {
    private static final int ACTIVE_SOCKET_LIMIT = 1000;
    private static final int PENDING_FRAME_LIMIT = 1000;
    private static final int HASH_INPUT_LIMIT = 65_536;
    private static final ReferenceQueue<WebDriver> STALE_DRIVERS = new ReferenceQueue<>();
    private static final Map<IdentityWeakReference, CacheEntry> CACHE = new HashMap<>();

    private final BrowserObservabilityRecorder.ObservationBinding binding =
            BrowserObservabilityRecorder.captureBinding();
    private final Map<String, SocketOwner> sockets = new HashMap<>();
    private final Map<String, List<PendingFrame>> pendingFrames = new HashMap<>();
    private final Map<String, BrowserObservabilityRecorder.ObservationSession> pendingClosed = new HashMap<>();
    private int pendingFrameCount;
    private boolean pendingCloseOverflow;
    private BrowserObservabilityRecorder.ObservationSession socketLimitWarningOwner;
    private BrowserObservabilityRecorder.ObservationSession pendingFrameLimitWarningOwner;
    private BrowserObservabilityRecorder.ObservationSession pendingCloseLimitWarningOwner;
    private boolean closed;

    CdpWebSocketTraceSource() { }

    private void register(DevTools devTools, WebDriver driver) {
        if (devTools.getCdpSession() == null) {
            devTools.createSessionIfThereIsNotOne(driver.getWindowHandle());
        }
        devTools.send(Network.enable(java.util.Optional.empty(), java.util.Optional.empty(),
                java.util.Optional.empty(), java.util.Optional.empty(), java.util.Optional.empty()));
        devTools.addListener(Network.webSocketCreated(), event ->
                created(event.getRequestId().toString(), event.getUrl()));
        devTools.addListener(Network.webSocketFrameSent(), event ->
                frame(event.getRequestId().toString(), "sent", event.getResponse()));
        devTools.addListener(Network.webSocketFrameReceived(), event ->
                frame(event.getRequestId().toString(), "received", event.getResponse()));
        devTools.addListener(Network.webSocketFrameError(), event ->
                failed(event.getRequestId().toString()));
        devTools.addListener(Network.webSocketClosed(), event -> closed(event.getRequestId().toString()));
    }

    public static boolean attach(WebDriver driver) {
        if (!(driver instanceof HasDevTools hasDevTools)) {
            return false;
        }
        synchronized (CACHE) {
            expungeStaleDrivers();
            CacheEntry entry = CACHE.get(new IdentityWeakReference(driver));
            if (entry != null) return !entry.closed && entry.source != null && !entry.source.closed;
            CdpWebSocketTraceSource created = new CdpWebSocketTraceSource();
            entry = new CacheEntry();
            entry.source = created;
            CACHE.put(new IdentityWeakReference(driver, STALE_DRIVERS), entry);
            try {
                created.register(hasDevTools.getDevTools(), driver);
                return true;
            } catch (RuntimeException ignored) {
                entry.closed = true;
                created.close();
                return false;
            }
        }
    }

    public static void closeAndRemove(WebDriver driver) {
        CdpWebSocketTraceSource source;
        synchronized (CACHE) {
            expungeStaleDrivers();
            if (driver == null) return;
            CacheEntry entry = CACHE.computeIfAbsent(new IdentityWeakReference(driver, STALE_DRIVERS),
                    ignored -> new CacheEntry());
            entry.closed = true;
            source = entry.source;
            entry.source = null;
        }
        if (source != null) {
            source.close();
        }
    }

    static boolean isAttached(WebDriver driver) {
        synchronized (CACHE) {
            expungeStaleDrivers();
            CacheEntry entry = driver == null ? null : CACHE.get(new IdentityWeakReference(driver));
            return entry != null && !entry.closed && entry.source != null && !entry.source.closed;
        }
    }

    synchronized void created(String id, String url) {
        if (closed || id == null) return;
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.resolveSession(binding);
        if (id.length() > 2_048 || sockets.size() >= ACTIVE_SOCKET_LIMIT) {
            if (!java.util.Objects.equals(socketLimitWarningOwner, owner)) {
                socketLimitWarningOwner = owner;
                BrowserObservabilityRecorder.recordWarning(owner, "websocket",
                        "A CDP WebSocket was omitted because the active-socket trace limit was reached.");
            }
            return;
        }
        String safeUrl = BrowserObservabilityRecorder.retainedNetworkText(url);
        BrowserObservabilityRecorder.ObservationSession closedOwner = pendingClosed.remove(id);
        if (pendingCloseOverflow && closedOwner == null) {
            return;
        }
        sockets.put(id, new SocketOwner(safeUrl));
        List<PendingFrame> reordered = pendingFrames.remove(id);
        BrowserObservabilityRecorder.ObservationSession createdOwner = reordered == null || reordered.isEmpty()
                ? (closedOwner == null ? owner : closedOwner) : reordered.getFirst().owner();
        add(createdOwner, new Entry(id, safeUrl, "", "created", 0, "", "", 0, "available", ""));
        if (reordered != null) {
            pendingFrameCount -= reordered.size();
            reordered.forEach(frame -> add(frame.owner(), frame.entry(id, safeUrl)));
        }
        if (closedOwner != null) {
            recordClosed(id, sockets.remove(id), closedOwner);
        }
    }

    synchronized void frame(String id, String direction, int opcode, String payload) {
        frame(id, direction, new WebSocketFrame(opcode, false, payload));
    }

    private synchronized void frame(String id, String direction, WebSocketFrame frame) {
        if (closed || frame == null || id == null || id.length() > 2_048) return;
        SocketOwner socket = sockets.get(id);
        BrowserObservabilityRecorder.ObservationSession owner =
                BrowserObservabilityRecorder.resolveSession(binding);
        PendingFrame retained = retainedFrame(owner, direction, frame);
        if (socket == null) {
            if (pendingFrameCount >= PENDING_FRAME_LIMIT) {
                if (!java.util.Objects.equals(pendingFrameLimitWarningOwner, owner)) {
                    pendingFrameLimitWarningOwner = owner;
                    BrowserObservabilityRecorder.recordWarning(owner, "websocket",
                            "A reordered CDP WebSocket frame was omitted because the pending trace limit was reached.");
                }
                return;
            }
            pendingFrames.computeIfAbsent(id, ignored -> new java.util.ArrayList<>()).add(retained);
            pendingFrameCount++;
            return;
        }
        add(owner, retained.entry(id, socket.url()));
    }

    private static PendingFrame retainedFrame(BrowserObservabilityRecorder.ObservationSession owner,
                                              String direction, WebSocketFrame frame) {
        String payload = frame.getPayloadData() == null ? "" : frame.getPayloadData();
        int opcode = frame.getOpcode() == null ? 0 : frame.getOpcode().intValue();
        boolean text = opcode == 1;
        String retainedText = "";
        String digest = "";
        long size = 0;
        String status = "available";
        String reason = "";
        if (payload.length() > HASH_INPUT_LIMIT) {
            status = "omitted-budget";
            reason = "CDP WebSocket frame exceeded the bounded inspection limit.";
        } else if (text) {
            retainedText = BrowserObservabilityRecorder.retainedNetworkText(payload);
            size = payload.getBytes(StandardCharsets.UTF_8).length;
        } else {
            byte[] decoded = decode(payload);
            if (decoded == null) {
                status = "malformed";
                reason = "CDP WebSocket binary frame was malformed.";
                size = 0;
            } else {
                size = decoded.length;
                digest = sha256(decoded);
            }
        }
        return new PendingFrame(owner, direction, opcode, retainedText, digest, size, status, reason);
    }

    synchronized void failed(String id) {
        if (closed) return;
        SocketOwner socket = sockets.get(id);
        if (socket != null) {
            add(BrowserObservabilityRecorder.resolveSession(binding),
                    new Entry(id, socket.url(), "", "error", 0, "", "", 0,
                    "failed", "CDP WebSocket frame processing failed."));
        }
    }

    synchronized void closed(String id) {
        if (closed) return;
        BrowserObservabilityRecorder.ObservationSession owner =
                BrowserObservabilityRecorder.resolveSession(binding);
        SocketOwner socket = sockets.remove(id);
        if (socket != null) {
            recordClosed(id, socket, owner);
        } else if (id != null && id.length() <= 2_048) {
            if (pendingClosed.size() >= ACTIVE_SOCKET_LIMIT) {
                pendingCloseOverflow = true;
                if (!java.util.Objects.equals(pendingCloseLimitWarningOwner, owner)) {
                    pendingCloseLimitWarningOwner = owner;
                    BrowserObservabilityRecorder.recordWarning(owner, "websocket",
                            "A reordered CDP WebSocket close was omitted because the pending trace limit was reached.");
                }
                return;
            }
            pendingClosed.put(id, owner);
        }
    }

    private void recordClosed(String id, SocketOwner socket,
                              BrowserObservabilityRecorder.ObservationSession owner) {
        if (socket != null) {
            add(owner, new Entry(id, socket.url(), "", "closed", 0, "", "", 0,
                    "available", ""));
        }
    }

    synchronized int activeSocketCount() { return sockets.size(); }
    synchronized boolean hasActiveSocket(String id) { return sockets.containsKey(id); }

    private void add(BrowserObservabilityRecorder.ObservationSession owner, Entry entry) {
        BrowserObservabilityRecorder.recordWebSocket(owner,
                new BrowserObservabilityRecorder.WebSocketObservation(entry.requestId(), entry.url(),
                        entry.direction(), entry.type(), entry.opcode(), entry.text(), entry.sha256(),
                        entry.sizeBytes(), entry.status(), entry.reason()));
    }

    private static byte[] decode(String payload) {
        try {
            return Base64.getDecoder().decode(payload);
        } catch (IllegalArgumentException ignored) {
            return null;
        }
    }

    private static String sha256(byte[] bytes) {
        try {
            return java.util.HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException(exception);
        }
    }

    private static void expungeStaleDrivers() {
        IdentityWeakReference stale;
        while ((stale = (IdentityWeakReference) STALE_DRIVERS.poll()) != null) CACHE.remove(stale);
    }

    @Override public synchronized void close() {
        closed = true;
        sockets.clear();
        pendingFrames.clear();
        pendingClosed.clear();
        pendingFrameCount = 0;
        pendingCloseOverflow = false;
        socketLimitWarningOwner = null;
        pendingFrameLimitWarningOwner = null;
        pendingCloseLimitWarningOwner = null;
    }

    record Entry(String requestId, String url, String direction, String type, int opcode,
                 String text, String sha256, long sizeBytes, String status, String reason) { }
    private record PendingFrame(BrowserObservabilityRecorder.ObservationSession owner,
                                String direction, int opcode, String text, String sha256,
                                long sizeBytes, String status, String reason) {
        private Entry entry(String requestId, String url) {
            return new Entry(requestId, url, direction, "frame", opcode, text, sha256, sizeBytes, status, reason);
        }
    }
    private record SocketOwner(String url) { }

    private static final class IdentityWeakReference extends WeakReference<WebDriver> {
        private final int identityHash;
        private IdentityWeakReference(WebDriver driver) { super(driver); identityHash = System.identityHashCode(driver); }
        private IdentityWeakReference(WebDriver driver, ReferenceQueue<WebDriver> queue) {
            super(driver, queue); identityHash = System.identityHashCode(driver);
        }
        @Override public int hashCode() { return identityHash; }
        @Override public boolean equals(Object other) {
            if (this == other) return true;
            if (!(other instanceof IdentityWeakReference reference)) return false;
            WebDriver driver = get();
            return driver != null && driver == reference.get();
        }
    }

    private static final class CacheEntry {
        private CdpWebSocketTraceSource source;
        private boolean closed;
    }
}
