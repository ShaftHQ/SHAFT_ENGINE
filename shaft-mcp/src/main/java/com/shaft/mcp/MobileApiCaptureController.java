package com.shaft.mcp;

import com.shaft.capture.model.BrowserMetadata;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.PageContext;
import com.shaft.capture.model.network.BodyRef;
import com.shaft.capture.model.network.HttpRequestRecord;
import com.shaft.capture.model.network.HttpResponseRecord;
import com.shaft.capture.model.network.NetworkTiming;
import com.shaft.capture.model.network.ResourceKind;
import com.shaft.capture.network.SecretHeaderReplacer;
import com.shaft.capture.proxy.ApiCaptureProxyServer;
import com.shaft.capture.proxy.CaptureCertificateAuthority;
import com.shaft.capture.proxy.ProxyTransaction;
import com.shaft.capture.storage.CaptureSessionStore;
import com.shaft.capture.storage.NetworkBodyStore;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Owns the lifecycle of one mobile API capture session: a loopback {@link ApiCaptureProxyServer}
 * plus the {@link CaptureSessionStore} it wraps captured {@link ProxyTransaction}s into as
 * first-class {@code CaptureEvent.NetworkEvent}s, mirroring how
 * {@code com.shaft.capture.runtime.ManagedCaptureRecorder} wraps
 * {@code CaptureNetworkRecorder}'s raw callbacks for browser-based capture.
 *
 * <p>Sensitive headers are redacted via {@link SecretHeaderReplacer} and bodies are externalized
 * via {@link NetworkBodyStore} exactly as P1's web recording path does -- this controller adds no
 * new privacy behavior, it reuses the same primitives against a different transport (a MITM proxy
 * instead of a WebDriver DevTools filter).
 */
final class MobileApiCaptureController {
    private static final DateTimeFormatter SESSION_ID_TIME = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss");

    private final NetworkBodyStore bodyStore = new NetworkBodyStore();
    private final SecretHeaderReplacer secretHeaderReplacer = new SecretHeaderReplacer();
    private final List<String> warnings = new CopyOnWriteArrayList<>();
    private final AtomicLong transactionCount = new AtomicLong();

    private CaptureCertificateAuthority certificateAuthority;
    private ApiCaptureProxyServer proxy;
    private CaptureSessionStore store;
    private Path bodiesDirectory;
    private String sessionId;
    private boolean active;

    /**
     * Starts a new mobile API capture session, launching the loopback MITM proxy.
     *
     * @param platform "Android", "iOS", or a caller-supplied label
     * @param deviceLabel emulator/simulator/device identifier, for the session record only
     * @param outputPath persisted capture session JSON path
     * @return status immediately after start
     */
    synchronized MobileApiCaptureStatus start(String platform, String deviceLabel, Path outputPath) {
        if (active) {
            return status();
        }
        warnings.clear();
        transactionCount.set(0);
        sessionId = "mobile-api-" + SESSION_ID_TIME.format(java.time.LocalDateTime.now());
        try {
            Files.createDirectories(outputPath.getParent());
            bodiesDirectory = outputPath.getParent().resolve(sessionId + "-network-bodies");
            Files.createDirectories(bodiesDirectory);

            certificateAuthority = new CaptureCertificateAuthority();
            store = new CaptureSessionStore(outputPath);
            store.start(new CaptureSession(
                    CaptureSession.CURRENT_SCHEMA_VERSION,
                    sessionId,
                    CaptureSession.SessionStatus.INCOMPLETE,
                    Instant.now(),
                    null,
                    new BrowserMetadata("mobile", "", platform, deviceLabel, Map.of()),
                    List.of(), List.of(), List.of(), null, Map.of()));

            proxy = new ApiCaptureProxyServer(certificateAuthority, 0, this::acceptTransaction, warnings::add);
            warnings.addAll(MobilePairingGuidance.forPlatform(platform, proxy.port()));
            active = true;
        } catch (RuntimeException | java.io.IOException failure) {
            active = false;
            warnings.add("Mobile API capture could not start: " + safeMessage(failure));
        }
        return status();
    }

    /**
     * Returns the current session status without changing state.
     *
     * @return current status
     */
    synchronized MobileApiCaptureStatus status() {
        return new MobileApiCaptureStatus(
                active,
                sessionId == null ? "" : sessionId,
                proxy != null && active ? proxy.port() : 0,
                certificateAuthority != null ? certificateAuthority.exportCertificatePem() : "",
                transactionCount.get(),
                List.copyOf(warnings));
    }

    /**
     * Stops the active session, tearing down the proxy.
     *
     * @param discard whether to mark the session incomplete rather than completed
     * @return final status
     */
    synchronized MobileApiCaptureStatus stop(boolean discard) {
        if (!active) {
            return status();
        }
        try {
            if (proxy != null) {
                proxy.stop();
            }
            if (store != null) {
                if (discard) {
                    store.markIncomplete(Instant.now());
                } else {
                    store.stop(Instant.now());
                }
            }
        } catch (RuntimeException failure) {
            warnings.add("Mobile API capture could not stop cleanly: " + safeMessage(failure));
        } finally {
            active = false;
        }
        return status();
    }

    /**
     * A known certificate-pinned host to tunnel without attempting MITM (Tier-3). See
     * {@code com.shaft.capture.proxy.CapturingHttpFilters}.
     *
     * @param host hostname, without port
     */
    synchronized void addPinnedHost(String host) {
        if (proxy != null && host != null && !host.isBlank()) {
            proxy.pinnedHosts().add(host.trim());
        }
    }

    void acceptTransaction(ProxyTransaction transaction) {
        try {
            Map<String, String> requestHeaders = secretHeaderReplacer.replaceSecrets(
                    transaction.requestHeaders(), sessionId);
            Map<String, String> responseHeaders = secretHeaderReplacer.replaceSecrets(
                    transaction.responseHeaders(), sessionId);

            BodyRef requestBodyRef = bodyStore.store(
                    transaction.requestBody(), contentTypeOf(requestHeaders), bodiesDirectory);
            HttpRequestRecord request = new HttpRequestRecord(
                    transaction.method(), transaction.url(), requestHeaders, requestBodyRef);

            HttpResponseRecord response = null;
            if (transaction.statusCode() > 0) {
                BodyRef responseBodyRef = bodyStore.store(
                        transaction.responseBody(), contentTypeOf(responseHeaders), bodiesDirectory);
                response = new HttpResponseRecord(transaction.statusCode(), responseHeaders, responseBodyRef);
            }

            long sequence = store.nextSequence();
            EventContext context = new EventContext(
                    sequence, Instant.now(), new PageContext("", "", "mobile-api-capture", List.of(), 0, 0),
                    EventContext.ReplayStatus.NOT_REPLAYED, List.of(), Map.of());
            CaptureEvent.NetworkEvent event = new CaptureEvent.NetworkEvent(
                    context,
                    "mobile-tx-" + sequence,
                    ResourceKind.XHR,
                    request,
                    response,
                    new NetworkTiming(null, null, null, null, null, null),
                    response == null ? "No response received" : "",
                    "",
                    null);
            store.append(event);
            transactionCount.incrementAndGet();
        } catch (RuntimeException failure) {
            warnings.add("A mobile API transaction could not be recorded: " + safeMessage(failure));
        }
    }

    private static String contentTypeOf(Map<String, String> headers) {
        for (Map.Entry<String, String> entry : headers.entrySet()) {
            if (entry.getKey().equalsIgnoreCase("content-type")) {
                return entry.getValue();
            }
        }
        return "";
    }

    private static String safeMessage(Exception exception) {
        String message = exception.getMessage();
        return message == null || message.isBlank() ? exception.getClass().getSimpleName() : message;
    }
}
