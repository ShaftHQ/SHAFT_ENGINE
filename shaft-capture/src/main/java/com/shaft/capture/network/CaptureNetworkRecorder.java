package com.shaft.capture.network;

import com.shaft.capture.model.network.BodyRef;
import com.shaft.capture.model.network.HttpRequestRecord;
import com.shaft.capture.model.network.HttpResponseRecord;
import com.shaft.capture.model.network.NetworkTiming;
import com.shaft.capture.model.network.ResourceKind;
import com.shaft.capture.storage.NetworkBodyStore;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.NetworkInterceptor;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.Filter;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;

/**
 * Per-session network transaction recorder. Registers a single Selenium DevTools {@link Filter}
 * (the same {@code NetworkInterceptor}-backed mechanism {@code BrowserNetworkInterceptor} uses),
 * classifies each transaction, applies asset/first-party/glob filters and a transaction cap,
 * scrubs secret headers via {@link SecretHeaderReplacer}, persists bodies through
 * {@link NetworkBodyStore}, and emits one {@link RecordedTransaction} per recorded transaction to
 * the session sink, which is responsible for wrapping it into a first-class
 * {@code CaptureEvent.NetworkEvent} with a properly sequenced {@code EventContext}.
 *
 * <p>This recorder is the sole owner of the DevTools network filter for its session. Selenium's
 * {@link NetworkInterceptor} registration is a single-slot replace, not a compose/stack: whichever
 * interceptor registers last on a given driver silently wins, and the other's filter goes dead with
 * no warning. {@code com.shaft.driver.internal.DriverFactory.DriverFactoryHelper} may already have
 * started a {@code BrowserNetworkInterceptor}-owned {@link NetworkInterceptor} for passive trace/HAR
 * observation on the same driver whenever {@code shaft.trace.enabled} and
 * {@code shaft.trace.includeNetwork} are {@code true} -- a global property independent of API
 * capture. Before installing its own filter, {@link #start()} calls
 * {@code DriverFactoryHelper.releaseBrowserNetworkObservationForHandoff(WebDriver)} so that prior
 * passive observer relinquishes the DevTools filter first; this recorder's own filter composes
 * {@link BrowserObservabilityRecorder#startNetwork(HttpRequest)} and
 * {@link BrowserObservabilityRecorder#finishNetwork(BrowserObservabilityRecorder.NetworkExchange, HttpResponse, String)}
 * so trace/HAR capture keeps working without a second interceptor. When the driver's
 * {@code BrowserNetworkInterceptor} instead has mock/validate rules registered, handoff is refused
 * (replacing that filter would silently break those rules); in that case this recorder logs a
 * one-time warning and does not register its own filter, avoiding the double-registration entirely.
 */
public final class CaptureNetworkRecorder implements AutoCloseable {
    private final WebDriver driver;
    private final NetworkBodyStore bodyStore;
    private final Path sessionDirectory;
    private final NetworkCaptureOptions options;
    private final String sessionId;
    private final SecretHeaderReplacer secretHeaderReplacer = new SecretHeaderReplacer();
    private final Consumer<RecordedTransaction> sink;
    private final Consumer<String> warn;
    private final AtomicLong nextTransactionId = new AtomicLong();
    private final AtomicLong recordedCount = new AtomicLong();
    private final Deque<String> lastEndpoints = new ArrayDeque<>();
    private final Object lastEndpointsLock = new Object();
    private volatile boolean maxTransactionsWarned;
    private NetworkInterceptor interceptor;
    private volatile boolean started;

    /**
     * Creates a network recorder for one browser session.
     *
     * @param driver active WebDriver session
     * @param bodiesDirectory directory used to persist request/response bodies
     * @param options network capture filtering and body-capture options
     * @param sessionId owning capture session identifier, used for deterministic secret-ref derivation
     * @param sink destination for each recorded, redacted network transaction
     * @param warn destination for safe one-time warnings
     */
    public CaptureNetworkRecorder(
            WebDriver driver,
            Path bodiesDirectory,
            NetworkCaptureOptions options,
            String sessionId,
            Consumer<RecordedTransaction> sink,
            Consumer<String> warn) {
        if (driver == null || bodiesDirectory == null || sink == null || warn == null) {
            throw new IllegalArgumentException(
                    "Network recorder requires a driver, bodies directory, sink, and warn consumer.");
        }
        this.driver = driver;
        this.bodyStore = new NetworkBodyStore();
        this.sessionDirectory = bodiesDirectory.toAbsolutePath().normalize();
        this.options = options == null ? NetworkCaptureOptions.defaults() : options;
        this.sessionId = sessionId == null || sessionId.isBlank() ? "capture-session" : sessionId;
        this.sink = sink;
        this.warn = warn;
    }

    /**
     * Starts network capture when the driver supports DevTools. Gates on {@link HasDevTools};
     * when unsupported, records a warning and leaves UI-only capture unaffected.
     *
     * <p>Before installing its own DevTools network filter, this method asks
     * {@code DriverFactoryHelper} to release any passive trace/HAR observer it already registered
     * on this driver, so this recorder becomes the sole interceptor owner instead of silently
     * racing with and replacing it (or being replaced by it). When the driver's
     * {@code BrowserNetworkInterceptor} instead has mock/validate rules registered, the handoff is
     * refused; this method then logs a one-time warning and does not register a filter, so API
     * capture is skipped rather than double-registering and breaking those rules.
     *
     * @return {@code true} when the network filter was registered
     */
    public synchronized boolean start() {
        if (started) {
            return true;
        }
        if (!(driver instanceof HasDevTools)) {
            warn.accept("API capture is not supported by this driver; continuing with UI-only capture.");
            return false;
        }
        if (!canBecomeSoleInterceptorOwner()) {
            warn.accept("API capture could not become the sole DevTools network interceptor for this "
                    + "driver because active network mock/validate rules are registered; "
                    + "continuing with UI-only capture.");
            return false;
        }
        try {
            interceptor = new NetworkInterceptor(driver, createFilter());
            started = true;
            return true;
        } catch (RuntimeException exception) {
            warn.accept("API capture could not start for this driver; continuing with UI-only capture.");
            return false;
        }
    }

    /**
     * Ensures this recorder can safely become the sole DevTools network filter owner for the
     * driver: releases any passive trace/HAR observation a {@code DriverFactoryHelper}-owned
     * {@code BrowserNetworkInterceptor} already registered on the same driver, and refuses to
     * proceed when that interceptor instead has active mock/validate rules registered.
     *
     * @return {@code true} when it is safe to install this recorder's own network filter
     */
    private boolean canBecomeSoleInterceptorOwner() {
        try {
            return DriverFactoryHelper.releaseBrowserNetworkObservationForHandoff(driver)
                    || !DriverFactoryHelper.hasBlockingBrowserNetworkInterceptionRules(driver);
        } catch (RuntimeException exception) {
            return true;
        }
    }

    /**
     * Returns the number of network transactions recorded so far.
     *
     * @return recorded transaction count
     */
    public int transactionCount() {
        return (int) Math.min(Integer.MAX_VALUE, recordedCount.get());
    }

    /**
     * Returns the most recently observed endpoints, most-recent-first, bounded to a small preview.
     *
     * @return immutable, most-recent-first endpoint list
     */
    public List<String> lastEndpoints() {
        synchronized (lastEndpointsLock) {
            return List.copyOf(lastEndpoints);
        }
    }

    /**
     * Stops network capture and releases the DevTools filter exactly once.
     */
    @Override
    public synchronized void close() {
        started = false;
        if (interceptor != null) {
            try {
                interceptor.close();
            } catch (RuntimeException ignored) {
                // Closing an already-reset DevTools network filter is harmless during teardown.
            } finally {
                interceptor = null;
            }
        }
    }

    private Filter createFilter() {
        return next -> request -> {
            long transactionId = nextTransactionId.incrementAndGet();
            String pageOrigin = currentPageOrigin();
            byte[] requestBody = copyRequestBody(request);
            boolean traceNetworkEnabled = isTraceNetworkEnabled();
            BrowserObservabilityRecorder.NetworkExchange traceExchange = traceNetworkEnabled
                    ? BrowserObservabilityRecorder.startNetwork(request)
                    : null;
            long startNanos = System.nanoTime();
            try {
                HttpResponse response = next.execute(request);
                byte[] responseBody = copyResponseBody(response);
                if (traceExchange != null) {
                    BrowserObservabilityRecorder.finishNetwork(traceExchange, response, "");
                }
                recordIfAccepted(transactionId, pageOrigin, request, response, requestBody, responseBody,
                        startNanos, "");
                return response;
            } catch (RuntimeException exception) {
                if (traceExchange != null) {
                    BrowserObservabilityRecorder.finishNetwork(traceExchange, null, exception.getClass().getSimpleName());
                }
                recordIfAccepted(transactionId, pageOrigin, request, null, requestBody, new byte[0],
                        startNanos, exception.getClass().getSimpleName());
                throw exception;
            }
        };
    }

    private void recordIfAccepted(
            long transactionId,
            String pageOrigin,
            HttpRequest request,
            HttpResponse response,
            byte[] requestBody,
            byte[] responseBody,
            long startNanos,
            String failureReason) {
        InternalResourceKind internalKind = classify(request, response);
        if (!accepted(request, pageOrigin, internalKind)) {
            return;
        }
        if (recordedCount.get() >= options.maxTransactions()) {
            if (!maxTransactionsWarned) {
                maxTransactionsWarned = true;
                warn.accept("API capture reached its transaction cap of " + options.maxTransactions()
                        + "; further network transactions are being dropped.");
            }
            return;
        }
        recordedCount.incrementAndGet();
        long durationMillis = java.util.concurrent.TimeUnit.NANOSECONDS.toMillis(
                Math.max(0, System.nanoTime() - startNanos));

        Map<String, String> requestHeaders = secretHeaderReplacer.replaceSecrets(headers(request), sessionId);
        Map<String, String> responseHeaders = response == null
                ? Map.of()
                : secretHeaderReplacer.replaceSecrets(headers(response), sessionId);

        BodyRef requestBodyRef = bodyStore.store(
                requestBody, request.getContentType(), sessionDirectory, (int) options.maxBodyBytes());
        BodyRef responseBodyRef = response == null
                ? null
                : bodyStore.store(responseBody, response.getContentType(), sessionDirectory, (int) options.maxBodyBytes());

        HttpRequestRecord requestRecord = new HttpRequestRecord(
                request.getMethod() == null ? "" : request.getMethod().name(),
                request.getUri(),
                requestHeaders,
                requestBodyRef);
        HttpResponseRecord responseRecord = response == null
                ? null
                : new HttpResponseRecord(response.getStatus(), responseHeaders, responseBodyRef);
        NetworkTiming timing = new NetworkTiming(null, null, null, null, null, Duration.ofMillis(durationMillis));

        RecordedTransaction transaction = new RecordedTransaction(
                String.valueOf(transactionId),
                internalKind.canonicalKind(),
                requestRecord,
                responseRecord,
                timing,
                failureReason,
                pageOrigin);
        recordEndpoint(transaction);
        sink.accept(transaction);
    }

    private void recordEndpoint(RecordedTransaction transaction) {
        synchronized (lastEndpointsLock) {
            lastEndpoints.addFirst(transaction.request().method() + " " + transaction.request().url());
            while (lastEndpoints.size() > 20) {
                lastEndpoints.removeLast();
            }
        }
    }

    private boolean accepted(HttpRequest request, String pageOrigin, InternalResourceKind resourceKind) {
        if (!options.includeAssetTypes() && resourceKind.isAsset()) {
            return false;
        }
        if (options.firstPartyOnly() && !isFirstParty(request.getUri(), pageOrigin)) {
            return false;
        }
        String url = request.getUri();
        boolean included = options.urlIncludeGlobs().isEmpty()
                || options.urlIncludeGlobs().stream().anyMatch(glob -> matchesGlob(url, glob));
        if (!included) {
            return false;
        }
        boolean excluded = options.urlExcludeGlobs().stream().anyMatch(glob -> matchesGlob(url, glob));
        return !excluded;
    }

    /**
     * Finer-grained internal resource classification used only for asset-type filtering.
     * {@link com.shaft.capture.model.network.ResourceKind} (T1's canonical, persisted vocabulary)
     * does not distinguish stylesheet/script/image/font/media from other non-API traffic, so this
     * richer classification is translated down to {@link #canonicalKind()} at the point a
     * transaction is recorded.
     */
    private enum InternalResourceKind {
        DOCUMENT(ResourceKind.DOCUMENT, false),
        XHR_FETCH(ResourceKind.XHR, false),
        STYLESHEET(ResourceKind.OTHER, true),
        SCRIPT(ResourceKind.OTHER, true),
        IMAGE(ResourceKind.OTHER, true),
        FONT(ResourceKind.OTHER, true),
        MEDIA(ResourceKind.OTHER, true),
        WEBSOCKET(ResourceKind.WEBSOCKET_HANDSHAKE, false),
        OTHER(ResourceKind.OTHER, false);

        private final ResourceKind canonicalKind;
        private final boolean asset;

        InternalResourceKind(ResourceKind canonicalKind, boolean asset) {
            this.canonicalKind = canonicalKind;
            this.asset = asset;
        }

        ResourceKind canonicalKind() {
            return canonicalKind;
        }

        boolean isAsset() {
            return asset;
        }
    }

    private static InternalResourceKind classify(HttpRequest request, HttpResponse response) {
        String accept = header(request, "Accept");
        String contentType = response == null ? "" : value(response.getContentType());
        if (contentType.isBlank()) {
            contentType = header(request, "Content-Type");
        }
        String path = pathOf(request.getUri());

        if (isDocument(contentType, accept, path)) {
            return InternalResourceKind.DOCUMENT;
        }
        if (isStylesheet(contentType, path)) {
            return InternalResourceKind.STYLESHEET;
        }
        if (isScript(contentType, path)) {
            return InternalResourceKind.SCRIPT;
        }
        if (isImage(contentType, path)) {
            return InternalResourceKind.IMAGE;
        }
        if (isFont(contentType, path)) {
            return InternalResourceKind.FONT;
        }
        if (isMedia(contentType, path)) {
            return InternalResourceKind.MEDIA;
        }
        if (isWebSocket(request)) {
            return InternalResourceKind.WEBSOCKET;
        }
        if (isXhrOrFetch(contentType, accept, request)) {
            return InternalResourceKind.XHR_FETCH;
        }
        if (isBlankOrHtmlPath(path)) {
            return InternalResourceKind.DOCUMENT;
        }
        return InternalResourceKind.XHR_FETCH;
    }

    private static boolean isDocument(String contentType, String accept, String path) {
        return containsAny(contentType, "text/html") || (accept.contains("text/html") && path.isBlank());
    }

    private static boolean isStylesheet(String contentType, String path) {
        return containsAny(contentType, "text/css") || path.endsWith(".css");
    }

    private static boolean isScript(String contentType, String path) {
        return containsAny(contentType, "javascript", "ecmascript") || path.endsWith(".js") || path.endsWith(".mjs");
    }

    private static boolean isImage(String contentType, String path) {
        return containsAny(contentType, "image/")
                || hasExtension(path, ".png", ".jpg", ".jpeg", ".gif", ".svg", ".webp", ".ico");
    }

    private static boolean isFont(String contentType, String path) {
        return containsAny(contentType, "font/", "application/font")
                || hasExtension(path, ".woff", ".woff2", ".ttf", ".otf", ".eot");
    }

    private static boolean isMedia(String contentType, String path) {
        return containsAny(contentType, "audio/", "video/") || hasExtension(path, ".mp4", ".webm", ".mp3", ".wav");
    }

    private static boolean isWebSocket(HttpRequest request) {
        return containsAny(header(request, "Upgrade"), "websocket");
    }

    private static boolean isXhrOrFetch(String contentType, String accept, HttpRequest request) {
        return containsAny(contentType, "json", "xml", "text/plain") || containsAny(accept, "json")
                || !header(request, "X-Requested-With").isBlank();
    }

    private static boolean isBlankOrHtmlPath(String path) {
        return path.isBlank() || hasExtension(path, ".htm", ".html");
    }

    private static boolean containsAny(String haystack, String... needles) {
        String lower = haystack.toLowerCase(Locale.ROOT);
        for (String needle : needles) {
            if (lower.contains(needle)) {
                return true;
            }
        }
        return false;
    }

    private static boolean hasExtension(String path, String... extensions) {
        String lower = path.toLowerCase(Locale.ROOT);
        for (String extension : extensions) {
            if (lower.endsWith(extension)) {
                return true;
            }
        }
        return false;
    }

    private static String pathOf(String uri) {
        try {
            String path = URI.create(uri).getPath();
            return path == null ? "" : path;
        } catch (RuntimeException exception) {
            return "";
        }
    }

    private static String header(HttpRequest request, String name) {
        return value(request.getHeader(name));
    }

    private boolean isFirstParty(String requestUrl, String pageOrigin) {
        if (pageOrigin.isBlank()) {
            return true;
        }
        String requestOrigin = originOf(requestUrl);
        return requestOrigin.isBlank() || requestOrigin.equalsIgnoreCase(pageOrigin);
    }

    private static String originOf(String url) {
        try {
            URI uri = new URI(url);
            String scheme = uri.getScheme();
            String host = uri.getHost();
            if (scheme == null || host == null) {
                return "";
            }
            int port = uri.getPort();
            return scheme.toLowerCase(Locale.ROOT) + "://" + host.toLowerCase(Locale.ROOT)
                    + (port == -1 ? "" : ":" + port);
        } catch (URISyntaxException | RuntimeException exception) {
            return "";
        }
    }

    private String currentPageOrigin() {
        try {
            return originOf(driver.getCurrentUrl());
        } catch (RuntimeException exception) {
            return "";
        }
    }

    private static boolean matchesGlob(String url, String glob) {
        if (glob == null || glob.isBlank()) {
            return false;
        }
        String regex = globToRegex(glob.trim());
        try {
            return url.matches(regex);
        } catch (RuntimeException exception) {
            return false;
        }
    }

    private static String globToRegex(String glob) {
        StringBuilder regex = new StringBuilder();
        for (int i = 0; i < glob.length(); i++) {
            char c = glob.charAt(i);
            switch (c) {
                case '*' -> regex.append(".*");
                case '?' -> regex.append('.');
                case '.', '(', ')', '+', '|', '^', '$', '@', '%', '[', ']', '{', '}', '\\' ->
                        regex.append('\\').append(c);
                default -> regex.append(c);
            }
        }
        return regex.toString();
    }

    private static byte[] copyRequestBody(HttpRequest request) {
        try {
            byte[] body = Contents.bytes(request.getContent());
            request.setContent(Contents.bytes(body));
            return body;
        } catch (RuntimeException exception) {
            return new byte[0];
        }
    }

    private static byte[] copyResponseBody(HttpResponse response) {
        if (response == null) {
            return new byte[0];
        }
        try {
            byte[] body = Contents.bytes(response.getContent());
            response.setContent(Contents.bytes(body));
            return body;
        } catch (RuntimeException exception) {
            return new byte[0];
        }
    }

    private static Map<String, String> headers(HttpRequest request) {
        Map<String, String> headers = new LinkedHashMap<>();
        request.forEachHeader(headers::put);
        return headers;
    }

    private static Map<String, String> headers(HttpResponse response) {
        Map<String, String> headers = new LinkedHashMap<>();
        response.forEachHeader(headers::put);
        return headers;
    }

    private static boolean isTraceNetworkEnabled() {
        try {
            return SHAFT.Properties.reporting != null
                    && SHAFT.Properties.reporting.traceEnabled()
                    && SHAFT.Properties.reporting.traceIncludeNetwork();
        } catch (RuntimeException exception) {
            return false;
        }
    }

    private static String value(String value) {
        return value == null ? "" : value;
    }

    /**
     * One redacted, replay-safe network transaction observed by this recorder, ready to be
     * wrapped into a {@code CaptureEvent.NetworkEvent} once the caller assigns an
     * {@code EventContext} (sequence, page, timestamp).
     *
     * @param transactionId stable identifier correlating request and response evidence
     * @param resourceKind canonical (T1) resource classification
     * @param request sanitized outbound request evidence
     * @param response sanitized inbound response evidence, or {@code null} when the exchange failed
     *                 before a response was observed
     * @param timing timing evidence; only the total wall-clock duration is populated
     * @param failureReason safe failure reason, blank when the exchange completed
     * @param initiatorPageUrl sanitized URL of the page that initiated the transaction
     */
    public record RecordedTransaction(
            String transactionId,
            ResourceKind resourceKind,
            HttpRequestRecord request,
            HttpResponseRecord response,
            NetworkTiming timing,
            String failureReason,
            String initiatorPageUrl) {
    }
}
