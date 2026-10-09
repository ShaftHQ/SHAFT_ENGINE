package com.shaft.gui.browser.internal;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.Command;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.Event;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.json.Json;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.Filter;
import org.openqa.selenium.remote.http.HttpHandler;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.time.Duration;
import java.util.Base64;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;

/**
 * Observes browser traffic through the CDP {@code Network} domain without pausing requests.
 *
 * <p>Unlike Selenium's {@code NetworkInterceptor}, this observer never enables the {@code Fetch}
 * domain, so the browser sends every request exactly as the page built it. Binary multipart
 * uploads therefore reach the server byte for byte (issue #6735). Request bodies are read from
 * {@code postDataEntries} (base64 bytes) for the trace only.
 */
public final class CdpPassiveNetworkObserver implements AutoCloseable {
    static final int MAX_IN_FLIGHT = 256;
    static final long MAX_RESPONSE_BODY_BYTES = 1_048_576;
    private static final Duration BODY_TIMEOUT = Duration.ofSeconds(5);
    private final Map<String, Pending> pending = new ConcurrentHashMap<>();
    private final ExecutorService workers = Executors.newVirtualThreadPerTaskExecutor();
    private final HttpHandler handler;
    private final DevTools devTools;
    private volatile boolean closed;

    CdpPassiveNetworkObserver(WebDriver driver, Filter filter) {
        this.handler = filter.apply(this::awaitResponse);
        this.devTools = driver instanceof HasDevTools hasDevTools ? hasDevTools.getDevTools() : null;
        if (devTools == null) {
            workers.shutdownNow();
            throw new IllegalStateException("CDP DevTools is unavailable for passive network observation.");
        }
        // Bind the same window Selenium's pausing interceptor used. The no-arg form reattaches when
        // the stored handle differs, which drops the BiDi script channel capture is already using.
        devTools.createSessionIfThereIsNotOne(driver.getWindowHandle());
        devTools.addListener(event("Network.requestWillBeSent"), this::requestWillBeSent);
        devTools.addListener(event("Network.responseReceived"), this::responseReceived);
        devTools.addListener(event("Network.loadingFinished"), this::loadingFinished);
        devTools.addListener(event("Network.loadingFailed"), this::loadingFailed);
        // Selenium's pausing interceptor disables the HTTP cache before Fetch.enable.
        // Match that without enabling Fetch. setCacheDisabled also keeps a back-forward
        // restore from resuming a capture script whose channel has already died.
        devTools.send(new Command<>("Network.setCacheDisabled", Map.of("cacheDisabled", true)));
        devTools.send(new Command<>("Network.enable", Map.of()));
    }

    private static Event<Map<String, Object>> event(String method) {
        return new Event<>(method, input -> input.read(Json.MAP_TYPE));
    }

    void requestWillBeSent(Map<String, Object> event) {
        if (closed) {
            return;
        }
        String id = text(event.get("requestId"));
        Map<String, Object> redirect = map(event.get("redirectResponse"));
        if (!redirect.isEmpty()) {
            Pending previous = pending.remove(id);
            if (previous != null) {
                previous.complete(response(redirect, null));
            }
        }
        Map<String, Object> request = map(event.get("request"));
        if (request.isEmpty() || pending.size() >= MAX_IN_FLIGHT) {
            return;
        }
        HttpRequest observed = request(request);
        boolean bodyPending = Boolean.TRUE.equals(request.get("hasPostData")) && requestBody(request).length == 0;
        Pending exchange = new Pending(observed);
        if (pending.putIfAbsent(id, exchange) != null) {
            return;
        }
        try {
            workers.execute(() -> {
                if (bodyPending) {
                    byte[] body = requestPostData(id);
                    if (body != null && body.length > 0) {
                        observed.setContent(Contents.bytes(body));
                    }
                }
                exchange.run(handler);
            });
        } catch (RejectedExecutionException e) {
            pending.remove(id, exchange);
        }
    }

    void responseReceived(Map<String, Object> event) {
        Pending exchange = pending.get(text(event.get("requestId")));
        if (exchange != null) {
            exchange.response = map(event.get("response"));
        }
    }

    void loadingFinished(Map<String, Object> event) {
        String id = text(event.get("requestId"));
        Pending exchange = pending.remove(id);
        if (exchange == null) {
            return;
        }
        Map<String, Object> received = exchange.response;
        double encoded = number(event.get("encodedDataLength"));
        if (closed || !textual(received) || encoded > MAX_RESPONSE_BODY_BYTES) {
            exchange.complete(response(received, null));
            return;
        }
        try {
            workers.execute(() -> exchange.complete(response(received, responseBody(id))));
        } catch (RejectedExecutionException e) {
            exchange.complete(response(received, null));
        }
    }

    void loadingFailed(Map<String, Object> event) {
        Pending exchange = pending.remove(text(event.get("requestId")));
        if (exchange != null) {
            String reason = text(event.get("errorText"));
            exchange.fail(new IllegalStateException(reason.isBlank() ? "Network request failed" : reason));
        }
    }

    /** Reads a body Chrome did not inline (for example a Blob-backed multipart upload) for the trace only. */
    private byte[] requestPostData(String requestId) {
        try {
            Map<String, Object> body = devTools.send(new Command<>("Network.getRequestPostData",
                    Map.of("requestId", requestId), input -> input.read(Json.MAP_TYPE)), BODY_TIMEOUT);
            return decode(body, "postData");
        } catch (RuntimeException e) {
            return null;
        }
    }

    private static byte[] decode(Map<String, Object> body, String field) {
        if (body == null) {
            return null;
        }
        String content = text(body.get(field));
        return Boolean.TRUE.equals(body.get("base64Encoded"))
                ? Base64.getDecoder().decode(content) : content.getBytes(java.nio.charset.StandardCharsets.UTF_8);
    }

    private byte[] responseBody(String requestId) {
        try {
            Map<String, Object> body = devTools.send(new Command<>("Network.getResponseBody",
                    Map.of("requestId", requestId), input -> input.read(Json.MAP_TYPE)), BODY_TIMEOUT);
            return decode(body, "body");
        } catch (RuntimeException e) {
            return null;
        }
    }

    private HttpResponse awaitResponse(HttpRequest request) {
        Pending exchange = Pending.CURRENT.get();
        if (request == null || exchange == null) {
            return new HttpResponse();
        }
        try {
            return exchange.result.join();
        } catch (java.util.concurrent.CompletionException e) {
            throw e.getCause() instanceof RuntimeException cause ? cause : e;
        }
    }

    static HttpRequest request(Map<String, Object> request) {
        HttpMethod method;
        try {
            method = HttpMethod.valueOf(text(request.get("method")).toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException e) {
            method = HttpMethod.GET;
        }
        HttpRequest observed = new HttpRequest(method, text(request.get("url")));
        map(request.get("headers")).forEach((name, value) -> observed.addHeader(name, text(value)));
        byte[] body = requestBody(request);
        if (body.length > 0) {
            observed.setContent(Contents.bytes(body));
        }
        return observed;
    }

    static byte[] requestBody(Map<String, Object> request) {
        Object entries = request.get("postDataEntries");
        if (entries instanceof List<?> list && !list.isEmpty()) {
            java.io.ByteArrayOutputStream bytes = new java.io.ByteArrayOutputStream();
            for (Object entry : list) {
                String encoded = text(map(entry).get("bytes"));
                if (!encoded.isEmpty()) {
                    bytes.writeBytes(Base64.getDecoder().decode(encoded));
                }
            }
            return bytes.toByteArray();
        }
        String postData = text(request.get("postData"));
        return postData.getBytes(java.nio.charset.StandardCharsets.UTF_8);
    }

    static HttpResponse response(Map<String, Object> received, byte[] body) {
        HttpResponse response = new HttpResponse().setStatus((int) number(received.get("status")));
        map(received.get("headers")).forEach((name, value) -> response.addHeader(name, text(value)));
        if (body != null && body.length > 0) {
            response.setContent(Contents.bytes(body));
        }
        return response;
    }

    static boolean textual(Map<String, Object> received) {
        String mime = text(received.get("mimeType")).toLowerCase(Locale.ROOT);
        return mime.startsWith("text/") || mime.contains("json") || mime.contains("xml")
                || mime.contains("javascript") || mime.contains("x-www-form-urlencoded");
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> map(Object value) {
        return value instanceof Map<?, ?> map ? (Map<String, Object>) map : Map.of();
    }

    private static String text(Object value) {
        return value == null ? "" : String.valueOf(value);
    }

    private static double number(Object value) {
        return value instanceof Number number ? number.doubleValue() : 0;
    }

    @Override
    public void close() {
        closed = true;
        pending.values().forEach(exchange -> exchange.fail(new IllegalStateException("Network observation closed")));
        pending.clear();
        workers.shutdown();
        // Network.disable is deliberately not sent: other SHAFT listeners may share the CDP session.
    }

    private static final class Pending {
        private static final ThreadLocal<Pending> CURRENT = new ThreadLocal<>();
        private final HttpRequest request;
        private final CompletableFuture<HttpResponse> result = new CompletableFuture<>();
        private volatile Map<String, Object> response = Map.of();

        private Pending(HttpRequest request) {
            this.request = request;
        }

        private void run(HttpHandler handler) {
            CURRENT.set(this);
            try {
                handler.execute(request);
            } catch (RuntimeException ignored) {
                // A failed request is already recorded by the observation filter.
            } finally {
                CURRENT.remove();
            }
        }

        private void complete(HttpResponse value) {
            result.complete(value);
        }

        private void fail(RuntimeException error) {
            result.completeExceptionally(error);
        }
    }
}
