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
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;

/**
 * Applies mock, assert and verify rules through the CDP {@code Fetch} domain without ever rebuilding
 * a paused request (issue #6741).
 *
 * <p>Selenium's {@code NetworkInterceptor} continues every paused request with a body rebuilt from
 * {@code Request.postData}, a UTF-8 string, which corrupts binary multipart uploads. This interceptor
 * continues each paused request and response by id only, so the browser sends exactly the bytes the
 * page built. A mocked response is fulfilled without sending the request.
 */
final class CdpFetchRuleInterceptor implements AutoCloseable {
    private static final Duration COMMAND_TIMEOUT = Duration.ofSeconds(5);
    private static final long RESPONSE_TIMEOUT_SECONDS = 120;
    private static final String ID_ATTRIBUTE = "shaft.fetch.requestId";
    private final Map<String, CompletableFuture<Map<String, Object>>> responses = new ConcurrentHashMap<>();
    private final ExecutorService workers = Executors.newVirtualThreadPerTaskExecutor();
    private final HttpHandler handler;
    private final DevTools devTools;
    private volatile boolean closed;

    CdpFetchRuleInterceptor(WebDriver driver, Filter filter) {
        this.devTools = driver instanceof HasDevTools hasDevTools ? hasDevTools.getDevTools() : null;
        if (devTools == null) {
            workers.shutdownNow();
            throw new IllegalStateException("CDP DevTools is unavailable for network interception rules.");
        }
        this.handler = filter.apply(this::continueAndAwaitResponse);
        devTools.createSessionIfThereIsNotOne(driver.getWindowHandle());
        devTools.addListener(new Event<>("Fetch.requestPaused", input -> input.read(Json.MAP_TYPE)), this::paused);
        devTools.send(new Command<>("Network.setCacheDisabled", Map.of("cacheDisabled", true)));
        devTools.send(new Command<>("Fetch.enable", Map.of("patterns", List.of(
                Map.of("urlPattern", "*", "requestStage", "Request"),
                Map.of("urlPattern", "*", "requestStage", "Response")))));
    }

    void paused(Map<String, Object> event) {
        if (closed) {
            // DevTools cannot drop one listener; a closed interceptor leaves paused requests to its successor.
            return;
        }
        String id = text(event.get("requestId"));
        boolean responseStage = event.containsKey("responseStatusCode") || event.containsKey("responseErrorReason");
        if (responseStage) {
            CompletableFuture<Map<String, Object>> waiting = responses.get(id);
            if (waiting == null || !waiting.complete(event)) {
                continueUnchanged(id);
            }
            return;
        }
        HttpRequest request = CdpPassiveNetworkObserver.request(map(event.get("request")));
        request.setAttribute(ID_ATTRIBUTE, id);
        try {
            workers.execute(() -> handle(id, request));
        } catch (RejectedExecutionException e) {
            continueUnchanged(id);
        }
    }

    private void handle(String id, HttpRequest request) {
        HttpResponse response = null;
        try {
            response = handler.execute(request);
        } catch (RuntimeException | AssertionError ignored) {
            // The observation filter records the failure; the browser must still get its response.
        }
        CompletableFuture<Map<String, Object>> sent = responses.remove(id);
        if (sent != null) {
            // The real request was sent: release its paused response exactly as the server returned it.
            continueUnchanged(id);
        } else if (response != null) {
            fulfill(id, response);
        } else {
            send("Fetch.failRequest", Map.of("requestId", id, "errorReason", "Failed"));
        }
    }

    /** Terminal handler: continue the paused request by id only (never rebuilt) and wait for its response. */
    private HttpResponse continueAndAwaitResponse(HttpRequest request) {
        String id = text(request.getAttribute(ID_ATTRIBUTE));
        CompletableFuture<Map<String, Object>> response = new CompletableFuture<>();
        responses.put(id, response);
        continueUnchanged(id);
        Map<String, Object> paused;
        try {
            paused = response.get(RESPONSE_TIMEOUT_SECONDS, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("Interrupted while waiting for a network response", e);
        } catch (java.util.concurrent.ExecutionException | java.util.concurrent.TimeoutException e) {
            responses.remove(id);
            throw new IllegalStateException("Network response was not received", e);
        }
        String error = text(paused.get("responseErrorReason"));
        if (!error.isBlank()) {
            throw new IllegalStateException(error);
        }
        return response(id, paused);
    }

    private HttpResponse response(String id, Map<String, Object> paused) {
        HttpResponse response = new HttpResponse().setStatus((int) number(paused.get("responseStatusCode")));
        Map<String, Object> received = new java.util.HashMap<>();
        for (Object header : list(paused.get("responseHeaders"))) {
            String name = text(map(header).get("name"));
            String value = text(map(header).get("value"));
            response.addHeader(name, value);
            if ("content-type".equalsIgnoreCase(name)) {
                received.put("mimeType", value);
            }
        }
        if (CdpPassiveNetworkObserver.textual(received)) {
            byte[] body = responseBody(id);
            if (body != null && body.length <= CdpPassiveNetworkObserver.MAX_RESPONSE_BODY_BYTES) {
                response.setContent(Contents.bytes(body));
            }
        }
        return response;
    }

    private byte[] responseBody(String id) {
        try {
            Map<String, Object> body = devTools.send(new Command<>("Fetch.getResponseBody",
                    Map.of("requestId", id), input -> input.read(Json.MAP_TYPE)), COMMAND_TIMEOUT);
            String content = text(body.get("body"));
            return Boolean.TRUE.equals(body.get("base64Encoded")) ? Base64.getDecoder().decode(content)
                    : content.getBytes(java.nio.charset.StandardCharsets.UTF_8);
        } catch (RuntimeException e) {
            return null;
        }
    }

    private void fulfill(String id, HttpResponse response) {
        List<Map<String, String>> headers = new ArrayList<>();
        response.forEachHeader((name, value) -> headers.add(Map.of("name", name, "value", value)));
        byte[] body = Contents.bytes(response.getContent());
        int status = response.getStatus() <= 0 ? 200 : response.getStatus();
        send("Fetch.fulfillRequest", Map.of("requestId", id, "responseCode", status,
                "responseHeaders", headers, "body", Base64.getEncoder().encodeToString(body)));
    }

    private void continueUnchanged(String id) {
        send("Fetch.continueRequest", Map.of("requestId", id));
    }

    private void send(String method, Map<String, Object> params) {
        try {
            devTools.send(new Command<>(method, params), COMMAND_TIMEOUT);
        } catch (RuntimeException ignored) {
            // The request was already released, or the target closed; nothing is left paused.
        }
    }

    private static List<?> list(Object value) {
        return value instanceof List<?> items ? items : List.of();
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
        responses.values().forEach(waiting -> waiting.completeExceptionally(
                new IllegalStateException("Network interception closed")));
        responses.clear();
        workers.shutdown();
        send("Fetch.disable", Map.of());
    }
}
