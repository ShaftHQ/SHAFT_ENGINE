package com.shaft.capture.proxy;

import io.netty.buffer.ByteBuf;
import io.netty.handler.codec.http.HttpContent;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpObject;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.HttpResponse;
import io.netty.handler.codec.http.LastHttpContent;
import org.littleshoot.proxy.HttpFiltersAdapter;

import java.io.ByteArrayOutputStream;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Accumulates one HTTP(S) request/response pair as it streams through the proxy (Netty delivers a
 * message as an {@link HttpRequest}/{@link HttpResponse} head followed by zero or more
 * {@link HttpContent} chunks and a terminal {@link LastHttpContent}) and emits one
 * {@link ProxyTransaction} to the sink once the response is fully received.
 *
 * <p>Tier-3 (certificate-pinned) hosts: {@link #proxyToServerAllowMitm()} consults a shared,
 * mutable {@code pinnedHosts} set (see {@link ApiCaptureProxyServer#pinnedHosts()}) and returns
 * {@code false} for any host in it, so LittleProxy tunnels that connection's raw encrypted bytes
 * through untouched instead of attempting (and failing) MITM interception -- the app keeps
 * working, just without capture for that host. Populating the set currently requires a caller to
 * add a known-pinned host proactively (e.g. from prior knowledge of the app under test); this
 * module has no direct signal for a device-side TLS rejection to add a host automatically.
 */
final class CapturingHttpFilters extends HttpFiltersAdapter {
    private final Consumer<ProxyTransaction> sink;
    private final Consumer<String> warn;
    private final Set<String> pinnedHosts;
    private final ByteArrayOutputStream requestBody = new ByteArrayOutputStream();
    private final ByteArrayOutputStream responseBody = new ByteArrayOutputStream();
    private HttpResponse capturedResponseHead;

    CapturingHttpFilters(HttpRequest originalRequest, Consumer<ProxyTransaction> sink, Consumer<String> warn,
            Set<String> pinnedHosts) {
        super(originalRequest);
        this.sink = sink;
        this.warn = warn;
        this.pinnedHosts = pinnedHosts;
    }

    @Override
    public boolean proxyToServerAllowMitm() {
        String host = originalRequest.headers().get(io.netty.handler.codec.http.HttpHeaderNames.HOST);
        if (host != null && pinnedHosts.contains(hostOnly(host))) {
            warn.accept("\"" + host + "\" is a known certificate-pinned host; tunneling without capture.");
            return false;
        }
        return true;
    }

    private static String hostOnly(String hostHeader) {
        int colonIndex = hostHeader.indexOf(':');
        return colonIndex < 0 ? hostHeader : hostHeader.substring(0, colonIndex);
    }

    @Override
    public HttpResponse clientToProxyRequest(HttpObject httpObject) {
        appendContent(httpObject, requestBody);
        return null; // null lets the request proceed unmodified
    }

    @Override
    public HttpObject serverToProxyResponse(HttpObject httpObject) {
        if (httpObject instanceof HttpResponse response) {
            capturedResponseHead = response;
        }
        appendContent(httpObject, responseBody);
        if (httpObject instanceof LastHttpContent) {
            emit();
        }
        return httpObject;
    }

    private static void appendContent(HttpObject httpObject, ByteArrayOutputStream buffer) {
        if (!(httpObject instanceof HttpContent content)) {
            return;
        }
        ByteBuf byteBuf = content.content();
        int length = byteBuf.readableBytes();
        if (length == 0) {
            return;
        }
        byte[] bytes = new byte[length];
        byteBuf.getBytes(byteBuf.readerIndex(), bytes);
        buffer.write(bytes, 0, bytes.length);
    }

    private void emit() {
        try {
            int statusCode = capturedResponseHead == null ? 0 : capturedResponseHead.status().code();
            Map<String, String> responseHeaders = capturedResponseHead == null
                    ? Map.of() : headersOf(capturedResponseHead.headers());
            sink.accept(new ProxyTransaction(
                    originalRequest.method().name(),
                    absoluteUrl(originalRequest),
                    headersOf(originalRequest.headers()),
                    requestBody.toByteArray(),
                    statusCode,
                    responseHeaders,
                    responseBody.toByteArray()));
        } catch (RuntimeException failure) {
            warn.accept("A captured transaction could not be recorded: " + safeMessage(failure));
        }
    }

    /**
     * Reconstructs the absolute URL for a request. Plain (non-MITM'd) HTTP proxy requests already
     * carry an absolute URI; requests decrypted from a MITM'd HTTPS CONNECT tunnel carry only the
     * origin-relative path, with the real host available from the Host header.
     */
    private static String absoluteUrl(HttpRequest request) {
        String uri = request.uri();
        if (uri.startsWith("http://") || uri.startsWith("https://")) {
            return uri;
        }
        String host = request.headers().get(io.netty.handler.codec.http.HttpHeaderNames.HOST);
        return "https://" + (host == null || host.isBlank() ? "unknown-host" : host) + uri;
    }

    private static Map<String, String> headersOf(HttpHeaders headers) {
        Map<String, String> map = new LinkedHashMap<>();
        headers.forEach(entry -> map.put(entry.getKey(), entry.getValue()));
        return map;
    }

    private static String safeMessage(RuntimeException exception) {
        String message = exception.getMessage();
        return message == null || message.isBlank() ? exception.getClass().getSimpleName() : message;
    }
}
