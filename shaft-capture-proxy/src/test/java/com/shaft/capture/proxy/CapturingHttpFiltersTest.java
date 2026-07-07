package com.shaft.capture.proxy;

import io.netty.buffer.Unpooled;
import io.netty.handler.codec.http.DefaultFullHttpRequest;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.DefaultHttpContent;
import io.netty.handler.codec.http.DefaultLastHttpContent;
import io.netty.handler.codec.http.HttpMethod;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.HttpVersion;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CapturingHttpFiltersTest {

    @Test
    void capturesAFullRequestResponsePairIncludingBodies() {
        List<ProxyTransaction> captured = new ArrayList<>();
        var request = new DefaultFullHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.POST, "/orders",
                Unpooled.copiedBuffer("{\"item\":\"widget\"}", StandardCharsets.UTF_8));
        request.headers().set("Host", "api.example.test");
        request.headers().set("Content-Type", "application/json");

        CapturingHttpFilters filters = new CapturingHttpFilters(request, captured::add, warning -> { });

        filters.clientToProxyRequest(request);

        var response = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.CREATED,
                Unpooled.copiedBuffer("{\"id\":\"abc-123\"}", StandardCharsets.UTF_8));
        response.headers().set("Content-Type", "application/json");
        filters.serverToProxyResponse(response);

        assertEquals(1, captured.size());
        ProxyTransaction transaction = captured.get(0);
        assertEquals("POST", transaction.method());
        assertEquals("https://api.example.test/orders", transaction.url());
        assertEquals("{\"item\":\"widget\"}", new String(transaction.requestBody(), StandardCharsets.UTF_8));
        assertEquals(201, transaction.statusCode());
        assertEquals("{\"id\":\"abc-123\"}", new String(transaction.responseBody(), StandardCharsets.UTF_8));
        assertEquals("application/json", transaction.requestHeaders().get("Content-Type"));
        assertEquals("application/json", transaction.responseHeaders().get("Content-Type"));
    }

    @Test
    void accumulatesChunkedResponseBodyAcrossMultipleContentObjects() {
        List<ProxyTransaction> captured = new ArrayList<>();
        var request = new DefaultFullHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/stream",
                Unpooled.EMPTY_BUFFER);
        request.headers().set("Host", "api.example.test");

        CapturingHttpFilters filters = new CapturingHttpFilters(request, captured::add, warning -> { });
        filters.clientToProxyRequest(request);

        var responseHead = new io.netty.handler.codec.http.DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK);
        filters.serverToProxyResponse(responseHead);
        filters.serverToProxyResponse(new DefaultHttpContent(Unpooled.copiedBuffer("{\"a\":1,", StandardCharsets.UTF_8)));
        filters.serverToProxyResponse(new DefaultLastHttpContent(Unpooled.copiedBuffer("\"b\":2}", StandardCharsets.UTF_8)));

        assertEquals(1, captured.size());
        assertEquals("{\"a\":1,\"b\":2}", new String(captured.get(0).responseBody(), StandardCharsets.UTF_8));
        assertEquals(200, captured.get(0).statusCode());
    }

    @Test
    void relativeUriIsReconstructedIntoAnAbsoluteHttpsUrlFromTheHostHeader() {
        List<ProxyTransaction> captured = new ArrayList<>();
        var request = new DefaultFullHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/v1/me", Unpooled.EMPTY_BUFFER);
        request.headers().set("Host", "secure.example.test");

        CapturingHttpFilters filters = new CapturingHttpFilters(request, captured::add, warning -> { });
        filters.clientToProxyRequest(request);
        filters.serverToProxyResponse(new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK, Unpooled.EMPTY_BUFFER));

        assertEquals("https://secure.example.test/v1/me", captured.get(0).url());
    }

    @Test
    void anAlreadyAbsoluteUriIsKeptAsIs() {
        List<ProxyTransaction> captured = new ArrayList<>();
        var request = new DefaultFullHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET,
                "http://api.example.test/plain", Unpooled.EMPTY_BUFFER);

        CapturingHttpFilters filters = new CapturingHttpFilters(request, captured::add, warning -> { });
        filters.clientToProxyRequest(request);
        filters.serverToProxyResponse(new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK, Unpooled.EMPTY_BUFFER));

        assertEquals("http://api.example.test/plain", captured.get(0).url());
    }

    @Test
    void noResponseYieldsZeroStatusCodeRatherThanCapturingNothing() {
        // A request with no response at all (e.g. connection dropped) currently never reaches
        // LastHttpContent, so no transaction is emitted -- this documents that behavior rather
        // than silently doing something else.
        List<ProxyTransaction> captured = new ArrayList<>();
        var request = new DefaultFullHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/never-responds", Unpooled.EMPTY_BUFFER);
        request.headers().set("Host", "api.example.test");

        CapturingHttpFilters filters = new CapturingHttpFilters(request, captured::add, warning -> { });
        filters.clientToProxyRequest(request);

        assertTrue(captured.isEmpty());
    }
}
