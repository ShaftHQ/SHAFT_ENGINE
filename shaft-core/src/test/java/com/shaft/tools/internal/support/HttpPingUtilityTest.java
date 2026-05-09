package com.shaft.tools.internal.support;

import com.sun.net.httpserver.HttpServer;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.io.IOException;
import java.net.InetSocketAddress;

class HttpPingUtilityTest {

    private HttpServer server;
    private int port;

    @BeforeClass
    void startServer() throws IOException {
        server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/status/", exchange -> {
            byte[] body = "ok".getBytes();
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        server.createContext("/error/", exchange -> {
            exchange.sendResponseHeaders(503, -1);
            exchange.close();
        });
        server.start();
        port = server.getAddress().getPort();
    }

    @AfterClass
    void stopServer() {
        if (server != null) server.stop(0);
    }

    @Test
    void shouldReturn200ForResponsiveServer() throws IOException {
        int status = HttpPingUtility.getStatusCode("http://localhost:" + port + "/status/", 3000);
        Assert.assertEquals(status, 200);
    }

    @Test
    void shouldReturn503ForServerThatReturns503() throws IOException {
        int status = HttpPingUtility.getStatusCode("http://localhost:" + port + "/error/", 3000);
        Assert.assertEquals(status, 503);
    }

    @Test
    void shouldThrowIOExceptionOnConnectionRefused() {
        Assert.expectThrows(IOException.class,
            () -> HttpPingUtility.getStatusCode("http://localhost:1/status/", 1000));
    }
}
