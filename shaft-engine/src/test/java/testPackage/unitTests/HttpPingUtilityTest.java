package testPackage.unitTests;

import com.shaft.tools.internal.support.HttpPingUtility;
import com.sun.net.httpserver.HttpServer;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.io.IOException;
import java.net.InetSocketAddress;

import static org.testng.Assert.*;

/**
 * Unit tests for HttpPingUtility — uses an in-process JDK HttpServer so no
 * external network access is required.
 */
public class HttpPingUtilityTest {

    private HttpServer server;
    private int port;

    @BeforeClass
    public void startServer() throws IOException {
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

    @AfterClass(alwaysRun = true)
    public void stopServer() {
        if (server != null) server.stop(0);
    }

    @Test(groups = {"unit"})
    public void getStatusCode_returnsOkForAvailableEndpoint() throws IOException {
        int status = HttpPingUtility.getStatusCode("http://localhost:" + port + "/status/", 3000);
        assertEquals(status, 200);
    }

    @Test(groups = {"unit"})
    public void getStatusCode_returnsErrorCodeForBadEndpoint() throws IOException {
        int status = HttpPingUtility.getStatusCode("http://localhost:" + port + "/error/", 3000);
        assertEquals(status, 503);
    }

    @Test(groups = {"unit"}, expectedExceptions = IOException.class)
    public void getStatusCode_throwsWhenConnectionRefused() throws IOException {
        // Port 1 is blocked/refused on every OS — connection will be refused immediately
        HttpPingUtility.getStatusCode("http://localhost:1/status/", 1000);
    }
}
