package com.shaft.driver.internal.DriverFactory;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.SessionNotCreatedException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

@Test(singleThreaded = true)
public class RemoteGridPreflightUnitTest {
    private final String savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
    private final String savedTargetBrowserName = SHAFT.Properties.web.targetBrowserName();

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        SHAFT.Properties.platform.set()
                .executionAddress(savedExecutionAddress)
                .remotePreflightEnabled(false)
                .remoteAdaptiveSessionThrottling(false)
                .remotePreflightFailFast(false)
                .remotePreflightTimeoutSeconds(5);
        SHAFT.Properties.web.set().targetBrowserName(savedTargetBrowserName);
        RemoteGridPreflight.resetForTests();
        Properties.clearForCurrentThread();
    }

    @Test
    public void inspectShouldReadStatusAndGraphqlCapacityAndAttachSummary() throws Exception {
        HttpServer server = gridServer(statusWithChromeSlots(), graphqlQueue(3));
        server.start();
        try (MockedStatic<ReportManagerHelper> reportManagerHelper = Mockito.mockStatic(ReportManagerHelper.class)) {
            var report = RemoteGridPreflight.inspect(gridUrl(server), chromeCapabilities());

            SHAFT.Validations.assertThat().object(report.gridMetadataAvailable()).isEqualTo(true).perform();
            SHAFT.Validations.assertThat().object(report.matchingSlots()).isEqualTo(2).perform();
            SHAFT.Validations.assertThat().object(report.matchingAvailableSlots()).isEqualTo(1).perform();
            SHAFT.Validations.assertThat().object(report.queueDepth()).isEqualTo(3).perform();
            reportManagerHelper.verify(() -> ReportManagerHelper.attach(
                    Mockito.eq("Text"),
                    Mockito.eq("Selenium Grid Preflight"),
                    Mockito.contains("matchingAvailableSlots=1")));
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void beforeSessionShouldFailFastWhenGridHasNoMatchingSlots() throws Exception {
        HttpServer server = gridServer(statusWithOnlyFirefoxSlot(), graphqlQueue(0));
        server.start();
        SHAFT.Properties.platform.set()
                .remotePreflightEnabled(true)
                .remotePreflightFailFast(true)
                .remotePreflightTimeoutSeconds(1);
        try (MockedStatic<ReportManagerHelper> ignored = Mockito.mockStatic(ReportManagerHelper.class)) {
            SessionNotCreatedException failure = org.testng.Assert.expectThrows(SessionNotCreatedException.class,
                    () -> RemoteGridPreflight.beforeSession(gridUrl(server), chromeCapabilities()));

            SHAFT.Validations.assertThat().object(failure.getMessage()).contains("No Selenium Grid slot matches").perform();
            SHAFT.Validations.assertThat().object(RemoteGridPreflight.classifyRemoteSessionFailure(failure))
                    .isEqualTo("incompatible capabilities").perform();
        } finally {
            server.stop(0);
        }
    }

    @Test
    public void beforeSessionShouldThrottleConcurrentLocalSessionCreationToMatchingGridCapacity() throws Exception {
        HttpServer server = gridServer(statusWithOneFreeChromeSlot(), graphqlQueue(0));
        server.start();
        SHAFT.Properties.platform.set()
                .remotePreflightEnabled(true)
                .remoteAdaptiveSessionThrottling(true)
                .remotePreflightTimeoutSeconds(1);
        var executor = Executors.newSingleThreadExecutor();
        try (MockedStatic<ReportManagerHelper> ignored = Mockito.mockStatic(ReportManagerHelper.class);
             var firstPermit = RemoteGridPreflight.beforeSession(gridUrl(server), chromeCapabilities())) {
            var secondPermit = executor.submit(() -> RemoteGridPreflight.beforeSession(gridUrl(server), chromeCapabilities()));

            SHAFT.Validations.assertThat().object(secondPermit.isDone()).isEqualTo(false).perform();
            firstPermit.close();

            try (var acquiredSecondPermit = secondPermit.get(2, TimeUnit.SECONDS)) {
                SHAFT.Validations.assertThat().object(acquiredSecondPermit).isNotNull().perform();
            }
        } finally {
            executor.shutdownNow();
            server.stop(0);
        }
    }

    private static MutableCapabilities chromeCapabilities() {
        var capabilities = new MutableCapabilities();
        capabilities.setCapability("browserName", "chrome");
        return capabilities;
    }

    private static String gridUrl(HttpServer server) {
        return "http://127.0.0.1:" + server.getAddress().getPort() + "/wd/hub";
    }

    private static HttpServer gridServer(String statusBody, String graphqlBody) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/status", exchange -> sendJson(exchange, statusBody));
        server.createContext("/graphql", exchange -> sendJson(exchange, graphqlBody));
        return server;
    }

    private static void sendJson(HttpExchange exchange, String body) throws IOException {
        byte[] response = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(200, response.length);
        exchange.getResponseBody().write(response);
        exchange.close();
    }

    private static String statusWithChromeSlots() {
        return """
                {"value":{"ready":true,"nodes":[{"status":"UP","slots":[
                {"stereotype":{"browserName":"chrome"},"session":null},
                {"stereotype":{"browserName":"chrome"},"session":{"id":"busy"}},
                {"stereotype":{"browserName":"firefox"},"session":null}
                ]}]}}""";
    }

    private static String statusWithOneFreeChromeSlot() {
        return """
                {"value":{"ready":true,"nodes":[{"status":"UP","slots":[
                {"stereotype":{"browserName":"chrome"},"session":null}
                ]}]}}""";
    }

    private static String statusWithOnlyFirefoxSlot() {
        return """
                {"value":{"ready":true,"nodes":[{"status":"UP","slots":[
                {"stereotype":{"browserName":"firefox"},"session":null}
                ]}]}}""";
    }

    private static String graphqlQueue(int queueDepth) {
        return "{\"data\":{\"grid\":{\"sessionQueueSize\":" + queueDepth + "}}}";
    }
}
