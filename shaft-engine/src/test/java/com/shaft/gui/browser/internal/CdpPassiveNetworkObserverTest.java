package com.shaft.gui.browser.internal;

import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.Command;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.Filter;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

public class CdpPassiveNetworkObserverTest {
    @Test(description = "Issue #6735: passive capture enables only the Network domain and never pauses requests")
    public void passiveCaptureShouldNeverEnableFetch() {
        DevTools devTools = Mockito.mock(DevTools.class);
        try (CdpPassiveNetworkObserver ignored = new CdpPassiveNetworkObserver(driver(devTools), next -> next)) {
            var commands = org.mockito.ArgumentCaptor.forClass(Command.class);
            Mockito.verify(devTools, Mockito.atLeastOnce()).send(commands.capture());
            Assert.assertEquals(commands.getAllValues().stream().map(Command::getMethod).toList(),
                    List.of("Network.setCacheDisabled", "Network.enable"));
        }
    }

    @Test(description = "Issue #6735: request bodies come from postDataEntries bytes, so binary parts stay exact")
    public void requestBodyShouldBeDecodedFromPostDataEntriesBytes() {
        byte[] binary = {(byte) 0x89, 'P', 'N', 'G', 0, (byte) 0xff, (byte) 0xfe};
        Map<String, Object> request = Map.of("method", "POST", "url", "https://example.test/upload",
                "headers", Map.of("Content-Type", "multipart/form-data; boundary=x"),
                "postData", "lossy string", "postDataEntries", List.of(
                        Map.of("bytes", Base64.getEncoder().encodeToString(new byte[]{1, 2})),
                        Map.of("bytes", Base64.getEncoder().encodeToString(binary))));

        HttpRequest observed = CdpPassiveNetworkObserver.request(request);

        byte[] expected = new byte[binary.length + 2];
        expected[0] = 1;
        expected[1] = 2;
        System.arraycopy(binary, 0, expected, 2, binary.length);
        Assert.assertEquals(Contents.bytes(observed.getContent()), expected);
        Assert.assertEquals(observed.getHeader("Content-Type"), "multipart/form-data; boundary=x");
    }

    @Test
    public void eventsShouldDriveTheObservationFilterWithRealStatusAndFailures() throws Exception {
        DevTools devTools = Mockito.mock(DevTools.class);
        Mockito.when(devTools.send(Mockito.argThat(command -> command != null
                && "Network.getResponseBody".equals(command.getMethod()))))
                .thenReturn(null);
        CompletableFuture<HttpResponse> finished = new CompletableFuture<>();
        CompletableFuture<Throwable> failed = new CompletableFuture<>();
        Filter recording = next -> request -> {
            try {
                HttpResponse response = next.execute(request);
                if (request.getUri().endsWith("/ok")) {
                    finished.complete(response);
                }
                return response;
            } catch (RuntimeException e) {
                failed.complete(e);
                throw e;
            }
        };
        try (CdpPassiveNetworkObserver observer = new CdpPassiveNetworkObserver(driver(devTools), recording)) {
            observer.requestWillBeSent(Map.of("requestId", "1", "request",
                    Map.of("method", "GET", "url", "https://example.test/ok", "headers", Map.of())));
            observer.responseReceived(Map.of("requestId", "1", "response",
                    Map.of("status", 201, "mimeType", "image/png", "headers", Map.of("Content-Type", "image/png"))));
            observer.loadingFinished(Map.of("requestId", "1", "encodedDataLength", 10));
            observer.requestWillBeSent(Map.of("requestId", "2", "request",
                    Map.of("method", "GET", "url", "https://example.test/down", "headers", Map.of())));
            observer.loadingFailed(Map.of("requestId", "2", "errorText", "net::ERR_CONNECTION_REFUSED"));

            Assert.assertEquals(finished.get(5, TimeUnit.SECONDS).getStatus(), 201);
            Assert.assertEquals(finished.get().getHeader("Content-Type"), "image/png");
            Assert.assertEquals(failed.get(5, TimeUnit.SECONDS).getMessage(), "net::ERR_CONNECTION_REFUSED");
        }
    }

    @Test(description = "Issue #6765: Selenium dispatches CDP events on a thread pool, so any event order must work")
    public void completionShouldNotDependOnTheOrderEventsAreHandled() throws Exception {
        List<List<String>> orders = List.of(
                List.of("request", "response", "finished"), List.of("request", "finished", "response"),
                List.of("response", "request", "finished"), List.of("response", "finished", "request"),
                List.of("finished", "request", "response"), List.of("finished", "response", "request"));
        for (List<String> order : orders) {
            CompletableFuture<HttpResponse> finished = new CompletableFuture<>();
            Filter recording = next -> request -> {
                HttpResponse response = next.execute(request);
                finished.complete(response);
                return response;
            };
            try (CdpPassiveNetworkObserver observer = new CdpPassiveNetworkObserver(
                    driver(Mockito.mock(DevTools.class)), recording)) {
                for (String step : order) {
                    switch (step) {
                        case "request" -> observer.requestWillBeSent(Map.of("requestId", "1", "request",
                                Map.of("method", "GET", "url", "https://example.test/ok", "headers", Map.of())));
                        case "response" -> observer.responseReceived(Map.of("requestId", "1", "response",
                                Map.of("status", 201, "mimeType", "image/png", "headers", Map.of())));
                        default -> observer.loadingFinished(Map.of("requestId", "1", "encodedDataLength", 10));
                    }
                }
                Assert.assertEquals(finished.get(5, TimeUnit.SECONDS).getStatus(), 201, "order " + order);
            }
        }
    }

    @Test(description = "Issue #6765: a failure handled before its request must still fail that request")
    public void failureShouldBeReportedWhateverTheEventOrder() throws Exception {
        for (boolean failureFirst : new boolean[]{true, false}) {
            CompletableFuture<Throwable> failed = new CompletableFuture<>();
            Filter recording = next -> request -> {
                try {
                    return next.execute(request);
                } catch (RuntimeException e) {
                    failed.complete(e);
                    throw e;
                }
            };
            try (CdpPassiveNetworkObserver observer = new CdpPassiveNetworkObserver(
                    driver(Mockito.mock(DevTools.class)), recording)) {
                Runnable request = () -> observer.requestWillBeSent(Map.of("requestId", "2", "request",
                        Map.of("method", "GET", "url", "https://example.test/down", "headers", Map.of())));
                Runnable failure = () -> observer.loadingFailed(Map.of("requestId", "2", "errorText", "net::ERR_FAILED"));
                (failureFirst ? failure : request).run();
                (failureFirst ? request : failure).run();
                Assert.assertEquals(failed.get(5, TimeUnit.SECONDS).getMessage(), "net::ERR_FAILED");
            }
        }
    }

    @Test
    public void onlyTextualResponsesShouldFetchBodies() {
        Assert.assertTrue(CdpPassiveNetworkObserver.textual(Map.of("mimeType", "application/json")));
        Assert.assertTrue(CdpPassiveNetworkObserver.textual(Map.of("mimeType", "text/html")));
        Assert.assertFalse(CdpPassiveNetworkObserver.textual(Map.of("mimeType", "image/png")));
        Assert.assertFalse(CdpPassiveNetworkObserver.textual(Map.of()));
    }

    private static WebDriver driver(DevTools devTools) {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class));
        Mockito.when(((HasDevTools) driver).getDevTools()).thenReturn(devTools);
        return driver;
    }
}
