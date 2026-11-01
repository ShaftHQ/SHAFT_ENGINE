package com.shaft.gui.browser.internal;

import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.Command;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.Filter;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.time.Duration;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

/**
 * Unit coverage for {@link CdpFetchRuleInterceptor} (issue #7077). Uses a mocked DevTools session —
 * no live browser.
 */
public class CdpFetchRuleInterceptorUnitTest {

    @Test
    public void constructorShouldRejectDriversWithoutDevTools() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        IllegalStateException thrown = Assert.expectThrows(IllegalStateException.class,
                () -> new CdpFetchRuleInterceptor(driver, next -> next));
        Assert.assertTrue(thrown.getMessage().contains("DevTools"), thrown.getMessage());
    }

    @Test
    public void constructorShouldEnableFetchAndDisableCache() {
        DevTools devTools = Mockito.mock(DevTools.class);
        try (CdpFetchRuleInterceptor ignored = new CdpFetchRuleInterceptor(driver(devTools), next -> next)) {
            ArgumentCaptor<Command> commands = ArgumentCaptor.forClass(Command.class);
            Mockito.verify(devTools, Mockito.atLeast(2)).send(commands.capture());
            Assert.assertEquals(commands.getAllValues().stream().map(Command::getMethod).toList(),
                    List.of("Network.setCacheDisabled", "Fetch.enable"));
            Mockito.verify(devTools).createSessionIfThereIsNotOne("window-1");
            Mockito.verify(devTools).addListener(Mockito.any(), Mockito.any(Consumer.class));
        }
    }

    @Test
    public void closedInterceptorShouldIgnorePausedEvents() throws Exception {
        DevTools devTools = Mockito.mock(DevTools.class);
        Consumer<Map<String, Object>> listener;
        try (CdpFetchRuleInterceptor interceptor = new CdpFetchRuleInterceptor(driver(devTools), next -> next)) {
            listener = capturePausedListener(devTools);
            interceptor.close();
        }
        listener.accept(Map.of("requestId", "closed-1", "request",
                Map.of("method", "GET", "url", "https://example.test/x", "headers", Map.of())));
        // After close, Fetch.disable was sent; no continue/fulfill for the ignored pause.
        Assert.assertFalse(sentMethods(devTools).contains("Fetch.continueRequest"));
        Assert.assertFalse(sentMethods(devTools).contains("Fetch.fulfillRequest"));
    }

    @Test
    public void mockFilterShouldFulfillPausedRequestWithoutRebuildingBody() throws Exception {
        DevTools devTools = Mockito.mock(DevTools.class);
        CountDownLatch fulfilled = new CountDownLatch(1);
        Mockito.doAnswer(invocation -> {
            Command<?> command = invocation.getArgument(0);
            if ("Fetch.fulfillRequest".equals(command.getMethod())) {
                fulfilled.countDown();
            }
            return null;
        }).when(devTools).send(Mockito.any(Command.class), Mockito.any(Duration.class));

        Filter mock = next -> request -> new HttpResponse()
                .setStatus(201)
                .addHeader("Content-Type", "text/plain")
                .setContent(Contents.utf8String("mocked-body"));

        try (CdpFetchRuleInterceptor ignored = new CdpFetchRuleInterceptor(driver(devTools), mock)) {
            capturePausedListener(devTools).accept(Map.of(
                    "requestId", "req-mock",
                    "request", Map.of("method", "POST", "url", "https://example.test/upload",
                            "headers", Map.of("Content-Type", "multipart/form-data"))));
            Assert.assertTrue(fulfilled.await(5, TimeUnit.SECONDS), "Fetch.fulfillRequest was never sent");
        }

        @SuppressWarnings("rawtypes")
        ArgumentCaptor<Command> commands = ArgumentCaptor.forClass(Command.class);
        Mockito.verify(devTools, Mockito.atLeastOnce()).send(commands.capture(), Mockito.any(Duration.class));
        Command<?> fulfill = commands.getAllValues().stream()
                .filter(command -> "Fetch.fulfillRequest".equals(command.getMethod()))
                .findFirst()
                .orElseThrow();
        @SuppressWarnings("unchecked")
        Map<String, Object> params = (Map<String, Object>) fulfill.getParams();
        Assert.assertEquals(params.get("requestId"), "req-mock");
        Assert.assertEquals(params.get("responseCode"), 201);
        Assert.assertEquals(new String(Base64.getDecoder().decode((String) params.get("body"))), "mocked-body");
    }

    @Test
    public void passthroughFilterShouldContinueByIdThenReturnServerResponse() throws Exception {
        DevTools devTools = Mockito.mock(DevTools.class);
        CountDownLatch continued = new CountDownLatch(1);
        Mockito.doAnswer(invocation -> {
            Command<?> command = invocation.getArgument(0);
            if ("Fetch.continueRequest".equals(command.getMethod())) {
                continued.countDown();
            }
            if ("Fetch.getResponseBody".equals(command.getMethod())) {
                return Map.of("body", Base64.getEncoder().encodeToString("server-body".getBytes()),
                        "base64Encoded", true);
            }
            return null;
        }).when(devTools).send(Mockito.any(Command.class), Mockito.any(Duration.class));

        AtomicReference<HttpResponse> observed = new AtomicReference<>();
        CountDownLatch observedReady = new CountDownLatch(1);
        Filter recording = next -> request -> {
            HttpResponse response = next.execute(request);
            observed.set(response);
            observedReady.countDown();
            return response;
        };

        try (CdpFetchRuleInterceptor ignored = new CdpFetchRuleInterceptor(driver(devTools), recording)) {
            Consumer<Map<String, Object>> listener = capturePausedListener(devTools);
            listener.accept(Map.of(
                    "requestId", "req-pass",
                    "request", Map.of("method", "GET", "url", "https://example.test/ok", "headers", Map.of())));
            Assert.assertTrue(continued.await(5, TimeUnit.SECONDS), "Fetch.continueRequest was never sent");

            // Response stage completes the waiting future.
            listener.accept(Map.of(
                    "requestId", "req-pass",
                    "responseStatusCode", 200,
                    "responseHeaders", List.of(
                            Map.of("name", "Content-Type", "value", "text/plain"))));
            Assert.assertTrue(observedReady.await(5, TimeUnit.SECONDS), "passthrough response never arrived");
        }

        Assert.assertEquals(observed.get().getStatus(), 200);
        Assert.assertEquals(observed.get().getHeader("Content-Type"), "text/plain");
        Assert.assertEquals(new String(Contents.bytes(observed.get().getContent())), "server-body");
    }

    @Test
    public void responseStageWithoutWaiterShouldContinueUnchanged() throws Exception {
        DevTools devTools = Mockito.mock(DevTools.class);
        CountDownLatch continued = new CountDownLatch(1);
        Mockito.doAnswer(invocation -> {
            Command<?> command = invocation.getArgument(0);
            if ("Fetch.continueRequest".equals(command.getMethod())) {
                continued.countDown();
            }
            return null;
        }).when(devTools).send(Mockito.any(Command.class), Mockito.any(Duration.class));

        try (CdpFetchRuleInterceptor ignored = new CdpFetchRuleInterceptor(driver(devTools), next -> next)) {
            capturePausedListener(devTools).accept(Map.of(
                    "requestId", "orphan-response",
                    "responseStatusCode", 204,
                    "responseHeaders", List.of()));
            Assert.assertTrue(continued.await(5, TimeUnit.SECONDS));
        }
    }

    @Test
    public void handlerFailureShouldFailThePausedRequest() throws Exception {
        DevTools devTools = Mockito.mock(DevTools.class);
        CountDownLatch failed = new CountDownLatch(1);
        Mockito.doAnswer(invocation -> {
            Command<?> command = invocation.getArgument(0);
            if ("Fetch.failRequest".equals(command.getMethod())) {
                failed.countDown();
            }
            return null;
        }).when(devTools).send(Mockito.any(Command.class), Mockito.any(Duration.class));

        Filter exploding = next -> request -> {
            throw new IllegalStateException("rule blew up");
        };

        try (CdpFetchRuleInterceptor ignored = new CdpFetchRuleInterceptor(driver(devTools), exploding)) {
            capturePausedListener(devTools).accept(Map.of(
                    "requestId", "req-fail",
                    "request", Map.of("method", "GET", "url", "https://example.test/boom", "headers", Map.of())));
            Assert.assertTrue(failed.await(5, TimeUnit.SECONDS), "Fetch.failRequest was never sent");
        }
    }

    @Test
    public void responseErrorReasonShouldSurfaceToTheFilter() throws Exception {
        DevTools devTools = Mockito.mock(DevTools.class);
        CountDownLatch continued = new CountDownLatch(1);
        Mockito.doAnswer(invocation -> {
            Command<?> command = invocation.getArgument(0);
            if ("Fetch.continueRequest".equals(command.getMethod())) {
                continued.countDown();
            }
            return null;
        }).when(devTools).send(Mockito.any(Command.class), Mockito.any(Duration.class));

        AtomicReference<Throwable> failure = new AtomicReference<>();
        CountDownLatch failed = new CountDownLatch(1);
        Filter recording = next -> request -> {
            try {
                return next.execute(request);
            } catch (RuntimeException e) {
                failure.set(e);
                failed.countDown();
                throw e;
            }
        };

        try (CdpFetchRuleInterceptor ignored = new CdpFetchRuleInterceptor(driver(devTools), recording)) {
            Consumer<Map<String, Object>> listener = capturePausedListener(devTools);
            listener.accept(Map.of(
                    "requestId", "req-err",
                    "request", Map.of("method", "GET", "url", "https://example.test/down", "headers", Map.of())));
            Assert.assertTrue(continued.await(5, TimeUnit.SECONDS));
            listener.accept(Map.of(
                    "requestId", "req-err",
                    "responseErrorReason", "Failed",
                    "responseHeaders", List.of()));
            Assert.assertTrue(failed.await(5, TimeUnit.SECONDS));
        }

        Assert.assertNotNull(failure.get());
        Assert.assertEquals(failure.get().getMessage(), "Failed");
    }

    @Test
    public void closeShouldDisableFetchAndFailOutstandingWaits() throws Exception {
        DevTools devTools = Mockito.mock(DevTools.class);
        List<String> timedMethods = new CopyOnWriteArrayList<>();
        Mockito.doAnswer(invocation -> {
            timedMethods.add(((Command<?>) invocation.getArgument(0)).getMethod());
            return null;
        }).when(devTools).send(Mockito.any(Command.class), Mockito.any(Duration.class));

        CdpFetchRuleInterceptor interceptor = new CdpFetchRuleInterceptor(driver(devTools), next -> next);
        interceptor.close();

        Assert.assertTrue(timedMethods.contains("Fetch.disable"), timedMethods.toString());
        // Second close is safe / idempotent enough for AutoCloseable try-with-resources patterns.
        interceptor.close();
    }

    @Test
    public void fulfillShouldDefaultInvalidStatusTo200() throws Exception {
        DevTools devTools = Mockito.mock(DevTools.class);
        CountDownLatch fulfilled = new CountDownLatch(1);
        AtomicReference<Map<String, Object>> fulfillParams = new AtomicReference<>();
        Mockito.doAnswer(invocation -> {
            Command<?> command = invocation.getArgument(0);
            if ("Fetch.fulfillRequest".equals(command.getMethod())) {
                @SuppressWarnings("unchecked")
                Map<String, Object> params = (Map<String, Object>) command.getParams();
                fulfillParams.set(params);
                fulfilled.countDown();
            }
            return null;
        }).when(devTools).send(Mockito.any(Command.class), Mockito.any(Duration.class));

        Filter mock = next -> request -> new HttpResponse().setStatus(0).setContent(Contents.utf8String(""));
        try (CdpFetchRuleInterceptor ignored = new CdpFetchRuleInterceptor(driver(devTools), mock)) {
            capturePausedListener(devTools).accept(Map.of(
                    "requestId", "req-status",
                    "request", Map.of("method", "GET", "url", "https://example.test/z", "headers", Map.of())));
            Assert.assertTrue(fulfilled.await(5, TimeUnit.SECONDS));
        }
        Assert.assertEquals(fulfillParams.get().get("responseCode"), 200);
    }

    private static WebDriver driver(DevTools devTools) {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasDevTools.class));
        Mockito.when(((HasDevTools) driver).getDevTools()).thenReturn(devTools);
        Mockito.when(driver.getWindowHandle()).thenReturn("window-1");
        return driver;
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    private static Consumer<Map<String, Object>> capturePausedListener(DevTools devTools) {
        ArgumentCaptor<Consumer> listeners = ArgumentCaptor.forClass(Consumer.class);
        Mockito.verify(devTools).addListener(Mockito.any(), listeners.capture());
        return listeners.getValue();
    }

    @SuppressWarnings("rawtypes")
    private static List<String> sentMethods(DevTools devTools) {
        ArgumentCaptor<Command> oneArg = ArgumentCaptor.forClass(Command.class);
        ArgumentCaptor<Command> twoArg = ArgumentCaptor.forClass(Command.class);
        try {
            Mockito.verify(devTools, Mockito.atLeast(0)).send(oneArg.capture());
        } catch (AssertionError ignored) {
            // no one-arg sends
        }
        try {
            Mockito.verify(devTools, Mockito.atLeast(0)).send(twoArg.capture(), Mockito.any(Duration.class));
        } catch (AssertionError ignored) {
            // no two-arg sends
        }
        List<String> methods = new java.util.ArrayList<>();
        oneArg.getAllValues().forEach(command -> methods.add(command.getMethod()));
        twoArg.getAllValues().forEach(command -> methods.add(command.getMethod()));
        return methods;
    }
}
