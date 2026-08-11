package com.shaft.gui.browser.internal;

import com.microsoft.playwright.APIResponse;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Request;
import com.microsoft.playwright.Route;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class PlaywrightNetworkInterceptorTest {
    @Test
    public void shouldFulfillMatchingMockedResponsesAndClearRoute() throws Exception {
        BrowserContext context = mock(BrowserContext.class);
        AtomicReference<Consumer<Route>> handler = new AtomicReference<>();
        AtomicBoolean routeClosed = new AtomicBoolean(false);
        when(context.route(eq("**/*"), any())).thenAnswer(invocation -> {
            handler.set(invocation.getArgument(1));
            return (AutoCloseable) () -> routeClosed.set(true);
        });
        PlaywrightNetworkInterceptor interceptor = new PlaywrightNetworkInterceptor(context);
        HttpResponse mocked = new HttpResponse()
                .setStatus(201)
                .addHeader("Content-Type", "application/json");
        mocked.setContent(Contents.utf8String("{\"ok\":true}"));

        interceptor.addRule(BrowserNetworkInterceptionRule.mock(
                request -> request.getMethod() == HttpMethod.GET && request.getUri().contains("/api/users"),
                request -> mocked));

        Route route = route("GET", "https://example.test/api/users", null);
        handler.get().accept(route);

        var optionsCaptor = org.mockito.ArgumentCaptor.forClass(Route.FulfillOptions.class);
        verify(route).fulfill(optionsCaptor.capture());
        Assert.assertEquals(optionsCaptor.getValue().status, Integer.valueOf(201));
        Assert.assertEquals(optionsCaptor.getValue().contentType, "application/json");
        Assert.assertEquals(new String(optionsCaptor.getValue().bodyBytes), "{\"ok\":true}");

        interceptor.clear();
        Assert.assertTrue(routeClosed.get());
    }

    @Test
    public void shouldValidateRealResponsesAndFulfillOriginalResponse() {
        BrowserContext context = mock(BrowserContext.class);
        AtomicReference<Consumer<Route>> handler = new AtomicReference<>();
        when(context.route(eq("**/*"), any())).thenAnswer(invocation -> {
            handler.set(invocation.getArgument(1));
            return (AutoCloseable) () -> { };
        });
        PlaywrightNetworkInterceptor interceptor = new PlaywrightNetworkInterceptor(context);
        AtomicBoolean validated = new AtomicBoolean(false);
        interceptor.addRule(BrowserNetworkInterceptionRule.validate(
                request -> request.getUri().contains("/api/status"),
                response -> {
                    validated.set(true);
                    Assert.assertEquals(response.getStatusCode(), 200);
                    Assert.assertTrue(response.asString().contains("\"ok\":true"));
                }));

        Route route = route("GET", "https://example.test/api/status", null);
        APIResponse response = mock(APIResponse.class);
        when(response.status()).thenReturn(200);
        when(response.headers()).thenReturn(Map.of("Content-Type", "application/json"));
        when(response.body()).thenReturn("{\"ok\":true}".getBytes());
        when(route.fetch()).thenReturn(response);

        handler.get().accept(route);

        Assert.assertTrue(validated.get());
        var optionsCaptor = org.mockito.ArgumentCaptor.forClass(Route.FulfillOptions.class);
        verify(route).fulfill(optionsCaptor.capture());
        Assert.assertSame(optionsCaptor.getValue().response, response);
    }

    @Test
    public void shouldApplyAuthenticationOnlyToMatchingOriginsAndRemoveItsRouteOnClear() throws Exception {
        BrowserContext context = mock(BrowserContext.class);
        AtomicReference<Consumer<Route>> handler = new AtomicReference<>();
        AtomicBoolean routeClosed = new AtomicBoolean(false);
        when(context.route(eq("**/*"), any())).thenAnswer(invocation -> {
            handler.set(invocation.getArgument(1));
            return (AutoCloseable) () -> routeClosed.set(true);
        });
        PlaywrightNetworkInterceptor interceptor = new PlaywrightNetworkInterceptor(context);
        interceptor.registerBasicAuthentication("https://example.test", "Basic opaque-token");

        Route matching = route("GET", "https://example.test/secure", null,
                Map.of("authorization", "Basic stale-token", "Accept", "text/html"));
        handler.get().accept(matching);
        var fallback = org.mockito.ArgumentCaptor.forClass(Route.FallbackOptions.class);
        verify(matching).fallback(fallback.capture());
        Assert.assertEquals(fallback.getValue().headers.get("Authorization"), "Basic opaque-token");
        Assert.assertFalse(fallback.getValue().headers.containsKey("authorization"));
        Assert.assertEquals(fallback.getValue().headers.get("Accept"), "text/html");

        Route unrelated = route("GET", "https://other.test/secure", null);
        handler.get().accept(unrelated);
        verify(unrelated).fallback();

        interceptor.startObserving();
        interceptor.stopObserving();
        Assert.assertFalse(routeClosed.get());

        Route afterObservation = route("GET", "https://example.test/after-observation", null);
        handler.get().accept(afterObservation);
        verify(afterObservation).fallback(any(Route.FallbackOptions.class));

        interceptor.clearAuthentication();
        Assert.assertTrue(routeClosed.get());
    }

    @Test
    public void authenticatedFetchShouldStopAtRedirectAndReevaluateTheNextOrigin() {
        BrowserContext context = mock(BrowserContext.class);
        AtomicReference<Consumer<Route>> handler = new AtomicReference<>();
        when(context.route(eq("**/*"), any())).thenAnswer(invocation -> {
            handler.set(invocation.getArgument(1));
            return (AutoCloseable) () -> { };
        });
        PlaywrightNetworkInterceptor interceptor = new PlaywrightNetworkInterceptor(context);
        interceptor.registerBasicAuthentication("https://example.test", "Basic scoped-token");
        interceptor.addRule(BrowserNetworkInterceptionRule.validate(request -> true, response -> { }));

        Route first = route("GET", "https://example.test/redirect", null);
        APIResponse redirect = mock(APIResponse.class);
        when(redirect.status()).thenReturn(302);
        when(redirect.headers()).thenReturn(Map.of("Location", "https://other.test/final"));
        when(redirect.body()).thenReturn(new byte[0]);
        when(first.fetch(any(Route.FetchOptions.class))).thenReturn(redirect);
        handler.get().accept(first);

        var fetch = org.mockito.ArgumentCaptor.forClass(Route.FetchOptions.class);
        verify(first).fetch(fetch.capture());
        Assert.assertEquals(fetch.getValue().maxRedirects, Integer.valueOf(0));
        Assert.assertEquals(fetch.getValue().headers.get("Authorization"), "Basic scoped-token");

        interceptor.clear();
        Route redirected = route("GET", "https://other.test/final", null);
        handler.get().accept(redirected);
        verify(redirected).fallback();
    }

    @Test
    public void unauthenticatedRedirectShouldBeReroutedIntoAProtectedOrigin() {
        BrowserContext context = mock(BrowserContext.class);
        AtomicReference<Consumer<Route>> handler = new AtomicReference<>();
        when(context.route(eq("**/*"), any())).thenAnswer(invocation -> {
            handler.set(invocation.getArgument(1));
            return (AutoCloseable) () -> { };
        });
        PlaywrightNetworkInterceptor interceptor = new PlaywrightNetworkInterceptor(context);
        interceptor.registerBasicAuthentication("https://secure.test", "Basic protected-token");
        interceptor.addRule(BrowserNetworkInterceptionRule.validate(request -> true, response -> { }));

        Route alias = route("GET", "http://alias.test/start", null);
        APIResponse redirect = mock(APIResponse.class);
        when(redirect.status()).thenReturn(302);
        when(redirect.headers()).thenReturn(Map.of("Location", "https://secure.test/private"));
        when(redirect.body()).thenReturn(new byte[0]);
        when(alias.fetch(any(Route.FetchOptions.class))).thenReturn(redirect);
        handler.get().accept(alias);
        var aliasFetch = org.mockito.ArgumentCaptor.forClass(Route.FetchOptions.class);
        verify(alias).fetch(aliasFetch.capture());
        Assert.assertEquals(aliasFetch.getValue().maxRedirects, Integer.valueOf(0));
        Assert.assertNull(aliasFetch.getValue().headers);

        interceptor.clear();
        Route protectedTarget = route("GET", "https://secure.test/private", null);
        handler.get().accept(protectedTarget);
        var protectedFallback = org.mockito.ArgumentCaptor.forClass(Route.FallbackOptions.class);
        verify(protectedTarget).fallback(protectedFallback.capture());
        Assert.assertEquals(protectedFallback.getValue().headers.get("Authorization"), "Basic protected-token");
    }

    private Route route(String method, String url, byte[] body) {
        return route(method, url, body, Map.of());
    }

    private Route route(String method, String url, byte[] body, Map<String, String> headers) {
        Request request = mock(Request.class);
        when(request.method()).thenReturn(method);
        when(request.url()).thenReturn(url);
        when(request.headers()).thenReturn(headers);
        when(request.postDataBuffer()).thenReturn(body);
        Route route = mock(Route.class);
        when(route.request()).thenReturn(request);
        return route;
    }
}
