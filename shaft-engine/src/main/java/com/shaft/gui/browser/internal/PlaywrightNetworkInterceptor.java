package com.shaft.gui.browser.internal;

import com.microsoft.playwright.APIResponse;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Request;
import com.microsoft.playwright.Route;
import com.shaft.tools.io.internal.HttpContractRecorder;
import io.restassured.builder.ResponseBuilder;
import io.restassured.response.Response;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.util.List;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.net.URI;

/**
 * Owns browser network interception rules for one Playwright browser context.
 */
public class PlaywrightNetworkInterceptor {
    private static final String ALL_REQUESTS = "**/*";
    private final BrowserContext browserContext;
    private final List<BrowserNetworkInterceptionRule> rules = new CopyOnWriteArrayList<>();
    private final List<BasicAuthenticationPolicy> authenticationPolicies = new CopyOnWriteArrayList<>();
    private AutoCloseable activeRoute;
    private boolean observing;
    private int observationCount;
    private boolean observationStatePresent;

    /** @return number of requests retained by this browser-context owner */
    public synchronized int observationCount() {
        if (!observationStatePresent) {
            throw new UnsupportedOperationException(
                    "No retained network-observation state exists for the live Playwright session.");
        }
        return observationCount;
    }

    /**
     * Creates a Playwright network interceptor backed by BrowserContext routing.
     *
     * @param browserContext active Playwright browser context
     */
    public PlaywrightNetworkInterceptor(BrowserContext browserContext) {
        this.browserContext = browserContext;
    }

    /**
     * Adds a rule and activates routing for the current browser context.
     *
     * @param rule rule to add
     */
    public synchronized void addRule(BrowserNetworkInterceptionRule rule) {
        rules.add(rule);
        if (activeRoute == null) {
            activateRoute();
        }
    }

    /**
     * Starts passive network observation for contract recording or validation.
     */
    public synchronized void startObserving() {
        if (activeRoute == null) {
            activateRoute();
        }
        observing = true;
    }

    /** Stops passive observation while preserving registered interception rules. */
    public synchronized void stopObserving() {
        observing = false;
        if (rules.isEmpty() && authenticationPolicies.isEmpty()) {
            closeActiveRoute();
        }
    }

    /**
     * Clears all registered rules and removes the Playwright route handler.
     */
    public synchronized void clear() {
        rules.clear();
        if (!observing && authenticationPolicies.isEmpty()) {
            closeActiveRoute();
        }
    }

    /** Registers or replaces a global or origin-scoped HTTP Basic authentication policy. */
    public synchronized void registerBasicAuthentication(String origin, String authorizationHeader) {
        java.util.Objects.requireNonNull(origin, "origin");
        authenticationPolicies.removeIf(policy -> java.util.Objects.equals(policy.origin(), origin));
        authenticationPolicies.add(new BasicAuthenticationPolicy(origin, authorizationHeader));
        if (activeRoute == null) {
            activateRoute();
        }
    }

    /** Clears only SHAFT-managed authentication policies. */
    public synchronized void clearAuthentication() {
        authenticationPolicies.clear();
        if (!observing && rules.isEmpty()) {
            closeActiveRoute();
        }
    }

    /** Clears every route concern owned by this interceptor during session teardown. */
    public synchronized void close() {
        rules.clear();
        authenticationPolicies.clear();
        observing = false;
        closeActiveRoute();
    }

    private void activateRoute() {
        activeRoute = browserContext.route(ALL_REQUESTS, this::handle);
        observationStatePresent = true;
    }

    private void handle(Route route) {
        synchronized (this) {
            observationCount++;
        }
        HttpRequest request = toSeleniumRequest(route.request());
        BrowserNetworkInterceptionRule rule = findMatchingRule(request);
        String authorization = authorizationFor(route.request().url());
        Map<String, String> providerHeaders = authorization == null ? null : withAuthorization(route.request(), authorization);
        boolean contractMode = HttpContractRecorder.isBrowserContractModeActive();
        if (rule == null && !contractMode) {
            if (providerHeaders == null) {
                route.fallback();
            } else {
                route.fallback(new Route.FallbackOptions().setHeaders(providerHeaders));
            }
            return;
        }
        if (rule != null && rule.mocksResponse()) {
            HttpResponse mockedResponse = rule.createResponse(request);
            HttpContractRecorder.handleBrowserExchange(request, mockedResponse, "");
            route.fulfill(toFulfillOptions(mockedResponse));
            return;
        }

        try {
            APIResponse response;
            if (authenticationPolicies.isEmpty()) {
                response = route.fetch();
            } else {
                Route.FetchOptions options = new Route.FetchOptions().setMaxRedirects(0);
                if (providerHeaders != null) {
                    options.setHeaders(providerHeaders);
                }
                response = route.fetch(options);
            }
            if (rule != null) {
                rule.validate(toRestAssuredResponse(response));
            }
            HttpContractRecorder.handleBrowserExchange(request, toSeleniumResponse(response), "");
            route.fulfill(new Route.FulfillOptions().setResponse(response));
        } catch (RuntimeException e) {
            HttpContractRecorder.handleBrowserExchange(request, null, e.getClass().getSimpleName());
            throw e;
        }
    }

    private String authorizationFor(String requestUrl) {
        String requestOrigin;
        try {
            URI uri = URI.create(requestUrl);
            requestOrigin = PermissionOrigin.normalize(uri.getScheme() + "://" + uri.getRawAuthority());
        } catch (RuntimeException ignored) {
            return null;
        }
        for (int i = authenticationPolicies.size() - 1; i >= 0; i--) {
            BasicAuthenticationPolicy policy = authenticationPolicies.get(i);
            if (policy.origin().equals(requestOrigin)) {
                return policy.authorizationHeader();
            }
        }
        return null;
    }

    private Map<String, String> withAuthorization(Request request, String authorization) {
        Map<String, String> headers = new LinkedHashMap<>(request.headers());
        headers.keySet().removeIf(name -> name.equalsIgnoreCase("Authorization"));
        headers.put("Authorization", authorization);
        return headers;
    }

    private BrowserNetworkInterceptionRule findMatchingRule(HttpRequest request) {
        for (int i = rules.size() - 1; i >= 0; i--) {
            BrowserNetworkInterceptionRule rule = rules.get(i);
            if (rule.matches(request)) {
                return rule;
            }
        }
        return null;
    }

    private HttpRequest toSeleniumRequest(Request request) {
        HttpRequest converted = new HttpRequest(toHttpMethod(request.method()), request.url());
        request.headers().forEach(converted::addHeader);
        byte[] body = request.postDataBuffer();
        if (body != null) {
            converted.setContent(Contents.bytes(body));
        }
        return converted;
    }

    private HttpMethod toHttpMethod(String method) {
        return HttpMethod.valueOf(method.toUpperCase(Locale.ROOT));
    }

    private Route.FulfillOptions toFulfillOptions(HttpResponse response) {
        Route.FulfillOptions options = new Route.FulfillOptions().setStatus(response.getStatus());
        Map<String, String> headers = new LinkedHashMap<>();
        response.forEachHeader(headers::put);
        if (!headers.isEmpty()) {
            options.setHeaders(headers);
        }
        if (response.getContentType() != null) {
            options.setContentType(response.getContentType());
        }
        byte[] body = Contents.bytes(response.getContent());
        if (body.length > 0) {
            options.setBodyBytes(body);
        }
        return options;
    }

    private Response toRestAssuredResponse(APIResponse response) {
        ResponseBuilder builder = new ResponseBuilder()
                .setStatusCode(response.status())
                .setBody(response.body());
        response.headers().forEach(builder::setHeader);
        String contentType = response.headers().get("content-type");
        if (contentType == null) {
            contentType = response.headers().get("Content-Type");
        }
        if (contentType != null) {
            builder.setContentType(contentType);
        }
        return builder.build();
    }

    private HttpResponse toSeleniumResponse(APIResponse response) {
        HttpResponse converted = new HttpResponse().setStatus(response.status());
        response.headers().forEach(converted::addHeader);
        converted.setContent(Contents.bytes(response.body()));
        return converted;
    }

    private void closeActiveRoute() {
        if (activeRoute != null) {
            try {
                activeRoute.close();
            } catch (Exception ignored) {
                // Closing an already-reset Playwright route is harmless during teardown.
            } finally {
                activeRoute = null;
            }
        }
    }

    private record BasicAuthenticationPolicy(String origin, String authorizationHeader) { }
}
