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

/**
 * Owns browser network interception rules for one Playwright browser context.
 */
public class PlaywrightNetworkInterceptor {
    private static final String ALL_REQUESTS = "**/*";
    private final BrowserContext browserContext;
    private final List<BrowserNetworkInterceptionRule> rules = new CopyOnWriteArrayList<>();
    private AutoCloseable activeRoute;
    private boolean observing;

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
            activeRoute = browserContext.route(ALL_REQUESTS, this::handle);
        }
    }

    /**
     * Starts passive network observation for contract recording or validation.
     */
    public synchronized void startObserving() {
        observing = true;
        if (activeRoute == null) {
            activeRoute = browserContext.route(ALL_REQUESTS, this::handle);
        }
    }

    /**
     * Clears all registered rules and removes the Playwright route handler.
     */
    public synchronized void clear() {
        rules.clear();
        if (!observing) {
            closeActiveRoute();
        }
    }

    private void handle(Route route) {
        HttpRequest request = toSeleniumRequest(route.request());
        BrowserNetworkInterceptionRule rule = findMatchingRule(request);
        boolean contractMode = HttpContractRecorder.isBrowserContractModeActive();
        if (rule == null && !contractMode) {
            route.resume();
            return;
        }
        if (rule != null && rule.mocksResponse()) {
            HttpResponse mockedResponse = rule.createResponse(request);
            HttpContractRecorder.handleBrowserExchange(request, mockedResponse, "");
            route.fulfill(toFulfillOptions(mockedResponse));
            return;
        }

        try {
            APIResponse response = route.fetch();
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
}
