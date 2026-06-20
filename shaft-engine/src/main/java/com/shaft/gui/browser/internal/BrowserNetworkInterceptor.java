package com.shaft.gui.browser.internal;

import io.restassured.builder.ResponseBuilder;
import io.restassured.response.Response;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.NetworkInterceptor;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.Filter;
import org.openqa.selenium.remote.http.HttpResponse;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Owns browser network interception rules for one WebDriver session.
 */
public class BrowserNetworkInterceptor {
    private final WebDriver driver;
    private final InterceptorFactory interceptorFactory;
    private final List<BrowserNetworkInterceptionRule> rules = new CopyOnWriteArrayList<>();
    private AutoCloseable activeInterceptor;

    /**
     * Creates a browser network interceptor backed by Selenium DevTools.
     *
     * @param driver the active WebDriver session
     */
    public BrowserNetworkInterceptor(WebDriver driver) {
        this(driver, NetworkInterceptor::new);
    }

    BrowserNetworkInterceptor(WebDriver driver, InterceptorFactory interceptorFactory) {
        this.driver = driver;
        this.interceptorFactory = interceptorFactory;
    }

    /**
     * Adds a rule and activates it for the current browser session.
     *
     * @param rule rule to add
     */
    public synchronized void addRule(BrowserNetworkInterceptionRule rule) {
        if (!(driver instanceof HasDevTools)) {
            throw new IllegalArgumentException("Network Interceptor is not supported by the current driver type.");
        }
        rules.add(rule);
        rebuildInterceptor();
    }

    /**
     * Clears all registered rules and removes the Selenium network filter.
     */
    public synchronized void clear() {
        rules.clear();
        closeActiveInterceptor();
    }

    private void rebuildInterceptor() {
        closeActiveInterceptor();
        activeInterceptor = interceptorFactory.create(driver, createFilter());
    }

    private Filter createFilter() {
        return next -> request -> {
            BrowserNetworkInterceptionRule rule = findMatchingRule(request);
            if (rule == null) {
                return next.execute(request);
            }
            if (rule.mocksResponse()) {
                return rule.createResponse(request);
            }

            HttpResponse response = next.execute(request);
            rule.validate(toRestAssuredResponse(response));
            return response;
        };
    }

    private BrowserNetworkInterceptionRule findMatchingRule(org.openqa.selenium.remote.http.HttpRequest request) {
        for (int i = rules.size() - 1; i >= 0; i--) {
            BrowserNetworkInterceptionRule rule = rules.get(i);
            if (rule.matches(request)) {
                return rule;
            }
        }
        return null;
    }

    private Response toRestAssuredResponse(HttpResponse response) {
        byte[] body = Contents.bytes(response.getContent());
        response.setContent(Contents.bytes(body));
        ResponseBuilder builder = new ResponseBuilder()
                .setStatusCode(response.getStatus())
                .setBody(body);
        if (response.getContentType() != null) {
            builder.setContentType(response.getContentType());
        }
        response.forEachHeader(builder::setHeader);
        return builder.build();
    }

    private void closeActiveInterceptor() {
        if (activeInterceptor != null) {
            try {
                activeInterceptor.close();
            } catch (Exception ignored) {
                // Closing an already-reset Selenium network filter is harmless during teardown.
            } finally {
                activeInterceptor = null;
            }
        }
    }

    @FunctionalInterface
    interface InterceptorFactory {
        AutoCloseable create(WebDriver driver, Filter filter);
    }
}
