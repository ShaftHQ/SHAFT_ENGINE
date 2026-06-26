package com.shaft.gui.browser.internal;

import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import com.shaft.tools.io.internal.HttpContractRecorder;
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
public class BrowserNetworkInterceptor implements AutoCloseable {
    private final WebDriver driver;
    private final InterceptorFactory interceptorFactory;
    private final List<BrowserNetworkInterceptionRule> rules = new CopyOnWriteArrayList<>();
    private AutoCloseable activeInterceptor;
    private boolean observing;

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
     * Starts passive network observation without changing existing mock/assert/verify rules.
     *
     * @return {@code true} when observation started
     */
    public synchronized boolean startObserving() {
        if (!(driver instanceof HasDevTools)) {
            BrowserObservabilityRecorder.recordWarning("network",
                    "Network capture is not supported by this driver.");
            return false;
        }
        observing = true;
        try {
            rebuildInterceptor();
            return true;
        } catch (RuntimeException e) {
            observing = false;
            closeActiveInterceptor();
            BrowserObservabilityRecorder.recordWarning("network",
                    "Network capture could not start for this driver.");
            return false;
        }
    }

    /**
     * Clears all registered rules. Passive trace observation remains active when it was started for the session.
     */
    public synchronized void clear() {
        rules.clear();
        if (observing) {
            try {
                rebuildInterceptor();
            } catch (RuntimeException e) {
                observing = false;
                closeActiveInterceptor();
                BrowserObservabilityRecorder.recordWarning("network",
                        "Network capture could not continue after clearing interceptors.");
            }
        } else {
            closeActiveInterceptor();
        }
    }

    /**
     * Clears rules and removes the Selenium network filter during driver teardown.
     */
    @Override
    public synchronized void close() {
        rules.clear();
        observing = false;
        closeActiveInterceptor();
    }

    private void rebuildInterceptor() {
        closeActiveInterceptor();
        activeInterceptor = interceptorFactory.create(driver, createFilter());
    }

    private Filter createFilter() {
        return next -> request -> {
            BrowserObservabilityRecorder.NetworkExchange exchange = BrowserObservabilityRecorder.startNetwork(request);
            BrowserNetworkInterceptionRule rule = findMatchingRule(request);
            try {
                HttpResponse response;
                if (rule == null) {
                    response = next.execute(request);
                } else if (rule.mocksResponse()) {
                    response = rule.createResponse(request);
                } else {
                    response = next.execute(request);
                    rule.validate(toRestAssuredResponse(response));
                }
                BrowserObservabilityRecorder.finishNetwork(exchange, response, "");
                HttpContractRecorder.handleBrowserExchange(request, response, "");
                return response;
            } catch (RuntimeException e) {
                BrowserObservabilityRecorder.finishNetwork(exchange, null, e.getClass().getSimpleName());
                HttpContractRecorder.handleBrowserExchange(request, null, e.getClass().getSimpleName());
                throw e;
            }
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
