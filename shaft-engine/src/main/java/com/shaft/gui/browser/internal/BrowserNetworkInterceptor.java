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
import java.util.OptionalInt;
import java.util.Map;
import java.util.HashMap;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Owns browser network interception rules for one WebDriver session.
 */
public class BrowserNetworkInterceptor implements AutoCloseable {
    private static final ReferenceQueue<WebDriver> STALE_DRIVERS = new ReferenceQueue<>();
    private static final Map<IdentityWeakReference, Entry> COUNTERS = new HashMap<>();
    private final WebDriver driver;
    private final InterceptorFactory interceptorFactory;
    private final List<BrowserNetworkInterceptionRule> rules = new CopyOnWriteArrayList<>();
    private AutoCloseable activeInterceptor;
    private boolean observing;
    private boolean closed;
    private final Counter counter = new Counter();

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
        counter.owner(this);
        synchronized (COUNTERS) {
            expungeStaleDrivers();
            Entry existing = COUNTERS.get(new IdentityWeakReference(driver));
            if (existing != null && existing.closed) {
                closed = true;
            } else {
                COUNTERS.put(new IdentityWeakReference(driver, STALE_DRIVERS), new Entry(counter));
            }
        }
    }

    /** Returns the retained count only when this exact driver has an installed interceptor owner. */
    public static OptionalInt observationCountIfPresent(WebDriver driver) {
        synchronized (COUNTERS) {
            expungeStaleDrivers();
            Entry match = COUNTERS.get(new IdentityWeakReference(driver));
            return match == null || match.closed || match.counter == null || !match.counter.active()
                    ? OptionalInt.empty() : OptionalInt.of(match.counter.value());
        }
    }

    /** Removes retained observations for this exact driver during terminal teardown. */
    public static void closeAndRemove(WebDriver driver) {
        if (driver == null) {
            return;
        }
        Counter removed;
        synchronized (COUNTERS) {
            expungeStaleDrivers();
            IdentityWeakReference lookup = new IdentityWeakReference(driver);
            Entry entry = COUNTERS.get(lookup);
            if (entry == null) {
                entry = new Entry(null);
                COUNTERS.put(new IdentityWeakReference(driver, STALE_DRIVERS), entry);
            }
            entry.closed = true;
            removed = entry.counter;
            entry.counter = null;
        }
        if (removed != null) {
            removed.closeOwner();
        }
    }

    /** @return whether this interceptor belongs to the exact driver instance */
    public boolean owns(WebDriver candidate) {
        return driver == candidate;
    }

    /**
     * Adds a rule and activates it for the current browser session.
     *
     * @param rule rule to add
     */
    public synchronized void addRule(BrowserNetworkInterceptionRule rule) {
        requireOpen();
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
        requireOpen();
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

    /** Stops passive observation while preserving registered interception rules. */
    public synchronized void stopObserving() {
        observing = false;
        if (rules.isEmpty()) {
            closeActiveInterceptor();
        } else {
            rebuildInterceptor();
        }
    }

    /**
     * Reports whether this interceptor has one or more mock/validate rules registered.
     *
     * <p>Callers that want to take over sole ownership of the DevTools network filter (for example
     * a dedicated API-capture recorder) must not do so while rules are registered here: replacing
     * this interceptor's filter would silently drop active mocking/validation behavior with no
     * warning to the caller that registered those rules.
     *
     * @return {@code true} when at least one interception rule is registered
     */
    public boolean hasActiveRules() {
        return !rules.isEmpty();
    }

    /**
     * Releases this interceptor's DevTools network filter so another owner (for example a
     * dedicated API-capture recorder that composes the same trace/HAR observation inside its own
     * filter) can become the sole registrant for this driver.
     *
     * <p>Only safe to call when {@link #hasActiveRules()} is {@code false}: this method does not
     * clear registered rules, and calling it while rules exist would leave them registered here but
     * inactive on the driver, silently breaking any mock/validate behavior callers still expect.
     *
     * @return {@code true} when passive observation was active and has been released
     */
    public synchronized boolean releaseForHandoff() {
        if (hasActiveRules()) {
            return false;
        }
        boolean wasActive = observing && activeInterceptor != null;
        observing = false;
        closeActiveInterceptor();
        return wasActive;
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
        closed = true;
        rules.clear();
        observing = false;
        closeActiveInterceptor();
        synchronized (COUNTERS) {
            expungeStaleDrivers();
            Entry entry = COUNTERS.get(new IdentityWeakReference(driver));
            if (entry != null && !entry.closed && entry.counter == counter) {
                COUNTERS.remove(new IdentityWeakReference(driver));
            }
        }
    }

    private void rebuildInterceptor() {
        requireOpen();
        closeActiveInterceptor();
        activeInterceptor = interceptorFactory.create(driver, createFilter());
        counter.activate();
    }

    private void requireOpen() {
        if (closed) {
            throw new UnsupportedOperationException("Browser network observation is closed for this driver session.");
        }
    }

    private Filter createFilter() {
        return next -> request -> {
            counter.increment();
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

    private static void expungeStaleDrivers() {
        IdentityWeakReference stale;
        while ((stale = (IdentityWeakReference) STALE_DRIVERS.poll()) != null) {
            COUNTERS.remove(stale);
        }
    }

    private static final class Counter {
        private int value;
        private boolean active;
        private WeakReference<BrowserNetworkInterceptor> owner;

        private synchronized void owner(BrowserNetworkInterceptor interceptor) {
            owner = new WeakReference<>(interceptor);
        }

        private synchronized void increment() {
            value++;
        }

        private synchronized void activate() {
            active = true;
        }

        private synchronized boolean active() {
            return active;
        }

        private synchronized int value() {
            return value;
        }

        private void closeOwner() {
            BrowserNetworkInterceptor interceptor;
            synchronized (this) {
                interceptor = owner == null ? null : owner.get();
            }
            if (interceptor != null) {
                interceptor.close();
            }
        }
    }

    private static final class Entry {
        private Counter counter;
        private boolean closed;

        private Entry(Counter counter) {
            this.counter = counter;
        }
    }

    private static final class IdentityWeakReference extends WeakReference<WebDriver> {
        private final int identityHash;

        private IdentityWeakReference(WebDriver driver) {
            super(driver);
            identityHash = System.identityHashCode(driver);
        }

        private IdentityWeakReference(WebDriver driver, ReferenceQueue<WebDriver> queue) {
            super(driver, queue);
            identityHash = System.identityHashCode(driver);
        }

        @Override public int hashCode() { return identityHash; }

        @Override public boolean equals(Object other) {
            if (this == other) return true;
            if (!(other instanceof IdentityWeakReference reference)) return false;
            WebDriver referent = get();
            return referent != null && referent == reference.get();
        }
    }
}
