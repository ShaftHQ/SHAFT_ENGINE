package com.shaft.gui.browser.internal;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.remote.http.Filter;
import org.openqa.selenium.remote.http.HttpHandler;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public class BrowserNetworkInterceptorTest {
    @Test
    public void failedObservationStartShouldNotPublishZeroState() {
        WebDriver driver = new EqualWebDriver();
        BrowserNetworkInterceptor interceptor = new BrowserNetworkInterceptor(driver,
                (ignored, filter) -> { throw new IllegalStateException("install failed"); });

        Assert.assertTrue(BrowserNetworkInterceptor.observationCountIfPresent(driver).isEmpty());
        Assert.assertFalse(interceptor.startObserving());
        Assert.assertTrue(BrowserNetworkInterceptor.observationCountIfPresent(driver).isEmpty());
        interceptor.close();
    }

    @Test
    public void retainedCountsShouldBeExactDriverBoundAndRemovedOnClose() {
        WebDriver first = new EqualWebDriver();
        WebDriver equalButDistinct = new EqualWebDriver();
        AtomicReference<Filter> filter = new AtomicReference<>();
        AtomicReference<Filter> siblingFilter = new AtomicReference<>();
        AtomicBoolean firstRouteClosed = new AtomicBoolean();
        BrowserNetworkInterceptor interceptor = new BrowserNetworkInterceptor(first, (driver, installed) -> {
            filter.set(installed);
            return () -> firstRouteClosed.set(true);
        });
        BrowserNetworkInterceptor sibling = new BrowserNetworkInterceptor(equalButDistinct, (driver, installed) -> {
            siblingFilter.set(installed);
            return () -> { };
        });

        Assert.assertTrue(BrowserNetworkInterceptor.observationCountIfPresent(first).isEmpty());
        Assert.assertTrue(BrowserNetworkInterceptor.observationCountIfPresent(equalButDistinct).isEmpty());
        Assert.assertTrue(interceptor.startObserving());
        Assert.assertTrue(sibling.startObserving());
        Assert.assertTrue(com.shaft.gui.capabilities.internal.AutomationCapabilityResolver.forWebDriver(first)
                .supports(com.shaft.gui.capabilities.AutomationFeature.NETWORK_OBSERVATION));
        HttpHandler handler = filter.get().apply(request -> new HttpResponse().setStatus(204));
        handler.execute(new HttpRequest(HttpMethod.GET, "/one"));
        handler.execute(new HttpRequest(HttpMethod.GET, "/two"));

        Assert.assertEquals(BrowserNetworkInterceptor.observationCountIfPresent(first).orElseThrow(), 2);
        Assert.assertEquals(BrowserNetworkInterceptor.observationCountIfPresent(equalButDistinct).orElseThrow(), 0);
        new com.shaft.driver.internal.DriverFactory.DriverFactoryHelper().closeDriver(first);
        Assert.assertTrue(BrowserNetworkInterceptor.observationCountIfPresent(first).isEmpty());
        Assert.assertTrue(firstRouteClosed.get());
        Assert.assertEquals(BrowserNetworkInterceptor.observationCountIfPresent(equalButDistinct).orElseThrow(), 0);
        interceptor.close();
        sibling.close();
    }

    @Test
    public void terminalTeardownShouldRejectLateNetworkOwnerPublication() {
        WebDriver driver = new EqualWebDriver();
        BrowserNetworkInterceptor.closeAndRemove(driver);
        AtomicBoolean installed = new AtomicBoolean();
        BrowserNetworkInterceptor late = new BrowserNetworkInterceptor(driver, (ignored, filter) -> {
            installed.set(true);
            return () -> { };
        });

        Assert.expectThrows(UnsupportedOperationException.class, late::startObserving);
        Assert.assertFalse(installed.get());
        Assert.assertTrue(BrowserNetworkInterceptor.observationCountIfPresent(driver).isEmpty());
        late.close();
    }

}

final class EqualWebDriver implements WebDriver, HasDevTools {
    @Override public boolean equals(Object other) { return other instanceof EqualWebDriver; }
    @Override public int hashCode() { return 7; }
    @Override public Optional<DevTools> maybeGetDevTools() { return Optional.empty(); }
    @Override public void get(String url) { /* No navigation is needed for this identity fixture. */ }
    @Override public String getCurrentUrl() { return ""; }
    @Override public String getTitle() { return ""; }
    @Override public List<WebElement> findElements(By by) { return List.of(); }
    @Override public WebElement findElement(By by) { throw new UnsupportedOperationException(); }
    @Override public String getPageSource() { return ""; }
    @Override public void close() { /* Teardown is modeled by the interceptor registry. */ }
    @Override public void quit() { /* Teardown is modeled by the interceptor registry. */ }
    @Override public Set<String> getWindowHandles() { return Set.of(); }
    @Override public String getWindowHandle() { return ""; }
    @Override public TargetLocator switchTo() { throw new UnsupportedOperationException(); }
    @Override public Navigation navigate() { throw new UnsupportedOperationException(); }
    @Override public Options manage() { throw new UnsupportedOperationException(); }
}
