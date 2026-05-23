package com.shaft.gui.browser.internal;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class BiDiNetworkFoundationManagerUnitTest {
    @AfterMethod(alwaysRun = true)
    public void cleanupThreadState() {
        BiDiNetworkFoundationManager.clearCurrentThread();
    }

    @Test
    public void testRemoveInterceptorClosesHandle() {
        RemoteWebDriver driver = mock(RemoteWebDriver.class);
        when(driver.getSessionId()).thenReturn(new SessionId("session-1"));

        AtomicInteger closedCounter = new AtomicInteger(0);
        AutoCloseable handle = closedCounter::incrementAndGet;

        String id = BiDiNetworkFoundationManager.registerInterceptor(driver, handle);
        boolean removed = BiDiNetworkFoundationManager.removeInterceptor(driver, id);

        Assert.assertTrue(removed);
        Assert.assertEquals(closedCounter.get(), 1);
        Assert.assertFalse(BiDiNetworkFoundationManager.removeInterceptor(driver, id));
    }

    @Test
    public void testCleanupForDriverClosesInterceptorsAndListeners() {
        RemoteWebDriver driver = mock(RemoteWebDriver.class);
        when(driver.getSessionId()).thenReturn(new SessionId("session-2"));

        AtomicInteger interceptorClosedCounter = new AtomicInteger(0);
        AtomicInteger listenerClosedCounter = new AtomicInteger(0);

        BiDiNetworkFoundationManager.registerInterceptor(driver, interceptorClosedCounter::incrementAndGet);
        BiDiNetworkFoundationManager.registerListener(driver, listenerClosedCounter::incrementAndGet);

        BiDiNetworkFoundationManager.cleanupForDriver(driver);

        Assert.assertEquals(interceptorClosedCounter.get(), 1);
        Assert.assertEquals(listenerClosedCounter.get(), 1);
    }

    @Test
    public void testCleanupForDriverDoesNotAffectOtherDriversInSameThread() {
        RemoteWebDriver firstDriver = mock(RemoteWebDriver.class);
        when(firstDriver.getSessionId()).thenReturn(new SessionId("session-3"));
        RemoteWebDriver secondDriver = mock(RemoteWebDriver.class);
        when(secondDriver.getSessionId()).thenReturn(new SessionId("session-4"));

        AtomicInteger firstClosedCounter = new AtomicInteger(0);
        AtomicInteger secondClosedCounter = new AtomicInteger(0);

        BiDiNetworkFoundationManager.registerInterceptor(firstDriver, firstClosedCounter::incrementAndGet);
        BiDiNetworkFoundationManager.registerInterceptor(secondDriver, secondClosedCounter::incrementAndGet);

        BiDiNetworkFoundationManager.cleanupForDriver(firstDriver);

        Assert.assertEquals(firstClosedCounter.get(), 1);
        Assert.assertEquals(secondClosedCounter.get(), 0);
    }

    @Test
    public void testClearCurrentThreadClosesEverythingIncludingFailingHandles() {
        RemoteWebDriver driver = mock(RemoteWebDriver.class);
        when(driver.getSessionId()).thenReturn(new SessionId("session-5"));

        AtomicInteger closedCounter = new AtomicInteger(0);
        BiDiNetworkFoundationManager.registerInterceptor(driver, closedCounter::incrementAndGet);
        BiDiNetworkFoundationManager.registerListener(driver, () -> {
            throw new Exception("expected");
        });

        BiDiNetworkFoundationManager.clearCurrentThread();

        Assert.assertEquals(closedCounter.get(), 1);
        Assert.assertFalse(BiDiNetworkFoundationManager.removeListener(driver, "missing"));
    }

    @Test
    public void testCleanupForNullDriverAndIdentityHashFallback() {
        BiDiNetworkFoundationManager.cleanupForDriver(null);

        WebDriver plainDriver = mock(WebDriver.class);
        AtomicInteger closedCounter = new AtomicInteger(0);
        String id = BiDiNetworkFoundationManager.registerListener(plainDriver, closedCounter::incrementAndGet);

        Assert.assertTrue(BiDiNetworkFoundationManager.removeListener(plainDriver, id));
        Assert.assertEquals(closedCounter.get(), 1);
    }
}
