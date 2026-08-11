package com.shaft.gui.browser.internal;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.bidi.BiDi;
import org.openqa.selenium.bidi.HasBiDi;
import org.openqa.selenium.bidi.module.LogInspector;
import org.openqa.selenium.logging.LogEntries;
import org.openqa.selenium.logging.Logs;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.Optional;

public class BidiConsoleLogSourceTest {
    @Test
    public void websocketThreadEventsShouldBeVisibleAndClearableFromTheOwningTestThread() throws Exception {
        WebDriver driver = Mockito.mock(WebDriver.class);
        BidiConsoleLogSource source = new BidiConsoleLogSource();
        BidiConsoleLogSource.install(driver, source);
        try (var executor = Executors.newSingleThreadExecutor()) {
            executor.submit(() -> source.record("error", "async boom", 42)).get(5, TimeUnit.SECONDS);
        }

        Assert.assertEquals(BidiConsoleLogSource.snapshot(driver).size(), 1);
        Assert.assertEquals(BidiConsoleLogSource.snapshot(driver).getFirst().message(), "async boom");
        BidiConsoleLogSource.clear(driver);
        Assert.assertTrue(BidiConsoleLogSource.snapshot(driver).isEmpty());
        BidiConsoleLogSource.closeAndRemove(driver);
    }

    @Test
    @SuppressWarnings("removal")
    public void attachedNegotiatedDriversShouldStartObservationDuringHelperInitialization() {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings().extraInterfaces(HasBiDi.class));
        BiDi bidi = Mockito.mock(BiDi.class);
        Mockito.when(((HasBiDi) driver).maybeGetBiDi()).thenReturn(Optional.of(bidi));
        try (var inspectors = Mockito.mockConstruction(LogInspector.class)) {
            new DriverFactoryHelper(driver);

            Assert.assertTrue(BidiConsoleLogSource.isHealthy(driver));
            Assert.assertEquals(inspectors.constructed().size(), 1);
            BidiConsoleLogSource.closeAndRemove(driver);
        }
    }

    @Test
    public void clearingADualProviderSessionShouldPreventBidiMessagesFromReappearingAfterFallback() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        WebDriver.Options options = Mockito.mock(WebDriver.Options.class);
        Logs logs = Mockito.mock(Logs.class);
        Mockito.when(driver.manage()).thenReturn(options);
        Mockito.when(options.logs()).thenReturn(logs);
        Mockito.when(logs.getAvailableLogTypes()).thenReturn(Set.of("browser"));
        Mockito.when(logs.get("browser")).thenReturn(new LogEntries(List.of()));
        BidiConsoleLogSource source = new BidiConsoleLogSource();
        source.record("error", "before-clear", 42);
        BidiConsoleLogSource.install(driver, source);

        var console = new com.shaft.gui.browser.BrowserActions(driver, true).console();
        console.clear();
        Mockito.when(logs.getAvailableLogTypes()).thenReturn(Set.of());

        Assert.assertTrue(console.messages().isEmpty());
        BidiConsoleLogSource.closeAndRemove(driver);
    }

    @Test
    public void concurrentProductionAndDrainShouldNotLoseMessages() throws Exception {
        WebDriver driver = Mockito.mock(WebDriver.class);
        BidiConsoleLogSource source = new BidiConsoleLogSource();
        BidiConsoleLogSource.install(driver, source);
        int eventCount = 500;
        try (var executor = Executors.newSingleThreadExecutor()) {
            var producer = executor.submit(() -> {
                for (int index = 0; index < eventCount; index++) {
                    source.record("log", "event-" + index, index);
                    Thread.yield();
                }
            });
            while (!producer.isDone()) {
                BidiConsoleLogSource.drainToRecorder(driver);
                Thread.yield();
            }
            producer.get(5, TimeUnit.SECONDS);
        }
        BidiConsoleLogSource.drainToRecorder(driver);

        Assert.assertEquals(com.shaft.tools.io.internal.BrowserObservabilityRecorder.snapshotConsole().size(), eventCount);
        com.shaft.tools.io.internal.BrowserObservabilityRecorder.clearConsole();
        BidiConsoleLogSource.closeAndRemove(driver);
    }
}
