package com.shaft.gui.browser.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.mockito.Mockito;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.bidi.BiDi;
import org.openqa.selenium.bidi.HasBiDi;
import org.openqa.selenium.bidi.module.LogInspector;
import org.openqa.selenium.logging.LogEntries;
import org.openqa.selenium.logging.LogEntry;
import org.openqa.selenium.logging.Logs;
import org.testng.Assert;
import org.testng.annotations.Test;
import org.testng.annotations.AfterMethod;
import com.shaft.properties.internal.Properties;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.Optional;

public class BidiConsoleLogSourceTest {
    @AfterMethod
    public void clearObservationContext() {
        com.shaft.tools.io.internal.ReportContext.clear();
        Properties.clearForCurrentThread();
    }
    @Test
    public void websocketThreadEventsShouldBeVisibleAndClearableFromTheOwningTestThread() throws Exception {
        WebDriver driver = Mockito.mock(WebDriver.class);
        BidiConsoleLogSource source = new BidiConsoleLogSource();
        BidiConsoleLogSource.install(driver, source);
        Assert.assertTrue(com.shaft.gui.capabilities.internal.AutomationCapabilityResolver.forWebDriver(driver)
                .supports(com.shaft.gui.capabilities.AutomationFeature.CONSOLE_LOGS));
        try (var executor = Executors.newSingleThreadExecutor()) {
            executor.submit(() -> source.record("error", "async boom", 42)).get(5, TimeUnit.SECONDS);
        }

        Assert.assertEquals(BidiConsoleLogSource.snapshot(driver).size(), 1);
        Assert.assertEquals(BidiConsoleLogSource.snapshot(driver).getFirst().message(), "async boom");
        BidiConsoleLogSource.clear(driver);
        Assert.assertTrue(BidiConsoleLogSource.snapshot(driver).isEmpty());
        BidiConsoleLogSource.closeAndRemove(driver);
        Assert.assertFalse(com.shaft.gui.capabilities.internal.AutomationCapabilityResolver.forWebDriver(driver)
                .supports(com.shaft.gui.capabilities.AutomationFeature.CONSOLE_LOGS));
    }

    @Test
    public void equalButDistinctDriversShouldRetainIndependentConsoleOwners() {
        WebDriver first = new EqualWebDriver();
        WebDriver second = new EqualWebDriver();
        BidiConsoleLogSource firstSource = new BidiConsoleLogSource();
        BidiConsoleLogSource secondSource = new BidiConsoleLogSource();
        firstSource.record("info", "first", 1);
        secondSource.record("error", "second", 2);
        BidiConsoleLogSource.install(first, firstSource);
        BidiConsoleLogSource.install(second, secondSource);

        Assert.assertEquals(BidiConsoleLogSource.snapshot(first).getFirst().message(), "first");
        Assert.assertEquals(BidiConsoleLogSource.snapshot(second).getFirst().message(), "second");
        BidiConsoleLogSource.closeAndRemove(first);
        Assert.assertFalse(BidiConsoleLogSource.isHealthy(first));
        Assert.assertEquals(BidiConsoleLogSource.snapshot(second).getFirst().message(), "second");
        BidiConsoleLogSource.closeAndRemove(second);
    }

    @Test
    public void terminalCloseShouldPreventLateConsoleOwnerPublication() {
        WebDriver driver = new EqualWebDriver();
        BidiConsoleLogSource.closeAndRemove(driver);
        BidiConsoleLogSource late = new BidiConsoleLogSource();
        BidiConsoleLogSource.install(driver, late);

        Assert.assertFalse(BidiConsoleLogSource.isHealthy(driver));
        Assert.assertFalse(BidiConsoleLogSource.attach(driver));
        Assert.assertTrue(BidiConsoleLogSource.snapshot(driver).isEmpty());
        late.close();
    }

    @Test
    public void legacySnapshotsShouldRejectLateRetentionAndKeepOnlyNewestBoundedEntries() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        List<BrowserObservabilityRecorder.ConsoleSnapshotEntry> entries = new ArrayList<>();
        for (int index = 0; index < 1005; index++) {
            entries.add(BrowserObservabilityRecorder.consoleEntry(
                    "browser", "info", "event-" + index, index));
        }
        LegacyConsoleLogSource.retain(driver, entries);
        var snapshot = LegacyConsoleLogSource.snapshotIfPresent(driver).orElseThrow();
        Assert.assertEquals(snapshot.size(), 1000);
        Assert.assertEquals(snapshot.getFirst().message(), "event-5");
        Assert.assertEquals(snapshot.getLast().message(), "event-1004");
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> snapshot.add(entries.getFirst()));

        LegacyConsoleLogSource.closeAndRemove(driver);
        LegacyConsoleLogSource.retain(driver, entries);
        Assert.assertTrue(LegacyConsoleLogSource.snapshotIfPresent(driver).isEmpty());
    }

    @Test
    public void legacySnapshotsShouldAccumulateSuccessiveProviderBatches() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        WebDriver.Options options = Mockito.mock(WebDriver.Options.class);
        Logs logs = Mockito.mock(Logs.class);
        Mockito.when(driver.manage()).thenReturn(options);
        Mockito.when(options.logs()).thenReturn(logs);
        Mockito.when(logs.getAvailableLogTypes()).thenReturn(Set.of("browser"));
        List<LogEntry> firstBatch = new ArrayList<>();
        List<LogEntry> secondBatch = new ArrayList<>();
        for (int index = 0; index < 900; index++) {
            firstBatch.add(new LogEntry(Level.INFO, index, "event-" + index));
        }
        for (int index = 900; index < 1100; index++) {
            secondBatch.add(new LogEntry(Level.INFO, index, "event-" + index));
        }
        Mockito.when(logs.get("browser"))
                .thenReturn(new LogEntries(firstBatch), new LogEntries(secondBatch));

        Assert.assertTrue(BrowserObservabilityRecorder.tryCollectConsole(driver));
        Assert.assertTrue(BrowserObservabilityRecorder.tryCollectConsole(driver));

        var snapshot = LegacyConsoleLogSource.snapshotIfPresent(driver).orElseThrow();
        Assert.assertEquals(snapshot.size(), 1000);
        Assert.assertEquals(snapshot.getFirst().message(), "event-100");
        Assert.assertEquals(snapshot.getLast().message(), "event-1099");
        LegacyConsoleLogSource.closeAndRemove(driver);
        BrowserObservabilityRecorder.clearConsole();
    }

    @Test
    public void legacySnapshotsShouldIsolateEqualButDistinctDriversThroughRealTeardown() {
        WebDriver first = new EqualWebDriver();
        WebDriver second = new EqualWebDriver();
        var firstEntry = BrowserObservabilityRecorder.consoleEntry(
                "browser", "info", "first-legacy", 1);
        var secondEntry = BrowserObservabilityRecorder.consoleEntry(
                "browser", "error", "second-legacy", 2);
        LegacyConsoleLogSource.retain(first, List.of(firstEntry));
        LegacyConsoleLogSource.retain(second, List.of(secondEntry));

        Assert.assertEquals(LegacyConsoleLogSource.snapshotIfPresent(first).orElseThrow().getFirst().message(),
                "first-legacy");
        Assert.assertEquals(LegacyConsoleLogSource.snapshotIfPresent(second).orElseThrow().getFirst().message(),
                "second-legacy");
        new DriverFactoryHelper().closeDriver(first);
        Assert.assertTrue(LegacyConsoleLogSource.snapshotIfPresent(first).isEmpty());
        Assert.assertEquals(LegacyConsoleLogSource.snapshotIfPresent(second).orElseThrow().getFirst().message(),
                "second-legacy");
        LegacyConsoleLogSource.closeAndRemove(second);
    }

    @Test
    public void legacyCollectionShouldRetainExactDriverSnapshotsAndClearOnTeardown() {
        WebDriver first = Mockito.mock(WebDriver.class);
        WebDriver second = Mockito.mock(WebDriver.class);
        WebDriver.Options firstOptions = Mockito.mock(WebDriver.Options.class);
        WebDriver.Options secondOptions = Mockito.mock(WebDriver.Options.class);
        Logs firstLogs = Mockito.mock(Logs.class);
        Logs secondLogs = Mockito.mock(Logs.class);
        Mockito.when(first.manage()).thenReturn(firstOptions);
        Mockito.when(second.manage()).thenReturn(secondOptions);
        Mockito.when(firstOptions.logs()).thenReturn(firstLogs);
        Mockito.when(secondOptions.logs()).thenReturn(secondLogs);
        Mockito.when(firstLogs.getAvailableLogTypes()).thenReturn(Set.of("browser"));
        Mockito.when(secondLogs.getAvailableLogTypes()).thenReturn(Set.of("browser"));
        Mockito.when(firstLogs.get("browser")).thenReturn(new LogEntries(List.of(
                new LogEntry(Level.INFO, 1, "first-legacy"))));
        Mockito.when(secondLogs.get("browser")).thenReturn(new LogEntries(List.of(
                new LogEntry(Level.SEVERE, 2, "second-legacy"))));

        Assert.assertTrue(BrowserObservabilityRecorder.tryCollectConsole(first));
        Assert.assertTrue(BrowserObservabilityRecorder.tryCollectConsole(second));
        Assert.assertEquals(LegacyConsoleLogSource.snapshotIfPresent(first).orElseThrow().getFirst().message(),
                "first-legacy");
        Assert.assertEquals(LegacyConsoleLogSource.snapshotIfPresent(second).orElseThrow().getFirst().message(),
                "second-legacy");
        new DriverFactoryHelper().closeDriver(first);
        Assert.assertTrue(LegacyConsoleLogSource.snapshotIfPresent(first).isEmpty());
        Assert.assertEquals(LegacyConsoleLogSource.snapshotIfPresent(second).orElseThrow().size(), 1);
        LegacyConsoleLogSource.closeAndRemove(second);
        BrowserObservabilityRecorder.clearConsole();
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
    public void callbackAfterCloseShouldNotRepopulateTheTerminalSource() {
        WebDriver closedDriver = Mockito.mock(WebDriver.class);
        WebDriver inspectionDriver = Mockito.mock(WebDriver.class);
        BidiConsoleLogSource source = new BidiConsoleLogSource();
        BidiConsoleLogSource.install(closedDriver, source);

        BidiConsoleLogSource.closeAndRemove(closedDriver);
        source.record("error", "late-sensitive-event", 42);
        BidiConsoleLogSource.install(inspectionDriver, source);

        Assert.assertTrue(BidiConsoleLogSource.snapshot(inspectionDriver).isEmpty());
        BidiConsoleLogSource.closeAndRemove(inspectionDriver);
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

        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().size(), eventCount);
        BrowserObservabilityRecorder.clearConsole();
        BidiConsoleLogSource.closeAndRemove(driver);
    }

    @Test
    public void asyncDrainShouldFollowReportSessionRolloverInsteadOfExecutorThread() throws Exception {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeConsole(true);
        WebDriver driver = Mockito.mock(WebDriver.class);
        com.shaft.tools.io.internal.ReportContext.start(new com.shaft.listeners.internal.TestExecutionInfo(
                "bidi-setup", getClass().getName(), "setup", "setup", "setup", null, null, false));
        BidiConsoleLogSource source = new BidiConsoleLogSource();
        BidiConsoleLogSource.install(driver, source);
        com.shaft.tools.io.internal.ReportContext.start(new com.shaft.listeners.internal.TestExecutionInfo(
                "bidi-test", getClass().getName(), "test", "test", "test", null, null, false));

        try (var executor = Executors.newSingleThreadExecutor()) {
            executor.submit(() -> {
                SHAFT.Properties.reporting.set().traceIncludeConsole(false);
                source.record("error", "async-owner-message", 42);
                BrowserObservabilityRecorder.collectConsole(driver);
            }).get();
        }

        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().size(), 1);
        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().getFirst().message(),
                "async-owner-message");
        BidiConsoleLogSource.closeAndRemove(driver);
    }

    @Test
    public void disabledLegacyCollectionShouldNotConsumeDriverLogsOrWarn() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeConsole(false);
        com.shaft.tools.io.internal.ReportContext.start(new com.shaft.listeners.internal.TestExecutionInfo(
                "legacy-disabled", getClass().getName(), "disabled", "disabled", "disabled",
                null, null, false));
        WebDriver driver = Mockito.mock(WebDriver.class);

        BrowserObservabilityRecorder.collectConsole(driver);

        Mockito.verify(driver, Mockito.never()).manage();
        Assert.assertTrue(BrowserObservabilityRecorder.drainWarnings().isEmpty());
    }

    @Test
    public void providerOverflowShouldReportOldestConsoleOmission() {
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeConsole(true);
        com.shaft.tools.io.internal.ReportContext.start(new com.shaft.listeners.internal.TestExecutionInfo(
                "bidi-overflow", getClass().getName(), "overflow", "overflow", "overflow",
                null, null, false));
        WebDriver driver = Mockito.mock(WebDriver.class);
        BidiConsoleLogSource source = new BidiConsoleLogSource();
        BidiConsoleLogSource.install(driver, source);
        for (int index = 0; index <= 1000; index++) {
            source.record("log", "bidi-" + index, index);
        }
        Assert.assertEquals(BidiConsoleLogSource.snapshot(driver).size(), 1000);
        Assert.assertEquals(BidiConsoleLogSource.snapshot(driver).getFirst().message(), "bidi-1");
        Assert.assertEquals(BidiConsoleLogSource.snapshot(driver).getLast().message(), "bidi-1000");

        BidiConsoleLogSource.drainToRecorder(driver);

        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().size(), 1000);
        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().getFirst().message(), "bidi-1");
        Assert.assertTrue(BrowserObservabilityRecorder.drainWarnings().stream()
                .anyMatch(warning -> warning.contains("oldest console")));
        BrowserObservabilityRecorder.clearConsole();
        source.record("log", "next-batch", 1001);
        BidiConsoleLogSource.drainToRecorder(driver);
        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().size(), 1);
        Assert.assertEquals(BrowserObservabilityRecorder.snapshotConsole().getFirst().message(), "next-batch");
        Assert.assertTrue(BrowserObservabilityRecorder.drainWarnings().isEmpty());
        BidiConsoleLogSource.closeAndRemove(driver);
    }
}
