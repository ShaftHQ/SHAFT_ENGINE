package com.shaft.gui.mobile;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobilePerformanceActionsContract;
import com.shaft.gui.driver.MobilePerformanceSample;
import com.shaft.gui.mobile.internal.MobilePerformanceState;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.android.HasSupportedPerformanceDataType;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.mac.Mac2Driver;
import io.appium.java_client.windows.WindowsDriver;
import org.mockito.Mockito;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.ImmutableCapabilities;
import org.openqa.selenium.remote.HttpCommandExecutor;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

public class MobilePerformanceActionsTest {
    @Test
    public void supportedTypesAndSamplesShouldDelegateToTheExactAppiumInterface() {
        AndroidDriver driver = liveAndroid("performance-android");
        List<String> providerTypes = new ArrayList<>(List.of("cpuinfo", "memoryinfo"));
        Mockito.when(driver.getSupportedPerformanceDataTypes()).thenReturn(providerTypes);
        Mockito.when(driver.getPerformanceData("com.example.app", "cpuinfo", 5)).thenReturn(table(
                row("user", "system"), row(12, 4)));
        MobileActions mobile = new SHAFT.GUI.WebDriver(driver).mobile();

        MobilePerformanceActionsContract performance = mobile.performance();
        List<String> types = performance.supportedTypes();
        providerTypes.set(0, "changed");
        MobilePerformanceSample sample = performance.sample("com.example.app", "cpuinfo");

        Assert.assertEquals(types, List.of("cpuinfo", "memoryinfo"));
        Assert.expectThrows(UnsupportedOperationException.class, () -> types.add("batteryinfo"));
        Assert.assertEquals(sample.applicationId(), "com.example.app");
        Assert.assertEquals(sample.dataType(), "cpuinfo");
        Assert.assertEquals(sample.columns(), List.of("user", "system"));
        Assert.assertEquals(sample.rows(), List.of(row(12, 4)));
        Assert.assertEquals(performance.history(), List.of(sample));
        Assert.assertSame(performance.and(), mobile);
        Mockito.verify(driver).getPerformanceData("com.example.app", "cpuinfo", 5);
    }

    @Test
    public void customExactInterfaceShouldBeSupportedWithoutPlatformNameInference() {
        AppiumDriver driver = liveCustom("performance-custom");
        HasSupportedPerformanceDataType provider = (HasSupportedPerformanceDataType) driver;
        Mockito.when(provider.getSupportedPerformanceDataTypes()).thenReturn(List.of("batteryinfo"));
        Mockito.when(provider.getPerformanceData("custom.app", "batteryinfo", 5)).thenReturn(table(
                row("level"), row(92)));

        MobilePerformanceSample sample = new SHAFT.GUI.WebDriver(driver).mobile().performance()
                .sample("custom.app", "batteryinfo");

        Assert.assertEquals(sample.rows(), List.of(row(92)));
    }

    @Test
    public void unsupportedDriverFamiliesShouldFailClosed() {
        for (AppiumDriver driver : List.of(
                live(AppiumDriver.class, "generic-performance"),
                live(IOSDriver.class, "ios-performance"),
                live(WindowsDriver.class, "windows-performance"),
                live(Mac2Driver.class, "mac-performance"))) {
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> new SHAFT.GUI.WebDriver(driver).mobile().performance());
        }
    }

    @Test
    public void staleNamespaceShouldRecheckLivenessBeforeEveryProviderCall() {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(
                new SessionId("closing-performance"), new SessionId("closing-performance"), null);
        MobilePerformanceActionsContract performance = new SHAFT.GUI.WebDriver(driver).mobile().performance();

        Assert.expectThrows(UnsupportedOperationException.class, performance::supportedTypes);
        Mockito.verify(driver, Mockito.never()).getSupportedPerformanceDataTypes();
    }

    @Test
    public void invalidRequestsAndMalformedProviderTablesShouldNotEnterHistory() {
        AndroidDriver driver = liveAndroid("malformed-performance");
        Mockito.when(driver.getPerformanceData("app", "empty", 5)).thenReturn(List.of());
        Mockito.when(driver.getPerformanceData("app", "duplicate", 5)).thenReturn(table(row("value", "value")));
        Mockito.when(driver.getPerformanceData("app", "width", 5)).thenReturn(table(row("one"), row(1, 2)));
        Mockito.when(driver.getPerformanceData("app", "non-string-header", 5)).thenReturn(table(row(1)));
        Mockito.when(driver.getPerformanceData("app", "null-header", 5)).thenReturn(
                java.util.Collections.singletonList(null));
        Mockito.when(driver.getPerformanceData("app", "explosive-value", 5)).thenReturn(
                table(row("value"), row(new ExplosiveValue())));
        MobilePerformanceActionsContract performance = new SHAFT.GUI.WebDriver(driver).mobile().performance();

        Assert.expectThrows(IllegalArgumentException.class, () -> performance.sample(" ", "cpuinfo"));
        Assert.expectThrows(IllegalArgumentException.class, () -> performance.sample("app", " "));
        Mockito.verify(driver, Mockito.never()).getPerformanceData(Mockito.anyString(), Mockito.anyString(), Mockito.anyInt());
        Assert.expectThrows(IllegalArgumentException.class, () -> performance.sample("app", "empty"));
        Assert.expectThrows(IllegalArgumentException.class, () -> performance.sample("app", "duplicate"));
        Assert.expectThrows(IllegalArgumentException.class, () -> performance.sample("app", "width"));
        Assert.expectThrows(IllegalArgumentException.class, () -> performance.sample("app", "non-string-header"));
        Assert.expectThrows(NullPointerException.class, () -> performance.sample("app", "null-header"));
        IllegalArgumentException malformedValue = Assert.expectThrows(IllegalArgumentException.class,
                () -> performance.sample("app", "explosive-value"));
        Assert.assertTrue(malformedValue.getMessage().contains("immutable JSON scalars"));
        Assert.assertTrue(performance.history().isEmpty());
    }

    @Test
    public void supportedTypesShouldRejectMalformedProviderValuesAndRetainExceptionIdentity() {
        AndroidDriver nullTypes = liveAndroid("null-types");
        Mockito.when(nullTypes.getSupportedPerformanceDataTypes()).thenReturn(null);
        Assert.expectThrows(NullPointerException.class,
                () -> new SHAFT.GUI.WebDriver(nullTypes).mobile().performance().supportedTypes());

        AndroidDriver blankTypes = liveAndroid("blank-types");
        Mockito.when(blankTypes.getSupportedPerformanceDataTypes()).thenReturn(List.of("cpuinfo", " "));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new SHAFT.GUI.WebDriver(blankTypes).mobile().performance().supportedTypes());

        AndroidDriver duplicateTypes = liveAndroid("duplicate-types");
        Mockito.when(duplicateTypes.getSupportedPerformanceDataTypes()).thenReturn(List.of("cpuinfo", "cpuinfo"));
        Assert.expectThrows(IllegalArgumentException.class,
                () -> new SHAFT.GUI.WebDriver(duplicateTypes).mobile().performance().supportedTypes());

        AndroidDriver failedTypes = liveAndroid("failed-types");
        IllegalStateException providerFailure = new IllegalStateException("types failed");
        Mockito.when(failedTypes.getSupportedPerformanceDataTypes()).thenThrow(providerFailure);
        RuntimeException observed = Assert.expectThrows(RuntimeException.class,
                () -> new SHAFT.GUI.WebDriver(failedTypes).mobile().performance().supportedTypes());
        Assert.assertSame(observed, providerFailure);
    }

    @Test
    public void providerExceptionsShouldRetainIdentityAndLeaveHistoryUntouched() {
        AndroidDriver driver = liveAndroid("failed-performance");
        IllegalStateException providerFailure = new IllegalStateException("provider failed");
        Mockito.when(driver.getPerformanceData("app", "cpuinfo", 5)).thenThrow(providerFailure);
        MobilePerformanceActionsContract performance = new SHAFT.GUI.WebDriver(driver).mobile().performance();

        RuntimeException observed = Assert.expectThrows(RuntimeException.class,
                () -> performance.sample("app", "cpuinfo"));

        Assert.assertSame(observed, providerFailure);
        Assert.assertTrue(performance.history().isEmpty());
    }

    @Test
    public void historyShouldBeBoundedClearableAndIsolatedByDriverIdentity() {
        AndroidDriver first = liveAndroid("same-session-id");
        AndroidDriver second = liveAndroid("same-session-id");
        AtomicInteger sequence = new AtomicInteger();
        Mockito.when(first.getPerformanceData(Mockito.eq("first.app"), Mockito.eq("cpuinfo"), Mockito.eq(5)))
                .thenAnswer(ignored -> table(row("sequence"), row(sequence.getAndIncrement())));
        Mockito.when(second.getPerformanceData(Mockito.eq("second.app"), Mockito.eq("cpuinfo"), Mockito.eq(5)))
                .thenReturn(table(row("sequence"), row(2)));
        MobilePerformanceActionsContract firstPerformance = new SHAFT.GUI.WebDriver(first).mobile().performance();
        MobilePerformanceActionsContract secondPerformance = new SHAFT.GUI.WebDriver(second).mobile().performance();

        for (int index = 0; index < 101; index++) {
            firstPerformance.sample("first.app", "cpuinfo");
        }
        secondPerformance.sample("second.app", "cpuinfo");

        List<MobilePerformanceSample> snapshot = firstPerformance.history();
        Assert.assertEquals(snapshot.size(), 100);
        Assert.assertEquals(snapshot.getFirst().rows(), List.of(row(1)));
        Assert.assertEquals(snapshot.getLast().rows(), List.of(row(100)));
        Assert.assertTrue(snapshot.stream()
                .allMatch(sample -> sample.applicationId().equals("first.app")));
        Assert.assertEquals(secondPerformance.history().size(), 1);
        Assert.assertSame(firstPerformance.clear(), firstPerformance);
        Assert.assertTrue(firstPerformance.history().isEmpty());
        Assert.assertEquals(snapshot.size(), 100);
        Assert.assertEquals(snapshot.getFirst().rows(), List.of(row(1)));
        Assert.expectThrows(UnsupportedOperationException.class, () -> snapshot.add(snapshot.getFirst()));
        Assert.assertEquals(secondPerformance.history().size(), 1);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> firstPerformance.history().add(secondPerformance.history().getFirst()));
    }

    @Test
    public void teardownShouldRemoveOnlyTheClosingDriversHistory() {
        AndroidDriver closing = liveAndroid("closing-history");
        AndroidDriver retained = liveAndroid("retained-history");
        Mockito.when(closing.getPerformanceData("closing.app", "cpuinfo", 5))
                .thenReturn(table(row("value"), row(1)));
        Mockito.when(retained.getPerformanceData("retained.app", "cpuinfo", 5))
                .thenReturn(table(row("value"), row(2)));
        MobilePerformanceActionsContract closingPerformance =
                new SHAFT.GUI.WebDriver(closing).mobile().performance();
        MobilePerformanceActionsContract retainedPerformance =
                new SHAFT.GUI.WebDriver(retained).mobile().performance();
        closingPerformance.sample("closing.app", "cpuinfo");
        retainedPerformance.sample("retained.app", "cpuinfo");

        new DriverFactoryHelper().closeDriver(closing);

        Assert.expectThrows(UnsupportedOperationException.class, closingPerformance::history);
        Assert.assertEquals(retainedPerformance.history().size(), 1);
    }

    @Test
    public void equalButDistinctDriversShouldRetainIdentityIsolatedState() {
        EqualPerformanceDriver first = new EqualPerformanceDriver("equal-first", 1);
        EqualPerformanceDriver second = new EqualPerformanceDriver("equal-second", 2);
        MobilePerformanceActionsContract firstPerformance = new SHAFT.GUI.WebDriver(first).mobile().performance();
        MobilePerformanceActionsContract secondPerformance = new SHAFT.GUI.WebDriver(second).mobile().performance();
        firstPerformance.sample("first.app", "cpuinfo");
        secondPerformance.sample("second.app", "cpuinfo");

        firstPerformance.clear();

        Assert.assertTrue(firstPerformance.history().isEmpty());
        Assert.assertEquals(secondPerformance.history().getFirst().rows(), List.of(row(2)));
        MobilePerformanceState.closeAndRemove(first);
        Assert.expectThrows(UnsupportedOperationException.class, firstPerformance::history);
        Assert.assertEquals(secondPerformance.history().size(), 1);
    }

    @Test
    public void inFlightSampleShouldNotRecreateStateAfterDriverTeardown() throws Exception {
        AndroidDriver closing = liveAndroid("in-flight-history");
        CountDownLatch providerStarted = new CountDownLatch(1);
        CountDownLatch providerRelease = new CountDownLatch(1);
        Mockito.when(closing.getPerformanceData("closing.app", "cpuinfo", 5)).thenAnswer(ignored -> {
            providerStarted.countDown();
            if (!providerRelease.await(10, TimeUnit.SECONDS)) {
                throw new IllegalStateException("provider release timed out");
            }
            return table(row("value"), row(1));
        });
        MobilePerformanceActionsContract performance =
                new SHAFT.GUI.WebDriver(closing).mobile().performance();

        try (var executor = Executors.newSingleThreadExecutor()) {
            Future<MobilePerformanceSample> future = executor.submit(
                    () -> performance.sample("closing.app", "cpuinfo"));
            Assert.assertTrue(providerStarted.await(10, TimeUnit.SECONDS));
            new DriverFactoryHelper().closeDriver(closing);
            providerRelease.countDown();
            ExecutionException failure = Assert.expectThrows(ExecutionException.class,
                    () -> future.get(10, TimeUnit.SECONDS));
            Assert.assertTrue(failure.getCause() instanceof UnsupportedOperationException);
        }
        Assert.expectThrows(UnsupportedOperationException.class, performance::history);
    }

    @Test
    public void concurrentSamplesShouldRetainTheNewestBoundWithoutCorruptingHistory() throws Exception {
        AndroidDriver driver = liveAndroid("parallel-performance");
        Mockito.when(driver.getPerformanceData("parallel.app", "cpuinfo", 5))
                .thenReturn(table(row("value"), row(1)));
        MobilePerformanceActionsContract performance = new SHAFT.GUI.WebDriver(driver).mobile().performance();

        try (var executor = Executors.newFixedThreadPool(8)) {
            List<Future<MobilePerformanceSample>> futures = new ArrayList<>();
            for (int index = 0; index < 160; index++) {
                futures.add(executor.submit(() -> performance.sample("parallel.app", "cpuinfo")));
            }
            for (Future<MobilePerformanceSample> future : futures) {
                Assert.assertNotNull(future.get(10, TimeUnit.SECONDS));
            }
            executor.shutdown();
            Assert.assertTrue(executor.awaitTermination(10, TimeUnit.SECONDS));
        }

        Assert.assertEquals(performance.history().size(), 100);
    }

    @Test
    public void clearAndCountShouldLinearizeWithConcurrentAppends() throws Exception {
        AndroidDriver driver = liveAndroid("atomic-clear-performance");
        MobilePerformanceSample sample = new MobilePerformanceSample(
                java.time.Instant.EPOCH, "atomic.app", "cpuinfo", List.of("value"), List.of(row(1)));

        try (var executor = Executors.newFixedThreadPool(2)) {
            for (int iteration = 0; iteration < 100; iteration++) {
                MobilePerformanceState.clear(driver);
                MobilePerformanceState.append(driver, sample);
                CountDownLatch start = new CountDownLatch(1);
                Future<Integer> cleared = executor.submit(() -> {
                    start.await();
                    return MobilePerformanceState.clearAndCount(driver);
                });
                Future<?> appended = executor.submit(() -> {
                    start.await();
                    MobilePerformanceState.append(driver, sample);
                    return null;
                });
                start.countDown();
                int clearedCount = cleared.get(10, TimeUnit.SECONDS);
                appended.get(10, TimeUnit.SECONDS);
                Assert.assertEquals(clearedCount + MobilePerformanceState.history(driver).size(), 2);
            }
        }
    }

    @Test
    public void everyOperationShouldRejectAClosedSessionWithoutCallingTheProvider() {
        MobilePerformanceActionsContract supportedTypes = stalePerformance("closed-types");
        Assert.expectThrows(UnsupportedOperationException.class, supportedTypes::supportedTypes);

        MobilePerformanceActionsContract sample = stalePerformance("closed-sample");
        Assert.expectThrows(UnsupportedOperationException.class, () -> sample.sample("app", "cpuinfo"));

        MobilePerformanceActionsContract history = stalePerformance("closed-history");
        Assert.expectThrows(UnsupportedOperationException.class, history::history);

        MobilePerformanceActionsContract clear = stalePerformance("closed-clear");
        Assert.expectThrows(UnsupportedOperationException.class, clear::clear);
    }

    private static AndroidDriver liveAndroid(String id) {
        return live(AndroidDriver.class, id);
    }

    private static AppiumDriver liveCustom(String id) {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(HasSupportedPerformanceDataType.class));
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(id));
        return driver;
    }

    private static MobilePerformanceActionsContract stalePerformance(String id) {
        AndroidDriver driver = Mockito.mock(AndroidDriver.class);
        SessionId live = new SessionId(id);
        Mockito.when(driver.getSessionId()).thenReturn(live, live, null);
        return new SHAFT.GUI.WebDriver(driver).mobile().performance();
    }

    private static <T extends AppiumDriver> T live(Class<T> type, String id) {
        T driver = Mockito.mock(type);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(id));
        return driver;
    }

    @SafeVarargs
    private static List<List<Object>> table(List<Object>... rows) {
        return List.of(rows);
    }

    private static List<Object> row(Object... values) {
        return List.of(values);
    }

    private static final class EqualPerformanceDriver extends AppiumDriver
            implements HasSupportedPerformanceDataType {
        private final SessionId sessionId;
        private final int value;

        private EqualPerformanceDriver(String id, int value) {
            super(Mockito.mock(HttpCommandExecutor.class), new ImmutableCapabilities());
            sessionId = new SessionId(id);
            this.value = value;
        }

        @Override
        protected void startSession(Capabilities capabilities) {
            // No remote session is needed for this identity-state regression fixture.
        }

        @Override
        public SessionId getSessionId() {
            return sessionId;
        }

        @Override
        public List<String> getSupportedPerformanceDataTypes() {
            return List.of("cpuinfo");
        }

        @Override
        public List<List<Object>> getPerformanceData(String packageName, String dataType, int dataReadTimeout) {
            return table(row("value"), row(value));
        }

        @Override
        public boolean equals(Object other) {
            return other instanceof EqualPerformanceDriver;
        }

        @Override
        public int hashCode() {
            return 1;
        }
    }

    private static final class ExplosiveValue {
        @Override
        public String toString() {
            throw new IllegalStateException("provider value toString must not run");
        }
    }
}
