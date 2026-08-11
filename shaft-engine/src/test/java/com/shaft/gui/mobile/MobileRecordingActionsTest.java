package com.shaft.gui.mobile;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.driver.MobileActionsContract;
import com.shaft.gui.driver.MobileRecordingActionsContract;
import com.shaft.gui.driver.MobileRecordingOptions;
import com.shaft.gui.mobile.internal.MobileRecordingState;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.appium.java_client.mac.Mac2Driver;
import io.appium.java_client.screenrecording.BaseStartScreenRecordingOptions;
import io.appium.java_client.screenrecording.CanRecordScreen;
import io.appium.java_client.windows.WindowsDriver;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.openqa.selenium.remote.SessionId;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.ImmutableCapabilities;
import org.openqa.selenium.remote.HttpCommandExecutor;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.ref.Reference;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.time.Duration;
import java.util.Base64;
import java.util.Comparator;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class MobileRecordingActionsTest {
    @Test
    public void namespaceShouldRequireTheExactLiveProviderInterface() {
        for (AppiumDriver supported : List.of(
                live(Mockito.mock(AndroidDriver.class), "android-recording"),
                live(Mockito.mock(IOSDriver.class), "ios-recording"),
                live(Mockito.mock(WindowsDriver.class), "windows-recording"),
                live(Mockito.mock(Mac2Driver.class), "mac-recording"),
                customRecorder("custom-recording"))) {
            MobileActionsContract mobile = new SHAFT.GUI.WebDriver(supported).mobile();
            Assert.assertSame(mobile.recording().and(), mobile);
        }

        AppiumDriver generic = live(Mockito.mock(AppiumDriver.class), "generic-recording");
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new SHAFT.GUI.WebDriver(generic).mobile().recording());
        AppiumDriver closed = customRecorder("closed-recording");
        Mockito.when(closed.getSessionId()).thenReturn(null);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new SHAFT.GUI.WebDriver(closed).mobile().recording());
    }

    @Test
    public void explicitOptionsShouldReachTheProviderAndStopShouldReturnFreshDecodedBytes() {
        AppiumDriver driver = customRecorder("recording-happy-path");
        CanRecordScreen provider = (CanRecordScreen) driver;
        byte[] expected = "recording-bytes".getBytes(StandardCharsets.UTF_8);
        Mockito.when(provider.stopRecordingScreen()).thenReturn(Base64.getEncoder().encodeToString(expected));
        MobileRecordingActionsContract recording = new SHAFT.GUI.WebDriver(driver).mobile().recording();
        MobileRecordingOptions options = new MobileRecordingOptions(Duration.ofSeconds(12), 1024);

        Assert.assertSame(recording.start(options), recording);
        ArgumentCaptor<BaseStartScreenRecordingOptions> captured =
                ArgumentCaptor.forClass(BaseStartScreenRecordingOptions.class);
        Mockito.verify(provider).startRecordingScreen(captured.capture());
        Assert.assertEquals(captured.getValue().build().get("timeLimit"), 12L);
        byte[] first = recording.stop();
        Assert.assertEquals(first, expected);

        first[0] = 0;
        recording.start(options);
        byte[] second = recording.stop();
        Assert.assertEquals(second, expected);
        Assert.assertNotSame(second, first);
    }

    @Test
    public void repeatedOrOutOfOrderOperationsShouldFailBeforeAnotherProviderCommand() {
        AppiumDriver driver = customRecorder("recording-order");
        CanRecordScreen provider = (CanRecordScreen) driver;
        Mockito.when(provider.stopRecordingScreen()).thenReturn(Base64.getEncoder().encodeToString(new byte[]{1}));
        MobileRecordingActionsContract recording = new SHAFT.GUI.WebDriver(driver).mobile().recording();

        Assert.expectThrows(IllegalStateException.class, recording::stop);
        Mockito.verify(provider, Mockito.never()).stopRecordingScreen();
        recording.start();
        Assert.expectThrows(IllegalStateException.class, recording::start);
        Mockito.verify(provider, Mockito.times(1))
                .startRecordingScreen(Mockito.any(BaseStartScreenRecordingOptions.class));
        recording.stop();
        Assert.expectThrows(IllegalStateException.class, recording::stop);
        Mockito.verify(provider, Mockito.times(1)).stopRecordingScreen();
    }

    @Test
    public void invalidInputsAndStaleFacadesShouldFailBeforeProviderCommands() {
        AppiumDriver invalidDriver = customRecorder("recording-invalid-inputs");
        CanRecordScreen invalidProvider = (CanRecordScreen) invalidDriver;
        MobileRecordingActionsContract invalid = new SHAFT.GUI.WebDriver(invalidDriver).mobile().recording();
        Assert.expectThrows(NullPointerException.class, () -> invalid.start(null));
        Assert.expectThrows(NullPointerException.class, () -> invalid.stopAndSave(null));
        Mockito.verify(invalidProvider, Mockito.never())
                .startRecordingScreen(Mockito.any(BaseStartScreenRecordingOptions.class));
        Mockito.verify(invalidProvider, Mockito.never()).stopRecordingScreen();

        Assert.expectThrows(UnsupportedOperationException.class, staleRecording("closed-start")::start);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> staleRecording("closed-options").start(MobileRecordingOptions.defaults()));
        Assert.expectThrows(UnsupportedOperationException.class, staleRecording("closed-stop")::stop);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> staleRecording("closed-save").stopAndSave(Path.of("closed.mp4")));
    }

    @Test
    public void providerExceptionsShouldKeepTheirIdentityAndLeaveRetryableState() {
        AppiumDriver driver = customRecorder("recording-provider-errors");
        CanRecordScreen provider = (CanRecordScreen) driver;
        RuntimeException startFailure = new RuntimeException("start-sentinel");
        RuntimeException stopFailure = new RuntimeException("stop-sentinel");
        Mockito.when(provider.startRecordingScreen(Mockito.any(BaseStartScreenRecordingOptions.class)))
                .thenThrow(startFailure).thenReturn("");
        Mockito.when(provider.stopRecordingScreen()).thenThrow(stopFailure)
                .thenReturn(Base64.getEncoder().encodeToString(new byte[]{1, 2}));
        MobileRecordingActionsContract recording = new SHAFT.GUI.WebDriver(driver).mobile().recording();

        Assert.assertSame(Assert.expectThrows(RuntimeException.class, recording::start), startFailure);
        recording.start();
        Assert.assertSame(Assert.expectThrows(RuntimeException.class, recording::stop), stopFailure);
        Assert.assertEquals(recording.stop(), new byte[]{1, 2});
    }

    @Test
    public void malformedBlankAndOversizedPayloadsShouldFailDeterministicallyAndReturnToIdle() {
        assertRejectedPayload("blank-recording", " ", 10,
                "The screen recording provider returned an empty payload.");
        assertRejectedPayload("malformed-recording", "not-base64!", 10,
                "The screen recording provider returned malformed Base64.");
        assertRejectedPayload("oversized-recording", Base64.getEncoder().encodeToString(new byte[]{1, 2, 3}), 2,
                "The screen recording exceeds the configured decoded-byte limit.");
    }

    @Test
    public void stopAndSaveShouldPublishTheExactTargetAndPreserveItWhenDecodeFails() throws Exception {
        Path directory = Files.createTempDirectory("shaft-mobile-recording-save");
        Path target = directory.resolve("nested").resolve("recording.mp4");
        AppiumDriver successDriver = customRecorder("recording-save-success");
        CanRecordScreen successProvider = (CanRecordScreen) successDriver;
        byte[] bytes = new byte[]{3, 2, 1};
        Mockito.when(successProvider.stopRecordingScreen()).thenReturn(Base64.getEncoder().encodeToString(bytes));
        MobileRecordingActionsContract success = new SHAFT.GUI.WebDriver(successDriver).mobile().recording();

        success.start();
        Assert.assertEquals(success.stopAndSave(target), target.toAbsolutePath().normalize());
        Assert.assertEquals(Files.readAllBytes(target), bytes);

        Files.writeString(target, "existing");
        AppiumDriver failedDriver = customRecorder("recording-save-failure");
        CanRecordScreen failedProvider = (CanRecordScreen) failedDriver;
        Mockito.when(failedProvider.stopRecordingScreen()).thenReturn("malformed!");
        MobileRecordingActionsContract failed = new SHAFT.GUI.WebDriver(failedDriver).mobile().recording();
        failed.start();
        Assert.expectThrows(IllegalArgumentException.class, () -> failed.stopAndSave(target));
        Assert.assertEquals(Files.readString(target), "existing");
    }

    @Test
    public void stopAndSaveShouldReplaceASymlinkEntryWithoutChangingItsReferent() throws Exception {
        Path directory = Files.createTempDirectory("shaft-mobile-recording-symlink");
        Path outside = directory.resolve("outside.mp4");
        Path target = directory.resolve("recording.mp4");
        Files.writeString(outside, "outside-recording");
        try {
            try {
                Files.createSymbolicLink(target, outside);
            } catch (IOException | UnsupportedOperationException unavailable) {
                throw new org.testng.SkipException("Symbolic links are unavailable on this host", unavailable);
            }
            AppiumDriver driver = customRecorder("recording-save-symlink");
            byte[] bytes = new byte[]{7, 8, 9};
            Mockito.when(((CanRecordScreen) driver).stopRecordingScreen())
                    .thenReturn(Base64.getEncoder().encodeToString(bytes));
            MobileRecordingActionsContract recording =
                    new SHAFT.GUI.WebDriver(driver).mobile().recording();

            recording.start();
            Assert.assertEquals(recording.stopAndSave(target), target.toAbsolutePath().normalize());
            Assert.assertFalse(Files.isSymbolicLink(target));
            Assert.assertEquals(Files.readAllBytes(target), bytes);
            Assert.assertEquals(Files.readString(outside), "outside-recording");
            try (var paths = Files.list(directory)) {
                Assert.assertFalse(paths.anyMatch(path -> path.getFileName().toString().contains(".tmp-")
                        || path.getFileName().toString().contains(".backup-")));
            }
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void evidenceSnapshotShouldRetainOnlyTheLatestSuccessfulSavedRecording() throws Exception {
        Method snapshotMethod = java.util.Arrays.stream(MobileRecordingState.class.getMethods())
                .filter(method -> method.getName().equals("snapshotIfPresent")
                        && java.util.Arrays.equals(method.getParameterTypes(), new Class<?>[]{AppiumDriver.class}))
                .findFirst().orElse(null);
        Assert.assertNotNull(snapshotMethod, "Evidence needs a non-creating recording-state snapshot.");

        AppiumDriver absent = customRecorder("evidence-recording-absent");
        Assert.assertFalse(hasRecordingState(absent));
        Assert.assertEquals(snapshotMethod.invoke(null, absent), Optional.empty());
        Assert.assertFalse(hasRecordingState(absent), "Evidence reads must not create recording state.");
        Mockito.verifyNoInteractions((CanRecordScreen) absent);
        InvocationTargetException nullFailure = Assert.expectThrows(InvocationTargetException.class,
                () -> snapshotMethod.invoke(null, new Object[]{null}));
        Assert.assertTrue(nullFailure.getCause() instanceof NullPointerException);

        AppiumDriver active = customRecorder("evidence-recording-active");
        CanRecordScreen activeProvider = (CanRecordScreen) active;
        Mockito.when(activeProvider.stopRecordingScreen())
                .thenReturn(Base64.getEncoder().encodeToString(new byte[]{1}));
        MobileRecordingActionsContract activeRecording = new SHAFT.GUI.WebDriver(active).mobile().recording();
        activeRecording.start();
        Object activeSnapshot = ((Optional<?>) snapshotMethod.invoke(null, active)).orElseThrow();
        Assert.assertTrue((Boolean) activeSnapshot.getClass().getMethod("recordingInProgress").invoke(activeSnapshot));
        Assert.assertEquals(activeSnapshot.getClass().getMethod("savedRecording").invoke(activeSnapshot), Optional.empty());
        Mockito.verify(activeProvider, Mockito.never()).stopRecordingScreen();
        activeRecording.stop();
        Object stoppedSnapshot = ((Optional<?>) snapshotMethod.invoke(null, active)).orElseThrow();
        Assert.assertFalse((Boolean) stoppedSnapshot.getClass().getMethod("recordingInProgress").invoke(stoppedSnapshot));
        Assert.assertEquals(stoppedSnapshot.getClass().getMethod("savedRecording").invoke(stoppedSnapshot), Optional.empty());

        Path directory = Files.createTempDirectory("shaft-mobile-recording-evidence");
        try {
            AppiumDriver savedDriver = customRecorder("evidence-recording-saved");
            CanRecordScreen savedProvider = (CanRecordScreen) savedDriver;
            byte[] firstBytes = new byte[]{3, 2, 1};
            byte[] secondBytes = new byte[]{4, 5, 6, 7};
            Mockito.when(savedProvider.stopRecordingScreen()).thenReturn(
                    Base64.getEncoder().encodeToString(firstBytes),
                    Base64.getEncoder().encodeToString(secondBytes),
                    "malformed!",
                    Base64.getEncoder().encodeToString(new byte[]{9}));
            MobileRecordingActionsContract savedRecording =
                    new SHAFT.GUI.WebDriver(savedDriver).mobile().recording();
            Path firstTarget = directory.resolve("first.mp4");
            Path secondTarget = directory.resolve("second.mp4");

            savedRecording.start();
            savedRecording.stopAndSave(firstTarget);
            Object firstSnapshot = ((Optional<?>) snapshotMethod.invoke(null, savedDriver)).orElseThrow();
            Object firstReference = ((Optional<?>) firstSnapshot.getClass()
                    .getMethod("savedRecording").invoke(firstSnapshot)).orElseThrow();
            String firstDigest = sha256(firstBytes);
            Assert.assertEquals(firstReference.getClass().getMethod("path").invoke(firstReference),
                    firstTarget.toAbsolutePath().normalize());
            Assert.assertEquals(firstReference.getClass().getMethod("sizeBytes").invoke(firstReference),
                    (long) firstBytes.length);
            Assert.assertEquals(firstReference.getClass().getMethod("sha256").invoke(firstReference), firstDigest);
            Assert.assertEquals(firstReference.toString(), "SavedRecording[sizeBytes=3]");

            Object savedState = recordingState(savedDriver);
            AtomicReference<Thread> snapshotThread = new AtomicReference<>();
            try (var executor = Executors.newSingleThreadExecutor()) {
                Future<?> blockedSnapshot;
                synchronized (savedState) {
                    blockedSnapshot = executor.submit(() -> {
                        snapshotThread.set(Thread.currentThread());
                        return snapshotMethod.invoke(null, savedDriver);
                    });
                    long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
                    while ((snapshotThread.get() == null || snapshotThread.get().getState() != Thread.State.BLOCKED)
                            && System.nanoTime() < deadline) {
                        Thread.onSpinWait();
                    }
                    Assert.assertNotNull(snapshotThread.get());
                    Assert.assertEquals(snapshotThread.get().getState(), Thread.State.BLOCKED,
                            "Recording evidence snapshots must share the lifecycle state monitor.");
                }
                Assert.assertTrue(((Optional<?>) blockedSnapshot.get(10, TimeUnit.SECONDS)).isPresent());
            }

            savedRecording.start();
            savedRecording.stopAndSave(secondTarget);
            Object latestSnapshot = ((Optional<?>) snapshotMethod.invoke(null, savedDriver)).orElseThrow();
            Object latestReference = ((Optional<?>) latestSnapshot.getClass()
                    .getMethod("savedRecording").invoke(latestSnapshot)).orElseThrow();
            Assert.assertEquals(latestReference.getClass().getMethod("path").invoke(latestReference),
                    secondTarget.toAbsolutePath().normalize());
            Assert.assertEquals(latestReference.getClass().getMethod("sizeBytes").invoke(latestReference),
                    (long) secondBytes.length);
            Assert.assertEquals(latestReference.getClass().getMethod("sha256").invoke(latestReference),
                    sha256(secondBytes));
            Assert.assertEquals(firstReference.getClass().getMethod("path").invoke(firstReference),
                    firstTarget.toAbsolutePath().normalize());
            Assert.assertEquals(firstReference.getClass().getMethod("sha256").invoke(firstReference), firstDigest);

            savedRecording.start();
            Assert.expectThrows(IllegalArgumentException.class,
                    () -> savedRecording.stopAndSave(directory.resolve("decode-failure.mp4")));
            Object failedDecodeSnapshot = ((Optional<?>) snapshotMethod.invoke(null, savedDriver)).orElseThrow();
            Assert.assertEquals(failedDecodeSnapshot.getClass().getMethod("savedRecording")
                    .invoke(failedDecodeSnapshot), Optional.of(latestReference));

            Path fileAsParent = directory.resolve("not-a-directory");
            Files.writeString(fileAsParent, "parent");
            savedRecording.start();
            Assert.expectThrows(UncheckedIOException.class,
                    () -> savedRecording.stopAndSave(fileAsParent.resolve("recording.mp4")));
            Object failedPublicationSnapshot =
                    ((Optional<?>) snapshotMethod.invoke(null, savedDriver)).orElseThrow();
            Assert.assertEquals(failedPublicationSnapshot.getClass().getMethod("savedRecording")
                    .invoke(failedPublicationSnapshot), Optional.of(latestReference));

            EqualRecordingDriver first = new EqualRecordingDriver("evidence-equal-first", new byte[]{7});
            EqualRecordingDriver second = new EqualRecordingDriver("evidence-equal-second", new byte[]{8});
            MobileRecordingActionsContract firstRecording = new SHAFT.GUI.WebDriver(first).mobile().recording();
            MobileRecordingActionsContract secondRecording = new SHAFT.GUI.WebDriver(second).mobile().recording();
            Path equalFirst = directory.resolve("equal-first.mp4");
            Path equalSecond = directory.resolve("equal-second.mp4");
            firstRecording.start();
            firstRecording.stopAndSave(equalFirst);
            secondRecording.start();
            secondRecording.stopAndSave(equalSecond);
            Object firstState = recordingState(first);
            MobileRecordingState.closeAndRemove(first);
            Assert.assertEquals(snapshotMethod.invoke(null, first), Optional.empty());
            Field retainedField = firstState.getClass().getDeclaredField("savedRecording");
            retainedField.setAccessible(true);
            Assert.assertNull(retainedField.get(firstState),
                    "Terminal teardown must release the sensitive path and digest descriptor.");
            Object secondSnapshot = ((Optional<?>) snapshotMethod.invoke(null, second)).orElseThrow();
            Object secondReference = ((Optional<?>) secondSnapshot.getClass()
                    .getMethod("savedRecording").invoke(secondSnapshot)).orElseThrow();
            Assert.assertEquals(secondReference.getClass().getMethod("path").invoke(secondReference),
                    equalSecond.toAbsolutePath().normalize());

            String validDigest = "0".repeat(64);
            Path relative = Path.of("folder", "..", "recording.mp4");
            MobileRecordingState.SavedRecording normalized =
                    new MobileRecordingState.SavedRecording(relative, 0, validDigest);
            Assert.assertEquals(normalized.path(), relative.toAbsolutePath().normalize());
            Assert.assertEquals(normalized.toString(), "SavedRecording[sizeBytes=0]");
            Assert.expectThrows(NullPointerException.class,
                    () -> new MobileRecordingState.SavedRecording(null, 0, validDigest));
            Assert.expectThrows(IllegalArgumentException.class,
                    () -> new MobileRecordingState.SavedRecording(relative, -1, validDigest));
            Assert.expectThrows(NullPointerException.class,
                    () -> new MobileRecordingState.SavedRecording(relative, 0, null));
            Assert.expectThrows(IllegalArgumentException.class,
                    () -> new MobileRecordingState.SavedRecording(relative, 0, "invalid"));
            Assert.expectThrows(IllegalArgumentException.class,
                    () -> new MobileRecordingState.SavedRecording(relative, 0, "A".repeat(64)));
            Assert.expectThrows(NullPointerException.class,
                    () -> new MobileRecordingState.Snapshot(false, null));
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void teardownShouldMakeOnlyTheClosingDriversRecordingStateTerminal() {
        AppiumDriver closing = customRecorder("recording-closing");
        AppiumDriver retained = customRecorder("recording-retained");
        MobileRecordingActionsContract closingRecording =
                new SHAFT.GUI.WebDriver(closing).mobile().recording();
        MobileRecordingActionsContract retainedRecording =
                new SHAFT.GUI.WebDriver(retained).mobile().recording();
        closingRecording.start();

        new DriverFactoryHelper().closeDriver(closing);

        Assert.expectThrows(UnsupportedOperationException.class, closingRecording::start);
        retainedRecording.start();
        Mockito.verify((CanRecordScreen) closing, Mockito.times(1))
                .startRecordingScreen(Mockito.any(BaseStartScreenRecordingOptions.class));
        Mockito.verify((CanRecordScreen) closing, Mockito.never()).stopRecordingScreen();
        Mockito.verify((CanRecordScreen) retained).startRecordingScreen(
                Mockito.any(BaseStartScreenRecordingOptions.class));
    }

    @Test
    public void inFlightStartShouldNotResurrectStateAfterRealDriverTeardown() throws Exception {
        AppiumDriver closing = customRecorder("recording-in-flight-close");
        CanRecordScreen provider = (CanRecordScreen) closing;
        CountDownLatch providerStarted = new CountDownLatch(1);
        CountDownLatch providerRelease = new CountDownLatch(1);
        Mockito.when(provider.startRecordingScreen(Mockito.any(BaseStartScreenRecordingOptions.class)))
                .thenAnswer(ignored -> {
                    providerStarted.countDown();
                    if (!providerRelease.await(10, TimeUnit.SECONDS)) {
                        throw new IllegalStateException("provider release timed out");
                    }
                    return "";
                });
        MobileRecordingActionsContract recording =
                new SHAFT.GUI.WebDriver(closing).mobile().recording();

        try (var executor = Executors.newSingleThreadExecutor()) {
            Future<?> future = executor.submit(() -> {
                recording.start();
                return null;
            });
            Assert.assertTrue(providerStarted.await(10, TimeUnit.SECONDS));
            new DriverFactoryHelper().closeDriver(closing);
            providerRelease.countDown();
            ExecutionException failure = Assert.expectThrows(ExecutionException.class,
                    () -> future.get(10, TimeUnit.SECONDS));
            Assert.assertTrue(failure.getCause() instanceof UnsupportedOperationException);
        }
        Assert.expectThrows(UnsupportedOperationException.class, recording::start);
    }

    @Test
    public void inFlightStopShouldNotPublishMediaAfterRealDriverTeardown() throws Exception {
        AppiumDriver closing = customRecorder("recording-in-flight-stop-close");
        CanRecordScreen provider = (CanRecordScreen) closing;
        CountDownLatch providerStarted = new CountDownLatch(1);
        CountDownLatch providerRelease = new CountDownLatch(1);
        Mockito.when(provider.stopRecordingScreen()).thenAnswer(ignored -> {
            providerStarted.countDown();
            if (!providerRelease.await(10, TimeUnit.SECONDS)) {
                throw new IllegalStateException("provider release timed out");
            }
            return Base64.getEncoder().encodeToString(new byte[]{1});
        });
        MobileRecordingActionsContract recording =
                new SHAFT.GUI.WebDriver(closing).mobile().recording();
        recording.start();

        try (var executor = Executors.newSingleThreadExecutor()) {
            Future<byte[]> future = executor.submit(recording::stop);
            Assert.assertTrue(providerStarted.await(10, TimeUnit.SECONDS));
            new DriverFactoryHelper().closeDriver(closing);
            providerRelease.countDown();
            ExecutionException failure = Assert.expectThrows(ExecutionException.class,
                    () -> future.get(10, TimeUnit.SECONDS));
            Assert.assertTrue(failure.getCause() instanceof UnsupportedOperationException);
        }
        Assert.expectThrows(UnsupportedOperationException.class, recording::start);
    }

    @Test
    public void parallelOperationsShouldExposeOneLinearizableProviderTransition() throws Exception {
        AppiumDriver driver = customRecorder("recording-linearizable");
        CanRecordScreen provider = (CanRecordScreen) driver;
        CountDownLatch startEntered = new CountDownLatch(1);
        CountDownLatch releaseStart = new CountDownLatch(1);
        CountDownLatch stopEntered = new CountDownLatch(1);
        CountDownLatch releaseStop = new CountDownLatch(1);
        Mockito.when(provider.startRecordingScreen(Mockito.any(BaseStartScreenRecordingOptions.class)))
                .thenAnswer(ignored -> {
                    startEntered.countDown();
                    Assert.assertTrue(releaseStart.await(10, TimeUnit.SECONDS));
                    return "";
                });
        Mockito.when(provider.stopRecordingScreen()).thenAnswer(ignored -> {
            stopEntered.countDown();
            Assert.assertTrue(releaseStop.await(10, TimeUnit.SECONDS));
            return Base64.getEncoder().encodeToString(new byte[]{1});
        });
        MobileRecordingActionsContract recording = new SHAFT.GUI.WebDriver(driver).mobile().recording();

        try (var executor = Executors.newFixedThreadPool(2)) {
            Future<?> starting = executor.submit(() -> {
                recording.start();
                return null;
            });
            Assert.assertTrue(startEntered.await(10, TimeUnit.SECONDS));
            Assert.expectThrows(IllegalStateException.class, recording::start);
            Assert.expectThrows(IllegalStateException.class, recording::stop);
            releaseStart.countDown();
            starting.get(10, TimeUnit.SECONDS);

            Future<byte[]> stopping = executor.submit(recording::stop);
            Assert.assertTrue(stopEntered.await(10, TimeUnit.SECONDS));
            Assert.expectThrows(IllegalStateException.class, recording::start);
            Assert.expectThrows(IllegalStateException.class, recording::stop);
            releaseStop.countDown();
            Assert.assertEquals(stopping.get(10, TimeUnit.SECONDS), new byte[]{1});
        }
        Mockito.verify(provider, Mockito.times(1))
                .startRecordingScreen(Mockito.any(BaseStartScreenRecordingOptions.class));
        Mockito.verify(provider, Mockito.times(1)).stopRecordingScreen();
    }

    @Test
    public void equalButDistinctDriversShouldKeepRecordingOwnershipIdentityIsolated() {
        EqualRecordingDriver first = new EqualRecordingDriver("equal-recording-first", new byte[]{1});
        EqualRecordingDriver second = new EqualRecordingDriver("equal-recording-second", new byte[]{2});
        MobileRecordingActionsContract firstRecording = new SHAFT.GUI.WebDriver(first).mobile().recording();
        MobileRecordingActionsContract secondRecording = new SHAFT.GUI.WebDriver(second).mobile().recording();
        firstRecording.start();
        secondRecording.start();

        MobileRecordingState.closeAndRemove(first);

        Assert.expectThrows(UnsupportedOperationException.class, firstRecording::stop);
        Assert.assertEquals(secondRecording.stop(), new byte[]{2});
        Assert.assertEquals(first.stopCalls.get(), 0);
        Assert.assertEquals(second.stopCalls.get(), 1);
    }

    private static AppiumDriver customRecorder(String sessionId) {
        return live(Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(CanRecordScreen.class)), sessionId);
    }

    private static boolean hasRecordingState(AppiumDriver driver) throws Exception {
        return recordingState(driver) != null;
    }

    private static Object recordingState(AppiumDriver driver) throws Exception {
        Field statesField = MobileRecordingState.class.getDeclaredField("STATES");
        statesField.setAccessible(true);
        Map<?, ?> states = (Map<?, ?>) statesField.get(null);
        synchronized (states) {
            for (Map.Entry<?, ?> entry : states.entrySet()) {
                if (entry.getKey() instanceof Reference<?> reference && reference.get() == driver) {
                    return entry.getValue();
                }
            }
            return null;
        }
    }

    private static String sha256(byte[] bytes) throws Exception {
        return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
    }

    private static MobileRecordingActionsContract staleRecording(String sessionId) {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class,
                Mockito.withSettings().extraInterfaces(CanRecordScreen.class));
        SessionId live = new SessionId(sessionId);
        Mockito.when(driver.getSessionId()).thenReturn(live, live, null);
        return new SHAFT.GUI.WebDriver(driver).mobile().recording();
    }

    private static void assertRejectedPayload(String sessionId, String payload, long maxBytes,
                                              String expectedMessage) {
        AppiumDriver driver = customRecorder(sessionId);
        CanRecordScreen provider = (CanRecordScreen) driver;
        Mockito.when(provider.stopRecordingScreen()).thenReturn(payload);
        MobileRecordingActionsContract recording = new SHAFT.GUI.WebDriver(driver).mobile().recording();
        MobileRecordingOptions options = new MobileRecordingOptions(Duration.ofSeconds(1), maxBytes);

        recording.start(options);
        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class, recording::stop);
        Assert.assertEquals(failure.getMessage(), expectedMessage);
        Assert.expectThrows(IllegalStateException.class, recording::stop);
        recording.start(options);
        Mockito.verify(provider, Mockito.times(2))
                .startRecordingScreen(Mockito.any(BaseStartScreenRecordingOptions.class));
    }

    private static <T extends AppiumDriver> T live(T driver, String sessionId) {
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId(sessionId));
        return driver;
    }

    private static void deleteRecursively(Path directory) throws IOException {
        if (!Files.exists(directory)) {
            return;
        }
        try (var paths = Files.walk(directory)) {
            for (Path path : paths.sorted(Comparator.reverseOrder()).toList()) {
                Files.deleteIfExists(path);
            }
        }
    }

    private static final class EqualRecordingDriver extends AppiumDriver implements CanRecordScreen {
        private final SessionId sessionId;
        private final byte[] payload;
        private final AtomicInteger stopCalls = new AtomicInteger();

        private EqualRecordingDriver(String sessionId, byte[] payload) {
            super(Mockito.mock(HttpCommandExecutor.class), new ImmutableCapabilities());
            this.sessionId = new SessionId(sessionId);
            this.payload = payload.clone();
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
        public <T extends BaseStartScreenRecordingOptions> String startRecordingScreen(T options) {
            return "";
        }

        @Override
        public String stopRecordingScreen() {
            stopCalls.incrementAndGet();
            return Base64.getEncoder().encodeToString(payload);
        }

        @Override
        public boolean equals(Object other) {
            return other instanceof EqualRecordingDriver;
        }

        @Override
        public int hashCode() {
            return 1;
        }
    }
}
