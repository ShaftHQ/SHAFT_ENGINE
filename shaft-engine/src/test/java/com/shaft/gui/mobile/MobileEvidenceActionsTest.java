package com.shaft.gui.mobile;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.driver.MobileEvidenceBundle;
import com.shaft.gui.driver.MobileEvidenceActionsContract;
import com.shaft.gui.driver.MobilePerformanceSample;
import com.shaft.gui.mobile.internal.MobileLogSource;
import com.shaft.gui.mobile.internal.MobileEvidenceState;
import com.shaft.gui.mobile.internal.MobilePerformanceState;
import com.shaft.gui.mobile.internal.MobileRecordingState;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.tools.io.trace.TraceArtifactReference;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.ListensToLogcatMessages;
import io.appium.java_client.remote.SupportsContextSwitching;
import io.appium.java_client.ws.StringWebSocketClient;
import org.mockito.Mockito;
import org.mockito.MockedStatic;
import org.openqa.selenium.ImmutableCapabilities;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.FileSystems;
import java.lang.reflect.Method;
import java.time.Instant;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class MobileEvidenceActionsTest {
    private static final byte[] PNG = new byte[]{
            (byte) 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x01};

    @Test
    public void captureShouldPublishOneBoundedArchiveWhoseArtifactReferencesResolve() throws Exception {
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-test-");
        Path target = directory.resolve("bundle.zip");
        try {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(true)
                    .traceIncludeNativePageSource(true);
            AppiumDriver driver = liveDriver();

            MobileEvidenceBundle bundle;
            try {
                bundle = new SHAFT.GUI.WebDriver(driver).mobile().evidence().capture(target);
            } catch (UnsupportedOperationException missingRuntime) {
                Assert.fail("Mobile evidence capture must publish a resolved archive.");
                return;
            }

            Assert.assertEquals(bundle.archive(), target.toAbsolutePath().normalize());
            Assert.assertEquals(bundle.context(), "NATIVE_APP");
            Assert.assertTrue(Files.isRegularFile(target));
            Map<String, TraceArtifactReference> artifacts = bundle.artifacts().stream()
                    .collect(Collectors.toMap(TraceArtifactReference::id, Function.identity()));
            Assert.assertEquals(artifacts.keySet(), java.util.Set.of("screenshot", "source", "recording"));
            Assert.assertFalse(artifacts.get("screenshot").omitted());
            Assert.assertFalse(artifacts.get("source").omitted());
            Assert.assertTrue(artifacts.get("recording").omitted());

            try (ZipFile archive = new ZipFile(target.toFile(), StandardCharsets.UTF_8)) {
                Assert.assertNotNull(archive.getEntry("mobile-evidence.json"));
                JsonObject manifest = JsonParser.parseString(new String(
                        archive.getInputStream(archive.getEntry("mobile-evidence.json")).readAllBytes(),
                        StandardCharsets.UTF_8)).getAsJsonObject();
                List<JsonObject> manifestArtifacts = manifest.getAsJsonArray("artifacts").asList().stream()
                        .map(value -> value.getAsJsonObject()).toList();
                Assert.assertEquals(manifestArtifacts.size(), artifacts.size());
                Assert.assertEquals(manifestArtifacts.stream()
                        .map(value -> value.get("id").getAsString()).collect(Collectors.toSet()), artifacts.keySet());
                for (TraceArtifactReference artifact : artifacts.values()) {
                    ZipEntry entry = archive.getEntry(artifact.path());
                    Assert.assertNotNull(entry, "Every artifact reference must resolve inside the archive.");
                    Assert.assertTrue(entry.getSize() > 0);
                    JsonObject manifestArtifact = manifestArtifacts.stream()
                            .filter(value -> value.get("id").getAsString().equals(artifact.id()))
                            .findFirst().orElseThrow();
                    Assert.assertEquals(manifestArtifact.get("kind").getAsString(), artifact.kind());
                    Assert.assertEquals(manifestArtifact.get("path").getAsString(), artifact.path());
                    Assert.assertEquals(manifestArtifact.get("mimeType").getAsString(), artifact.mimeType());
                    Assert.assertEquals(manifestArtifact.get("omitted").getAsBoolean(), artifact.omitted());
                }
            }
        } finally {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(screenshots)
                    .traceIncludeNativePageSource(nativeSource);
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void captureShouldRedactAndPreserveTypedStateAndVerifiedRecording() throws Exception {
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-state-");
        Path recording = directory.resolve("saved.mp4");
        Path target = directory.resolve("bundle.zip");
        AppiumDriver driver = statefulDriver();
        try {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(false)
                    .traceIncludeNativePageSource(false);
            MobileLogSource.start(driver);
            List<Consumer<String>> messageHandlers = ((ListensToLogcatMessages) driver)
                    .getLogcatClient().getMessageHandlers();
            List<Consumer<Throwable>> errorHandlers = ((ListensToLogcatMessages) driver)
                    .getLogcatClient().getErrorHandlers();
            messageHandlers.getFirst().accept("secret-log-value");
            errorHandlers.getFirst().accept(new IllegalStateException("secret-error-value"));

            MobilePerformanceSample sample = new MobilePerformanceSample(Instant.now(), "secret.app",
                    "memoryinfo", List.of("metric"), List.of(List.of("secret-performance-value")));
            MobilePerformanceState.append(driver, sample);
            byte[] recordingBytes = "verified-recording".getBytes(StandardCharsets.UTF_8);
            MobileRecordingState.start(driver, MobileRecordingState.Owner.EXPLICIT, 1024, () -> { });
            MobileRecordingState.stop(driver, MobileRecordingState.Owner.EXPLICIT,
                    () -> Base64.getEncoder().encodeToString(recordingBytes));
            Files.write(recording, recordingBytes);
            MobileRecordingState.retainSavedRecording(driver, recording, recordingBytes);
            Set<Path> recordingStagesBefore = evidenceRecordingStages();
            Set<Path> archiveStagesBefore = evidenceArchiveStages();

            for (String secret : List.of("secret-log-value", "secret-error-value",
                    "secret.app", "secret-performance-value", recording.toString())) {
                FailureTraceReporter.registerSensitiveValue(secret);
                FailureTraceReporter.registerSensitiveSourceValue(secret);
            }

            MobileEvidenceBundle bundle = new SHAFT.GUI.WebDriver(driver).mobile().evidence().capture(target);
            Assert.assertEquals(evidenceRecordingStages(), recordingStagesBefore);
            Assert.assertEquals(evidenceArchiveStages(), archiveStagesBefore);

            Assert.assertEquals(bundle.logMessages().size(), 1);
            Assert.assertEquals(bundle.logErrors().size(), 1);
            Assert.assertEquals(bundle.performanceSamples().size(), 1);
            Assert.assertFalse(bundle.logMessages().getFirst().text().contains("secret-log-value"));
            Assert.assertFalse(bundle.logErrors().getFirst().message().contains("secret-error-value"));
            Assert.assertFalse(bundle.performanceSamples().getFirst().applicationId().contains("secret.app"));
            Assert.assertFalse(String.valueOf(bundle.performanceSamples().getFirst().rows())
                    .contains("secret-performance-value"));
            Assert.assertFalse(bundle.omissions().containsKey("recording"));
            Assert.assertEquals(MobileLogSource.snapshotIfPresent(driver).orElseThrow().messages().size(), 1);
            Assert.assertEquals(MobilePerformanceState.historyIfPresent(driver).orElseThrow().size(), 1);
            Assert.assertTrue(MobileRecordingState.snapshotIfPresent(driver).orElseThrow()
                    .savedRecording().isPresent());

            try (ZipFile archive = new ZipFile(target.toFile(), StandardCharsets.UTF_8)) {
                String manifest = new String(archive.getInputStream(archive.getEntry("mobile-evidence.json"))
                        .readAllBytes(), StandardCharsets.UTF_8);
                Assert.assertFalse(manifest.contains("secret-log-value"));
                Assert.assertFalse(manifest.contains("secret-error-value"));
                Assert.assertFalse(manifest.contains("secret-performance-value"));
                Assert.assertFalse(manifest.contains(recording.toString()));
                Assert.assertEquals(archive.getInputStream(archive.getEntry("artifacts/recording.mp4"))
                        .readAllBytes(), recordingBytes);
            }
        } finally {
            MobileLogSource.closeAndRemove(driver);
            MobilePerformanceState.closeAndRemove(driver);
            MobileRecordingState.closeAndRemove(driver);
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(screenshots)
                    .traceIncludeNativePageSource(nativeSource);
            Files.deleteIfExists(target);
            Files.deleteIfExists(recording);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void captureShouldApplyOneAggregateBudgetIncludingTheManifestAndMarkers() throws Exception {
        int previousMax = SHAFT.Properties.reporting.traceMaxArtifactMb();
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-budget-");
        Path target = directory.resolve("bundle.zip");
        try {
            SHAFT.Properties.reporting.set()
                    .traceMaxArtifactMb(1)
                    .traceIncludeScreenshots(true)
                    .traceIncludeNativePageSource(true);
            byte[] exactCapScreenshot = new byte[1024 * 1024];
            System.arraycopy(PNG, 0, exactCapScreenshot, 0, PNG.length);
            AppiumDriver driver = liveDriver();
            Mockito.when(driver.getScreenshotAs(OutputType.BYTES)).thenReturn(exactCapScreenshot);

            MobileEvidenceBundle bundle;
            try {
                bundle = new SHAFT.GUI.WebDriver(driver).mobile().evidence().capture(target);
            } catch (IllegalArgumentException aggregateFailure) {
                Assert.fail("A component that exhausts the aggregate budget must become an explicit omission.");
                return;
            }

            TraceArtifactReference screenshot = bundle.artifacts().stream()
                    .filter(reference -> reference.id().equals("screenshot")).findFirst().orElseThrow();
            Assert.assertTrue(screenshot.omitted());
            Assert.assertEquals(bundle.omissions().get("screenshot"), "oversized");
            try (ZipFile archive = new ZipFile(target.toFile(), StandardCharsets.UTF_8)) {
                long uncompressedBytes = archive.stream().mapToLong(ZipEntry::getSize).sum();
                Assert.assertTrue(uncompressedBytes <= 1024L * 1024L);
            }
        } finally {
            SHAFT.Properties.reporting.set()
                    .traceMaxArtifactMb(previousMax)
                    .traceIncludeScreenshots(screenshots)
                    .traceIncludeNativePageSource(nativeSource);
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void captureShouldRedactTheCurrentContextInTheDescriptorAndManifest() throws Exception {
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean domSource = SHAFT.Properties.reporting.traceIncludeDomSnapshots();
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-context-");
        Path target = directory.resolve("bundle.zip");
        try {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(false)
                    .traceIncludeDomSnapshots(false);
            AppiumDriver driver = liveDriver();
            Mockito.when(((SupportsContextSwitching) driver).getContext())
                    .thenReturn("WEBVIEW_secret-context", "WEBVIEW_secret-context");
            FailureTraceReporter.registerSensitiveValue("secret-context");
            FailureTraceReporter.registerSensitiveSourceValue("secret-context");

            MobileEvidenceBundle bundle = new SHAFT.GUI.WebDriver(driver).mobile().evidence().capture(target);

            Assert.assertFalse(bundle.context().contains("secret-context"));
            try (ZipFile archive = new ZipFile(target.toFile(), StandardCharsets.UTF_8)) {
                String manifest = new String(archive.getInputStream(archive.getEntry("mobile-evidence.json"))
                        .readAllBytes(), StandardCharsets.UTF_8);
                Assert.assertFalse(manifest.contains("secret-context"));
            }
        } finally {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(screenshots)
                    .traceIncludeDomSnapshots(domSource);
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void captureShouldFailClosedBeforeProviderOrTargetMutationForInvalidAndStaleSessions() throws Exception {
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-preflight-");
        Path staleParent = directory.resolve("must-not-exist");
        try {
            AppiumDriver invalidDriver = liveDriver();
            MobileEvidenceActionsContract invalid = new SHAFT.GUI.WebDriver(invalidDriver).mobile().evidence();
            Mockito.clearInvocations(invalidDriver);
            Assert.expectThrows(IllegalArgumentException.class, () -> invalid.capture(directory));
            Mockito.verifyNoInteractions(invalidDriver);

            Path parentFile = directory.resolve("parent-file");
            Files.writeString(parentFile, "keep", StandardCharsets.UTF_8);
            Mockito.clearInvocations(invalidDriver);
            Assert.expectThrows(IllegalArgumentException.class,
                    () -> invalid.capture(parentFile.resolve("bundle.zip")));
            Mockito.verifyNoInteractions(invalidDriver);
            Assert.assertEquals(Files.readString(parentFile, StandardCharsets.UTF_8), "keep");

            AppiumDriver staleDriver = liveDriver();
            MobileEvidenceActionsContract stale = new SHAFT.GUI.WebDriver(staleDriver).mobile().evidence();
            Mockito.when(staleDriver.getSessionId()).thenReturn(null);
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> stale.capture(staleParent.resolve("bundle.zip")));
            Assert.assertFalse(Files.exists(staleParent));
        } finally {
            Files.deleteIfExists(directory.resolve("parent-file"));
            Files.deleteIfExists(staleParent.resolve("bundle.zip"));
            Files.deleteIfExists(staleParent);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void teardownBeforePublicationShouldLeaveTheExistingTargetUntouched() throws Exception {
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-close-");
        Path target = directory.resolve("bundle.zip");
        Files.writeString(target, "old-target", StandardCharsets.UTF_8);
        AppiumDriver driver = liveDriver();
        try {
            MobileEvidenceState.begin(driver);
            MobileEvidenceState.closeAndRemove(driver);
            Assert.expectThrows(UnsupportedOperationException.class,
                    () -> MobileEvidenceState.publish(driver, () -> {
                        try {
                            Files.writeString(target, "new-target", StandardCharsets.UTF_8);
                        } catch (java.io.IOException exception) {
                            throw new IllegalStateException(exception);
                        }
                    }));
            Assert.assertEquals(Files.readString(target, StandardCharsets.UTF_8), "old-target");
        } finally {
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void realDriverTeardownShouldTerminalizeEvidencePublication() {
        AppiumDriver driver = liveDriver();
        MobileEvidenceState.begin(driver);
        new DriverFactoryHelper().closeDriver(driver);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> MobileEvidenceState.publish(driver, () -> Assert.fail("Closed evidence must not publish.")));
    }

    @Test
    public void failedPublicationShouldDeleteThePreparedRecordingStage() throws Exception {
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-failed-publish-");
        Path recording = directory.resolve("saved.mp4");
        AppiumDriver driver = liveDriver();
        byte[] recordingBytes = "recording-to-clean".getBytes(StandardCharsets.UTF_8);
        try {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(false).traceIncludeNativePageSource(false);
            MobileRecordingState.start(driver, MobileRecordingState.Owner.EXPLICIT, 1024, () -> { });
            MobileRecordingState.stop(driver, MobileRecordingState.Owner.EXPLICIT,
                    () -> Base64.getEncoder().encodeToString(recordingBytes));
            Files.write(recording, recordingBytes);
            MobileRecordingState.retainSavedRecording(driver, recording, recordingBytes);
            Set<Path> before = evidenceRecordingStages();
            Set<Path> archivesBefore = evidenceArchiveStages();

            Assert.expectThrows(IllegalStateException.class,
                    () -> new SHAFT.GUI.WebDriver(driver).mobile().evidence().capture(failingPublicationTarget()));
            Assert.assertEquals(evidenceRecordingStages(), before);
            Assert.assertEquals(evidenceArchiveStages(), archivesBefore);
        } finally {
            MobileRecordingState.closeAndRemove(driver);
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(screenshots).traceIncludeNativePageSource(nativeSource);
            Files.deleteIfExists(recording);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void targetPublicationLockShouldSerializeSameTargetWriters() throws Exception {
        Path target = Files.createTempDirectory("shaft-mobile-evidence-lock-").resolve("bundle.zip");
        AtomicInteger active = new AtomicInteger();
        AtomicInteger maximum = new AtomicInteger();
        CountDownLatch firstEntered = new CountDownLatch(1);
        CountDownLatch secondStarted = new CountDownLatch(1);
        AtomicReference<Thread> secondThread = new AtomicReference<>();
        CountDownLatch releaseFirst = new CountDownLatch(1);
        var executor = Executors.newFixedThreadPool(2);
        try {
            var first = executor.submit(() -> MobileEvidenceArchiveWriter.withTargetLock(target, () -> {
                maximum.accumulateAndGet(active.incrementAndGet(), Math::max);
                firstEntered.countDown();
                try {
                    Assert.assertTrue(releaseFirst.await(10, TimeUnit.SECONDS));
                } catch (InterruptedException exception) {
                    Thread.currentThread().interrupt();
                    throw new AssertionError(exception);
                } finally {
                    active.decrementAndGet();
                }
            }));
            Assert.assertTrue(firstEntered.await(10, TimeUnit.SECONDS));
            var second = executor.submit(() -> {
                secondThread.set(Thread.currentThread());
                secondStarted.countDown();
                MobileEvidenceArchiveWriter.withTargetLock(target, () -> {
                    maximum.accumulateAndGet(active.incrementAndGet(), Math::max);
                    active.decrementAndGet();
                });
            });
            Assert.assertTrue(secondStarted.await(10, TimeUnit.SECONDS));
            awaitState(secondThread.get(), Thread.State.BLOCKED);
            releaseFirst.countDown();
            first.get(10, TimeUnit.SECONDS);
            second.get(10, TimeUnit.SECONDS);
            Assert.assertEquals(maximum.get(), 1);
            var locks = MobileEvidenceArchiveWriter.class.getDeclaredField("TARGET_LOCKS");
            locks.setAccessible(true);
            Assert.assertTrue(((Map<?, ?>) locks.get(null)).isEmpty());
        } finally {
            releaseFirst.countDown();
            executor.shutdownNow();
            Files.deleteIfExists(target.getParent());
        }
    }

    @Test
    public void waitingForAnotherDriversTargetShouldNotBlockTeardown() throws Exception {
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-lock-order-");
        Path target = directory.resolve("bundle.zip");
        Files.writeString(target, "old-target", StandardCharsets.UTF_8);
        AppiumDriver waitingDriver = liveDriver();
        MobileEvidenceState.begin(waitingDriver);
        CountDownLatch firstEntered = new CountDownLatch(1);
        CountDownLatch releaseFirst = new CountDownLatch(1);
        CountDownLatch waitingStarted = new CountDownLatch(1);
        AtomicReference<Thread> waitingThread = new AtomicReference<>();
        var executor = Executors.newFixedThreadPool(3);
        try (MobileEvidenceArchiveWriter.StagedArchive staged = MobileEvidenceArchiveWriter.stage(
                "{}".getBytes(StandardCharsets.UTF_8), List.of(), 1024)) {
            var first = executor.submit(() -> MobileEvidenceArchiveWriter.withTargetLock(target, () -> {
                firstEntered.countDown();
                try {
                    Assert.assertTrue(releaseFirst.await(10, TimeUnit.SECONDS));
                } catch (InterruptedException exception) {
                    Thread.currentThread().interrupt();
                    throw new AssertionError(exception);
                }
            }));
            Assert.assertTrue(firstEntered.await(10, TimeUnit.SECONDS));
            var waiting = executor.submit(() -> {
                waitingThread.set(Thread.currentThread());
                waitingStarted.countDown();
                MobileEvidenceArchiveWriter.publish(waitingDriver, staged, target, () -> { });
            });
            Assert.assertTrue(waitingStarted.await(10, TimeUnit.SECONDS));
            awaitState(waitingThread.get(), Thread.State.BLOCKED);
            var close = executor.submit(() -> new DriverFactoryHelper().closeDriver(waitingDriver));
            close.get(10, TimeUnit.SECONDS);
            Assert.assertFalse(waiting.isDone());
            releaseFirst.countDown();
            first.get(10, TimeUnit.SECONDS);
            Assert.expectThrows(java.util.concurrent.ExecutionException.class,
                    () -> waiting.get(10, TimeUnit.SECONDS));
            Assert.assertEquals(Files.readString(target, StandardCharsets.UTF_8), "old-target");
        } finally {
            releaseFirst.countDown();
            executor.shutdownNow();
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void largeAllowlistedMetadataShouldBeOmitted() throws Exception {
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-metadata-");
        Path target = directory.resolve("bundle.zip");
        try {
            SHAFT.Properties.reporting.set().traceIncludeScreenshots(false).traceIncludeNativePageSource(false);
            AppiumDriver driver = liveDriver();
            Mockito.when(driver.getCapabilities()).thenReturn(new ImmutableCapabilities(
                    "platformName", "Android", "platformVersion", "x".repeat(2 * 1024 * 1024)));
            MobileEvidenceBundle bundle = new SHAFT.GUI.WebDriver(driver).mobile().evidence().capture(target);
            Assert.assertFalse(bundle.deviceMetadata().containsKey("platformVersion"));
        } finally {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(screenshots).traceIncludeNativePageSource(nativeSource);
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void capsAboveTwoGibibytesShouldStillAcceptASmallManifest() throws Exception {
        int previousMax = SHAFT.Properties.reporting.traceMaxArtifactMb();
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-large-cap-");
        Path target = directory.resolve("bundle.zip");
        try {
            SHAFT.Properties.reporting.set().traceMaxArtifactMb(2048)
                    .traceIncludeScreenshots(false).traceIncludeNativePageSource(false);
            MobileEvidenceBundle bundle = new SHAFT.GUI.WebDriver(liveDriver()).mobile().evidence().capture(target);
            Assert.assertTrue(Files.isRegularFile(bundle.archive()));
        } finally {
            SHAFT.Properties.reporting.set().traceMaxArtifactMb(previousMax)
                    .traceIncludeScreenshots(screenshots).traceIncludeNativePageSource(nativeSource);
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void oversizedSingleLogValueShouldBeOmittedWithinTheAggregateBudget() throws Exception {
        int previousMax = SHAFT.Properties.reporting.traceMaxArtifactMb();
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-large-log-");
        Path target = directory.resolve("bundle.zip");
        AppiumDriver driver = liveDriver();
        MobileLogSource.Snapshot snapshot = new MobileLogSource.Snapshot(true,
                List.of(new com.shaft.gui.driver.MobileLogMessage(Instant.now(), "logcat", "x".repeat(2 * 1024 * 1024))),
                List.of());
        try (MockedStatic<MobileLogSource> logs = Mockito.mockStatic(MobileLogSource.class,
                Mockito.CALLS_REAL_METHODS)) {
            SHAFT.Properties.reporting.set().traceMaxArtifactMb(1)
                    .traceIncludeScreenshots(false).traceIncludeNativePageSource(false);
            logs.when(() -> MobileLogSource.snapshotIfPresent(driver)).thenReturn(Optional.of(snapshot));
            MobileEvidenceBundle bundle = new SHAFT.GUI.WebDriver(driver).mobile().evidence().capture(target);
            Assert.assertEquals(bundle.omissions().get("logs"), "oversized");
            Assert.assertTrue(bundle.logMessages().isEmpty());
            try (ZipFile archive = new ZipFile(target.toFile(), StandardCharsets.UTF_8)) {
                Assert.assertTrue(archive.stream().mapToLong(ZipEntry::getSize).sum() <= 1024L * 1024L);
            }
        } finally {
            SHAFT.Properties.reporting.set().traceMaxArtifactMb(previousMax)
                    .traceIncludeScreenshots(screenshots).traceIncludeNativePageSource(nativeSource);
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void captureShouldReplaceOnlyTheExactSymlinkEntry() throws Exception {
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-link-");
        Path referent = directory.resolve("outside.txt");
        Path target = directory.resolve("bundle.zip");
        Files.writeString(referent, "keep-referent", StandardCharsets.UTF_8);
        try {
            try {
                Files.createSymbolicLink(target, referent.getFileName());
            } catch (UnsupportedOperationException | java.nio.file.FileSystemException unavailable) {
                throw new org.testng.SkipException("Symbolic links are unavailable on this host", unavailable);
            }
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(false)
                    .traceIncludeNativePageSource(false);

            new SHAFT.GUI.WebDriver(liveDriver()).mobile().evidence().capture(target);

            Assert.assertFalse(Files.isSymbolicLink(target));
            Assert.assertEquals(Files.readString(referent, StandardCharsets.UTF_8), "keep-referent");
            try (ZipFile archive = new ZipFile(target.toFile(), StandardCharsets.UTF_8)) {
                Assert.assertNotNull(archive.getEntry("mobile-evidence.json"));
            }
            try (var children = Files.list(directory)) {
                Assert.assertTrue(children.map(path -> path.getFileName().toString())
                        .noneMatch(name -> name.contains(".tmp-") || name.contains(".backup-")));
            }
        } finally {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(screenshots)
                    .traceIncludeNativePageSource(nativeSource);
            Files.deleteIfExists(target);
            Files.deleteIfExists(referent);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void captureShouldOmitChangedAndActiveRecordingsWithoutStoppingThem() throws Exception {
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-recording-");
        Path recording = directory.resolve("saved.mp4");
        Path changedTarget = directory.resolve("changed.zip");
        Path activeTarget = directory.resolve("active.zip");
        AppiumDriver driver = liveDriver();
        try {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(false)
                    .traceIncludeNativePageSource(false);
            byte[] original = "original-recording".getBytes(StandardCharsets.UTF_8);
            MobileRecordingState.start(driver, MobileRecordingState.Owner.EXPLICIT, 1024, () -> { });
            MobileRecordingState.stop(driver, MobileRecordingState.Owner.EXPLICIT,
                    () -> Base64.getEncoder().encodeToString(original));
            Files.write(recording, original);
            MobileRecordingState.retainSavedRecording(driver, recording, original);
            Files.writeString(recording, "changed-recording", StandardCharsets.UTF_8);

            MobileEvidenceBundle changed = new SHAFT.GUI.WebDriver(driver).mobile().evidence().capture(changedTarget);
            Assert.assertEquals(changed.omissions().get("recording"), "changed");
            Assert.assertTrue(changed.artifacts().stream().filter(reference -> reference.id().equals("recording"))
                    .findFirst().orElseThrow().omitted());

            Files.write(recording, original);
            MobileRecordingState.start(driver, MobileRecordingState.Owner.EXPLICIT, 1024, () -> { });
            MobileEvidenceBundle active = new SHAFT.GUI.WebDriver(driver).mobile().evidence().capture(activeTarget);
            Assert.assertEquals(active.omissions().get("recording"), "active");
            Assert.assertTrue(MobileRecordingState.snapshotIfPresent(driver).orElseThrow().recordingInProgress());
            Assert.assertTrue(MobileRecordingState.snapshotIfPresent(driver).orElseThrow()
                    .savedRecording().isPresent());
        } finally {
            MobileRecordingState.closeAndRemove(driver);
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(screenshots)
                    .traceIncludeNativePageSource(nativeSource);
            Files.deleteIfExists(activeTarget);
            Files.deleteIfExists(changedTarget);
            Files.deleteIfExists(recording);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void captureShouldReadOneAtomicLogSnapshot() throws Exception {
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-log-snapshot-");
        Path target = directory.resolve("bundle.zip");
        AppiumDriver driver = liveDriver();
        MobileLogSource.Snapshot snapshot = new MobileLogSource.Snapshot(true,
                List.of(new com.shaft.gui.driver.MobileLogMessage(Instant.now(), "logcat", "message")),
                List.of(new com.shaft.gui.driver.MobileLogError(Instant.now(), "logcat",
                        IllegalStateException.class.getName(), "error")));
        try (MockedStatic<MobileLogSource> logs = Mockito.mockStatic(
                MobileLogSource.class, Mockito.CALLS_REAL_METHODS)) {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(false)
                    .traceIncludeNativePageSource(false);
            logs.when(() -> MobileLogSource.snapshotIfPresent(driver)).thenReturn(Optional.of(snapshot));

            MobileEvidenceBundle bundle = new SHAFT.GUI.WebDriver(driver).mobile().evidence().capture(target);

            Assert.assertEquals(bundle.logMessages().size(), 1);
            Assert.assertEquals(bundle.logErrors().size(), 1);
            logs.verify(() -> MobileLogSource.snapshotIfPresent(driver), Mockito.times(1));
        } finally {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(screenshots)
                    .traceIncludeNativePageSource(nativeSource);
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void manifestSerializationShouldStopAtTheSuppliedByteLimit() throws Exception {
        Method bounded;
        try {
            bounded = Class.forName("com.shaft.gui.mobile.MobileEvidenceArchiveWriter")
                    .getDeclaredMethod("serializeBounded", Object.class, long.class);
        } catch (ReflectiveOperationException missingBoundedSerializer) {
            Assert.fail("Mobile evidence needs a bounded streaming manifest serializer.");
            return;
        }
        bounded.setAccessible(true);
        @SuppressWarnings("unchecked")
        Optional<byte[]> result = (Optional<byte[]>) bounded.invoke(null,
                Map.of("payload", "x".repeat(2 * 1024 * 1024)), 1024L);
        Assert.assertTrue(result.isEmpty());
    }

    @Test
    public void captureShouldRegisterTargetPathsBeforePreflightRejection() throws Exception {
        Path directory = Files.createTempDirectory("shaft-mobile-evidence-private-target-");
        try {
            String sensitiveTarget = directory.toString();
            Assert.expectThrows(IllegalArgumentException.class,
                    () -> new SHAFT.GUI.WebDriver(liveDriver()).mobile().evidence().capture(directory));
            Assert.assertFalse(FailureTraceReporter.redactInvocationText(sensitiveTarget)
                    .contains(sensitiveTarget));
        } finally {
            Files.deleteIfExists(directory);
        }
    }

    @Test
    public void evidencePublicationShouldExposeLifecycleAndTargetSerializationOwners() throws Exception {
        try {
            Class<?> lifecycle = Class.forName("com.shaft.gui.mobile.internal.MobileEvidenceState");
            lifecycle.getDeclaredMethod("begin", AppiumDriver.class);
            lifecycle.getDeclaredMethod("publish", AppiumDriver.class, Runnable.class);
            Class<?> writer = Class.forName("com.shaft.gui.mobile.MobileEvidenceArchiveWriter");
            writer.getDeclaredMethod("withTargetLock", Path.class, Runnable.class);
        } catch (ReflectiveOperationException missingOwner) {
            Assert.fail("Evidence publication needs teardown and same-target serialization owners.");
        }
    }

    private static AppiumDriver liveDriver() {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class, Mockito.withSettings()
                .extraInterfaces(SupportsContextSwitching.class)
                .defaultAnswer(Mockito.RETURNS_DEEP_STUBS));
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("evidence-archive"));
        Mockito.when(driver.getCapabilities()).thenReturn(new ImmutableCapabilities(
                "platformName", "Android",
                "appium:appPackage", "example.app"));
        Mockito.when(driver.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        Mockito.when(driver.getPageSource()).thenReturn("<node text=\"visible\"/>");
        Mockito.when(((SupportsContextSwitching) driver).getContext())
                .thenReturn("NATIVE_APP", "NATIVE_APP");
        return driver;
    }

    private static AppiumDriver statefulDriver() {
        AppiumDriver driver = Mockito.mock(AppiumDriver.class, Mockito.withSettings()
                .extraInterfaces(SupportsContextSwitching.class, ListensToLogcatMessages.class)
                .defaultAnswer(Mockito.RETURNS_DEEP_STUBS));
        StringWebSocketClient client = Mockito.mock(StringWebSocketClient.class);
        CopyOnWriteArrayList<Consumer<String>> messages = new CopyOnWriteArrayList<>();
        CopyOnWriteArrayList<Consumer<Throwable>> errors = new CopyOnWriteArrayList<>();
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("evidence-state"));
        Mockito.when(driver.getCapabilities()).thenReturn(new ImmutableCapabilities(
                "platformName", "Android",
                "appium:appPackage", "secret.app"));
        Mockito.when(((ListensToLogcatMessages) driver).getLogcatClient()).thenReturn(client);
        Mockito.when(client.isListening()).thenReturn(true);
        Mockito.when(client.getMessageHandlers()).thenReturn(messages);
        Mockito.when(client.getErrorHandlers()).thenReturn(errors);
        Mockito.when(client.getConnectionHandlers()).thenReturn(new CopyOnWriteArrayList<>());
        Mockito.when(client.getDisconnectionHandlers()).thenReturn(new CopyOnWriteArrayList<>());
        Mockito.doAnswer(invocation -> messages.add(invocation.getArgument(0)))
                .when((ListensToLogcatMessages) driver).addLogcatMessagesListener(Mockito.any());
        Mockito.doAnswer(invocation -> errors.add(invocation.getArgument(0)))
                .when((ListensToLogcatMessages) driver).addLogcatErrorsListener(Mockito.any());
        return driver;
    }

    private static Set<Path> evidenceRecordingStages() throws java.io.IOException {
        Path temporary = Path.of(System.getProperty("java.io.tmpdir"));
        try (var entries = Files.list(temporary)) {
            return entries.filter(path -> path.getFileName().toString()
                            .startsWith("shaft-mobile-evidence-recording-"))
                    .map(path -> path.toAbsolutePath().normalize())
                    .collect(Collectors.toUnmodifiableSet());
        }
    }

    private static Set<Path> evidenceArchiveStages() throws java.io.IOException {
        Path temporary = Path.of(System.getProperty("java.io.tmpdir"));
        try (var entries = Files.list(temporary)) {
            return entries.filter(path -> path.getFileName().toString().startsWith("shaft-mobile-evidence-")
                            && path.getFileName().toString().endsWith(".zip"))
                    .map(path -> path.toAbsolutePath().normalize())
                    .collect(Collectors.toUnmodifiableSet());
        }
    }

    private static Path failingPublicationTarget() {
        if (System.getProperty("os.name", "").toLowerCase().contains("win")) {
            Set<String> roots = new java.util.HashSet<>();
            FileSystems.getDefault().getRootDirectories().forEach(root ->
                    roots.add(root.toString().substring(0, 1).toUpperCase()));
            for (char drive = 'Z'; drive >= 'A'; drive--) {
                if (!roots.contains(String.valueOf(drive))) {
                    return Path.of(drive + ":\\shaft-mobile-evidence-unavailable\\bundle.zip");
                }
            }
        }
        return Path.of("/proc/shaft-mobile-evidence-unavailable/bundle.zip");
    }

    private static void awaitState(Thread thread, Thread.State expected) {
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
        while (thread.getState() != expected && thread.isAlive() && System.nanoTime() < deadline) {
            Thread.onSpinWait();
        }
        Assert.assertEquals(thread.getState(), expected);
    }
}
