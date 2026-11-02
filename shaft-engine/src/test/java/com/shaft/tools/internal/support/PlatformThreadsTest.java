package com.shaft.tools.internal.support;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/** Issue #6871: native, CPU-bound and latency-critical work stays off virtual threads. */
public class PlatformThreadsTest {
    private static final Path MAIN = Path.of("src/main/java/com/shaft");
    private static final List<String> PLATFORM_ONLY = List.of(
            "tools/io/internal/PdfDocumentProcessor.java",
            "tools/io/internal/PdfBatchProcessor.java",
            "gui/browser/internal/CdpFetchRuleInterceptor.java",
            "gui/browser/internal/CdpPassiveNetworkObserver.java");

    @Test
    public void factoryCreatesNamedDaemonPlatformThreads() {
        Thread thread = PlatformThreads.daemonFactory("shaft-test").newThread(() -> { });
        Assert.assertFalse(thread.isVirtual());
        Assert.assertTrue(thread.isDaemon());
        Assert.assertEquals(thread.getName(), "shaft-test-1");
    }

    @Test
    public void startDaemonRunsTheTaskOnAPlatformThread() throws Exception {
        CompletableFuture<Thread> ran = new CompletableFuture<>();
        PlatformThreads.startDaemon("shaft-start", () -> ran.complete(Thread.currentThread()));
        Thread thread = ran.get(5, TimeUnit.SECONDS);
        Assert.assertFalse(thread.isVirtual());
        Assert.assertTrue(thread.isDaemon());
        Assert.assertEquals(thread.getName(), "shaft-start");
    }

    @Test
    public void carrierHoldingWorkDoesNotUseVirtualThreads() throws IOException {
        for (String relative : PLATFORM_ONLY) {
            String source = Files.readString(MAIN.resolve(relative));
            Assert.assertFalse(source.contains("ofVirtual") || source.contains("newVirtualThreadPerTaskExecutor"),
                    relative + " must use platform threads (#6871)");
        }
        String properties = Files.readString(MAIN.resolve("properties/internal/PropertiesHelper.java"));
        Assert.assertFalse(properties.contains("Thread.ofVirtual().start(ImageProcessingActions::loadOpenCVIfAvailable)"));
    }
}
