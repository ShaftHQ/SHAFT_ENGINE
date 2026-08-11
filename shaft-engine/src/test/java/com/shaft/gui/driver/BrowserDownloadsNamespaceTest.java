package com.shaft.gui.driver;

import org.testng.Assert;
import org.testng.annotations.Test;
import org.mockito.Mockito;
import org.openqa.selenium.HasDownloads;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.remote.SessionId;
import io.appium.java_client.AppiumDriver;
import com.shaft.gui.capabilities.AutomationFeature;
import com.shaft.gui.capabilities.internal.AutomationCapabilityResolver;
import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Download;
import com.microsoft.playwright.Page;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.driver.SHAFT;

import java.time.Instant;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

public class BrowserDownloadsNamespaceTest {
    @Test
    public void downloadsNamespaceShouldExposeOneCompactBackendNeutralContract() throws Exception {
        boolean discoverable = Arrays.stream(BrowserActionsContract.class.getMethods())
                .anyMatch(method -> method.getName().equals("downloads")
                        && method.getParameterCount() == 0
                        && method.getReturnType().getSimpleName().equals("DownloadActionsContract"));

        Assert.assertTrue(discoverable);
        Assert.assertTrue(BrowserActionsContract.class.getMethod("downloads").isDefault());
        Assert.assertEquals(Class.forName("com.shaft.gui.browser.BrowserActions")
                .getDeclaredMethod("downloads").getReturnType().getSimpleName(), "DownloadActions");
        Assert.assertEquals(Class.forName("com.shaft.gui.playwright.browser.BrowserActions")
                .getDeclaredMethod("downloads").getReturnType().getSimpleName(), "DownloadActions");
        Assert.assertEquals(descriptors(Class.forName("com.shaft.gui.driver.DownloadActionsContract")), Set.of(
                "all[]->List",
                "and[]->BrowserActionsContract",
                "clear[]->DownloadActionsContract",
                "latest[]->BrowserDownload",
                "waitFor[interface java.lang.Runnable]->BrowserDownload",
                "waitFor[interface java.util.function.Predicate, interface java.lang.Runnable]->BrowserDownload"));
        Assert.assertEquals(descriptors(Class.forName("com.shaft.gui.driver.BrowserDownload")), Set.of(
                "and[]->DownloadActionsContract",
                "cancel[]->BrowserDownload",
                "creationTime[]->Optional",
                "delete[]->DownloadActionsContract",
                "failure[]->Optional",
                "lastModifiedTime[]->Optional",
                "saveAs[interface java.nio.file.Path]->BrowserDownload",
                "size[]->OptionalLong",
                "suggestedFilename[]->String",
                "url[]->Optional"));
    }

    @Test
    public void seleniumShouldExposeDownloadedMetadataAndClearThroughTheNativeOwner() {
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("downloads"));
        Mockito.when(driver.isDownloadsEnabled()).thenReturn(true);
        Mockito.when(driver.getDownloadedFiles()).thenReturn(List.of(
                new HasDownloads.DownloadedFile("first.txt", 1_000, 2_000, 12),
                new HasDownloads.DownloadedFile("second.pdf", 3_000, 4_000, 42)));
        DownloadActionsContract downloads = new com.shaft.gui.browser.BrowserActions(driver, true).downloads();

        List<BrowserDownload> files;
        try {
            files = downloads.all();
        } catch (UnsupportedOperationException missingBehavior) {
            files = List.of();
        }

        Assert.assertEquals(files.size(), 2);
        Assert.assertEquals(files.get(1).suggestedFilename(), "second.pdf");
        Assert.assertEquals(files.get(1).size().orElseThrow(), 42);
        Assert.assertEquals(files.get(1).creationTime().orElseThrow(), Instant.ofEpochMilli(3_000));
        Assert.assertEquals(files.get(1).lastModifiedTime().orElseThrow(), Instant.ofEpochMilli(4_000));
        Assert.assertTrue(files.get(1).url().isEmpty());
        Assert.assertTrue(files.get(1).failure().isEmpty());
        Assert.assertSame(downloads.latest().and(), downloads);
        Assert.assertSame(downloads.clear(), downloads);
        Mockito.verify(driver).deleteDownloadableFiles();
    }

    @Test
    public void downloadsShouldRequireAnEnabledLiveNonAppiumSession() {
        RemoteWebDriver disabled = Mockito.mock(RemoteWebDriver.class);
        Mockito.when(disabled.getSessionId()).thenReturn(new SessionId("disabled"));
        Mockito.when(disabled.isDownloadsEnabled()).thenReturn(false);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(disabled, true).downloads().all());
        Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(disabled)
                .supports(AutomationFeature.DOWNLOADS));

        RemoteWebDriver closed = Mockito.mock(RemoteWebDriver.class);
        Mockito.when(closed.isDownloadsEnabled()).thenReturn(true);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(closed, true).downloads().all());
        Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(closed)
                .supports(AutomationFeature.DOWNLOADS));

        AppiumDriver appium = Mockito.mock(AppiumDriver.class);
        Mockito.when(appium.getSessionId()).thenReturn(new SessionId("appium"));
        Mockito.when(appium.isDownloadsEnabled()).thenReturn(true);
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(appium, true).downloads().all());
        Assert.assertFalse(AutomationCapabilityResolver.forWebDriver(appium)
                .supports(AutomationFeature.DOWNLOADS));
    }

    @Test
    public void seleniumShouldArmBeforeTriggerWaitForANewDownloadAndSaveToTheExactTarget() throws Exception {
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("downloads"));
        Mockito.when(driver.isDownloadsEnabled()).thenReturn(true);
        var existing = new HasDownloads.DownloadedFile("existing.txt", 1_000, 1_000, 1);
        var downloaded = new HasDownloads.DownloadedFile("report.pdf", 2_000, 3_000, 7);
        Mockito.when(driver.getDownloadedFiles())
                .thenReturn(List.of(existing), List.of(existing), List.of(existing, downloaded));
        Mockito.doAnswer(invocation -> {
            String filename = invocation.getArgument(0);
            Path directory = invocation.getArgument(1);
            Files.writeString(directory.resolve(filename), "payload");
            return null;
        }).when(driver).downloadFile(Mockito.eq("report.pdf"), Mockito.any(Path.class));
        AtomicInteger triggerCount = new AtomicInteger();
        DownloadActionsContract downloads = new com.shaft.gui.browser.BrowserActions(driver, true).downloads();

        BrowserDownload result = null;
        try {
            result = downloads.waitFor(file -> file.suggestedFilename().endsWith(".pdf"),
                    triggerCount::incrementAndGet);
        } catch (UnsupportedOperationException missingBehavior) {
            // RED: the namespace exists before the trigger-aware adapter is implemented.
        }
        Assert.assertNotNull(result);
        Path output = Files.createTempDirectory("shaft-download-target").resolve("renamed.pdf");
        result.saveAs(output);

        Assert.assertEquals(triggerCount.get(), 1);
        Assert.assertEquals(result.suggestedFilename(), "report.pdf");
        Assert.assertEquals(Files.readString(output), "payload");
    }

    @Test
    public void playwrightShouldWaitPersistCancelDeleteAndClearNativeDownloads() throws Exception {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Download first = Mockito.mock(Download.class);
        Download second = Mockito.mock(Download.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);
        Mockito.when(first.suggestedFilename()).thenReturn("report.pdf");
        Mockito.when(first.url()).thenReturn("https://example.test/report.pdf");
        Mockito.when(first.failure()).thenReturn(null);
        Mockito.when(second.suggestedFilename()).thenReturn("other.txt");
        Mockito.when(session.downloadSnapshot()).thenReturn(
                List.of(first, second), List.of(first, second), List.of(second));
        AtomicInteger triggerCount = new AtomicInteger();
        Mockito.when(page.waitForDownload(Mockito.any(Page.WaitForDownloadOptions.class), Mockito.any(Runnable.class)))
                .thenAnswer(invocation -> {
                    Page.WaitForDownloadOptions options = invocation.getArgument(0);
                    Assert.assertTrue(options.predicate.test(first));
                    invocation.<Runnable>getArgument(1).run();
                    return first;
                });
        AtomicReference<Path> providerTarget = new AtomicReference<>();
        Mockito.doAnswer(invocation -> {
            Path path = invocation.getArgument(0);
            providerTarget.set(path);
            Files.writeString(path, "payload");
            return null;
        }).when(first).saveAs(Mockito.any(Path.class));
        DownloadActionsContract downloads = new com.shaft.gui.playwright.browser.BrowserActions(session).downloads();

        BrowserDownload result = null;
        try {
            result = downloads.waitFor(file -> file.suggestedFilename().endsWith(".pdf"),
                    triggerCount::incrementAndGet);
        } catch (UnsupportedOperationException missingBehavior) {
            // RED: the Playwright adapter is not implemented yet.
        }
        Assert.assertNotNull(result);
        Path target = Files.createTempDirectory("shaft-playwright-download").resolve("renamed.pdf");
        result.saveAs(target).cancel();
        Assert.assertEquals(result.suggestedFilename(), "report.pdf");
        Assert.assertEquals(result.url().orElseThrow(), "https://example.test/report.pdf");
        Assert.assertTrue(result.failure().isEmpty());
        Assert.assertEquals(triggerCount.get(), 1);
        Assert.assertEquals(Files.readString(target), "payload");
        Assert.assertNotEquals(providerTarget.get(), target.toAbsolutePath().normalize());
        Assert.assertFalse(Files.exists(providerTarget.get()));
        Mockito.verify(first).cancel();
        Assert.assertSame(result.delete(), downloads);
        Mockito.verify(first).delete();
        Mockito.verify(session).forgetDownload(first);

        Assert.assertEquals(downloads.all().size(), 2);
        Assert.assertEquals(downloads.latest().suggestedFilename(), "other.txt");
        Assert.assertSame(downloads.clear(), downloads);
        Mockito.verify(second).delete();
        Mockito.verify(session).forgetDownload(second);
    }

    @Test
    public void playwrightDownloadInventoryShouldRemainAvailableWithoutAPageButWaitingShouldNot() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Download download = Mockito.mock(Download.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.downloadSnapshot()).thenReturn(List.of(download));
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(download.suggestedFilename()).thenReturn("retained.txt");
        DownloadActionsContract downloads = new com.shaft.gui.playwright.browser.BrowserActions(session).downloads();

        Assert.assertEquals(downloads.latest().suggestedFilename(), "retained.txt");
        Assert.assertTrue(AutomationCapabilityResolver.forPlaywright(session)
                .supports(AutomationFeature.DOWNLOADS));
        Assert.expectThrows(UnsupportedOperationException.class,
                () -> downloads.waitFor(() -> { }));

        Mockito.when(context.isClosed()).thenReturn(true);
        Assert.expectThrows(UnsupportedOperationException.class, downloads::all);
        Assert.assertFalse(AutomationCapabilityResolver.forPlaywright(session)
                .supports(AutomationFeature.DOWNLOADS));
    }

    @Test
    public void seleniumShouldRejectUnsafeProviderFilenamesBeforeWriting() throws Exception {
        for (String unsafe : List.of("", "../escape.txt", "folder/file.txt", "C:\\absolute.txt")) {
            RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class);
            Mockito.when(driver.getSessionId()).thenReturn(new SessionId("downloads"));
            Mockito.when(driver.isDownloadsEnabled()).thenReturn(true);
            Mockito.when(driver.getDownloadedFiles()).thenReturn(List.of(
                    new HasDownloads.DownloadedFile(unsafe, 1, 2, 3)));
            BrowserDownload download = new com.shaft.gui.browser.BrowserActions(driver, true)
                    .downloads().latest();

            Assert.expectThrows(IllegalArgumentException.class,
                    () -> download.saveAs(Files.createTempDirectory("shaft-unsafe-download").resolve("safe.txt")));
            Mockito.verify(driver, Mockito.never()).downloadFile(Mockito.anyString(), Mockito.any(Path.class));
        }
    }

    @Test
    public void seleniumSaveFailureShouldPreserveTheProviderExceptionAndRemovePartialStaging() throws Exception {
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("downloads"));
        Mockito.when(driver.isDownloadsEnabled()).thenReturn(true);
        Mockito.when(driver.getDownloadedFiles()).thenReturn(List.of(
                new HasDownloads.DownloadedFile("report.pdf", 1, 2, 3)));
        AtomicReference<Path> staging = new AtomicReference<>();
        IllegalStateException providerFailure = new IllegalStateException("provider failed");
        Mockito.doAnswer(invocation -> {
            Path directory = invocation.getArgument(1);
            staging.set(directory);
            Files.writeString(directory.resolve("partial.tmp"), "partial");
            throw providerFailure;
        }).when(driver).downloadFile(Mockito.eq("report.pdf"), Mockito.any(Path.class));
        BrowserDownload download = new com.shaft.gui.browser.BrowserActions(driver, true)
                .downloads().latest();

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class,
                () -> download.saveAs(Files.createTempDirectory("shaft-download-failure").resolve("saved.pdf")));

        Assert.assertSame(thrown, providerFailure);
        Assert.assertFalse(Files.exists(staging.get()));
    }

    @Test
    public void downloadsDefaultMethodCollisionBoundaryShouldBeExecutableDocumentation() throws Exception {
        String compatible = """
                import com.shaft.gui.driver.*;
                interface LegacyDownloads { default DownloadActionsContract downloads() { return null; } }
                interface CompatibleDownloadsFacade extends BrowserActionsContract, LegacyDownloads {
                    @Override default DownloadActionsContract downloads() {
                        return BrowserActionsContract.super.downloads();
                    }
                }
                """;
        String incompatible = """
                import com.shaft.gui.driver.*;
                interface LegacyDownloads { default String downloads() { return "legacy"; } }
                interface IncompatibleDownloadsFacade extends BrowserActionsContract, LegacyDownloads {}
                """;

        Assert.assertTrue(compiles("CompatibleDownloadsFacade", compatible));
        Assert.assertFalse(compiles("IncompatibleDownloadsFacade", incompatible));
    }

    @Test
    public void seleniumWaitShouldReturnTheFirstMatchingNewDownload() {
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("downloads"));
        Mockito.when(driver.isDownloadsEnabled()).thenReturn(true);
        var existing = new HasDownloads.DownloadedFile("existing.txt", 1_000, 1_000, 1);
        var first = new HasDownloads.DownloadedFile("first.pdf", 2_000, 2_000, 2);
        var second = new HasDownloads.DownloadedFile("second.pdf", 3_000, 3_000, 3);
        Mockito.when(driver.getDownloadedFiles()).thenReturn(List.of(existing), List.of(existing, first, second));

        BrowserDownload result = new com.shaft.gui.browser.BrowserActions(driver, true).downloads()
                .waitFor(file -> file.suggestedFilename().endsWith(".pdf"), () -> { });

        Assert.assertEquals(result.suggestedFilename(), "first.pdf");
    }

    @Test
    public void seleniumWaitShouldDetectAnIdenticalAdditionalMetadataOccurrence() {
        int originalTimeout = SHAFT.Properties.timeouts.browserNavigationTimeout();
        try {
            SHAFT.Properties.timeouts.set().browserNavigationTimeout(1);
            RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class);
            Mockito.when(driver.getSessionId()).thenReturn(new SessionId("downloads"));
            Mockito.when(driver.isDownloadsEnabled()).thenReturn(true);
            var baseline = new HasDownloads.DownloadedFile("same.txt", 1_000, 1_000, 1);
            var additional = new HasDownloads.DownloadedFile("same.txt", 1_000, 1_000, 1);
            Mockito.when(driver.getDownloadedFiles()).thenReturn(List.of(baseline), List.of(baseline, additional));

            BrowserDownload result = null;
            try {
                result = new com.shaft.gui.browser.BrowserActions(driver, true).downloads().waitFor(() -> { });
            } catch (org.openqa.selenium.TimeoutException missingMultisetBehavior) {
                // RED: set membership cannot distinguish a repeated identity occurrence.
            }
            Assert.assertNotNull(result);
            Assert.assertEquals(result.suggestedFilename(), "same.txt");
        } finally {
            SHAFT.Properties.timeouts.set().browserNavigationTimeout(originalTimeout);
        }
    }

    @Test
    public void playwrightSaveFailureShouldPreserveAnExistingTargetAndRemoveStaging() throws Exception {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Download nativeDownload = Mockito.mock(Download.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);
        Mockito.when(page.waitForDownload(Mockito.any(Page.WaitForDownloadOptions.class), Mockito.any(Runnable.class)))
                .thenReturn(nativeDownload);
        Path target = Files.createTempDirectory("shaft-playwright-safe-save").resolve("existing.pdf");
        Files.writeString(target, "known-good");
        AtomicReference<Path> providerTarget = new AtomicReference<>();
        IllegalStateException providerFailure = new IllegalStateException("stream failed");
        Mockito.doAnswer(invocation -> {
            Path path = invocation.getArgument(0);
            providerTarget.set(path);
            Files.writeString(path, "partial");
            throw providerFailure;
        }).when(nativeDownload).saveAs(Mockito.any(Path.class));
        BrowserDownload download = new com.shaft.gui.playwright.browser.BrowserActions(session)
                .downloads().waitFor(() -> { });

        RuntimeException thrown = Assert.expectThrows(RuntimeException.class, () -> download.saveAs(target));

        Assert.assertSame(thrown, providerFailure);
        Assert.assertEquals(Files.readString(target), "known-good");
        Assert.assertNotEquals(providerTarget.get(), target);
        Assert.assertFalse(Files.exists(providerTarget.get()));
    }

    @Test
    public void playwrightSaveShouldReplaceASymlinkWithoutFollowingIt() throws Exception {
        Path directory = Files.createTempDirectory("shaft-playwright-symlink-save");
        Path outside = directory.resolve("outside.txt");
        Path target = directory.resolve("target.txt");
        Files.writeString(outside, "outside");
        try {
            Files.createSymbolicLink(target, outside);
        } catch (IOException | UnsupportedOperationException unavailable) {
            throw new org.testng.SkipException("Symbolic links are unavailable on this host", unavailable);
        }
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Download nativeDownload = Mockito.mock(Download.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);
        Mockito.when(page.waitForDownload(Mockito.any(Page.WaitForDownloadOptions.class), Mockito.any(Runnable.class)))
                .thenReturn(nativeDownload);
        Mockito.doAnswer(invocation -> {
            Files.writeString(invocation.getArgument(0), "download");
            return null;
        }).when(nativeDownload).saveAs(Mockito.any(Path.class));
        BrowserDownload download = new com.shaft.gui.playwright.browser.BrowserActions(session)
                .downloads().waitFor(() -> { });

        download.saveAs(target);

        Assert.assertFalse(Files.isSymbolicLink(target));
        Assert.assertEquals(Files.readString(target), "download");
        Assert.assertEquals(Files.readString(outside), "outside");
    }

    private static Set<String> descriptors(Class<?> type) {
        return Arrays.stream(type.getDeclaredMethods())
                .map(method -> method.getName() + Arrays.toString(method.getParameterTypes())
                        + "->" + method.getReturnType().getSimpleName())
                .collect(Collectors.toSet());
    }

    private static boolean compiles(String name, String source) throws Exception {
        Path output = Files.createTempDirectory("shaft-downloads-api-compat");
        output.toFile().deleteOnExit();
        var compiler = ToolProvider.getSystemJavaCompiler();
        var sourceFile = new SimpleJavaFileObject(URI.create("string:///" + name + ".java"),
                javax.tools.JavaFileObject.Kind.SOURCE) {
            @Override public CharSequence getCharContent(boolean ignoreEncodingErrors) { return source; }
        };
        return Boolean.TRUE.equals(compiler.getTask(null, null, null,
                List.of("-classpath", System.getProperty("java.class.path"), "-d", output.toString()),
                null, List.of(sourceFile)).call());
    }
}
