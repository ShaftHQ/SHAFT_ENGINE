package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Download;
import com.microsoft.playwright.Page;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import org.mockito.Mockito;
import org.openqa.selenium.HasDownloads;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.remote.SessionId;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.nio.file.Files;
import java.nio.file.Path;

public class BrowserDownloadsNamespaceTraceTest {
    @AfterMethod
    public void clearTrace() {
        TraceEventRecorder.clear();
    }

    @Test
    public void seleniumDownloadInventoryShouldEmitOneBackendOwnedEvent() {
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("downloads"));
        Mockito.when(driver.isDownloadsEnabled()).thenReturn(true);
        Mockito.when(driver.getDownloadedFiles()).thenReturn(List.of(
                new HasDownloads.DownloadedFile("report.pdf", 1, 2, 3)));

        new com.shaft.gui.browser.BrowserActions(driver, true).downloads().all();

        assertSingleEvent("all", "passed", AutomationBackend.SELENIUM_WEBDRIVER);
    }

    @Test
    public void unsupportedDownloadInventoryShouldEmitOneFailedEvent() {
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("downloads"));
        Mockito.when(driver.isDownloadsEnabled()).thenReturn(false);

        Assert.expectThrows(UnsupportedOperationException.class,
                () -> new com.shaft.gui.browser.BrowserActions(driver, true).downloads().all());

        assertSingleEvent("all", "failed", AutomationBackend.SELENIUM_WEBDRIVER);
    }

    @Test
    public void playwrightDownloadWaitShouldEmitOneBackendOwnedEvent() {
        PlaywrightSession session = Mockito.mock(PlaywrightSession.class);
        Browser browser = Mockito.mock(Browser.class);
        BrowserContext context = Mockito.mock(BrowserContext.class);
        Page page = Mockito.mock(Page.class);
        Download download = Mockito.mock(Download.class);
        Mockito.when(session.browser()).thenReturn(browser);
        Mockito.when(session.browserContext()).thenReturn(context);
        Mockito.when(session.page()).thenReturn(page);
        Mockito.when(browser.isConnected()).thenReturn(true);
        Mockito.when(page.isClosed()).thenReturn(false);
        Mockito.when(page.waitForDownload(Mockito.any(Page.WaitForDownloadOptions.class), Mockito.any(Runnable.class)))
                .thenReturn(download);

        new com.shaft.gui.playwright.browser.BrowserActions(session).downloads().waitFor(() -> { });

        assertSingleEvent("wait-for", "passed", AutomationBackend.MICROSOFT_PLAYWRIGHT);
    }

    @Test
    public void seleniumDownloadArtifactOperationsShouldEachOwnOneTraceEvent() throws Exception {
        RemoteWebDriver driver = Mockito.mock(RemoteWebDriver.class);
        Mockito.when(driver.getSessionId()).thenReturn(new SessionId("downloads"));
        Mockito.when(driver.isDownloadsEnabled()).thenReturn(true);
        Mockito.when(driver.getDownloadedFiles()).thenReturn(List.of(
                new HasDownloads.DownloadedFile("report.pdf", 1, 2, 3)));
        Mockito.doAnswer(invocation -> {
            Path directory = invocation.getArgument(1);
            Files.writeString(directory.resolve("report.pdf"), "payload");
            return null;
        }).when(driver).downloadFile(Mockito.eq("report.pdf"), Mockito.any(Path.class));
        var download = new com.shaft.gui.browser.BrowserActions(driver, true).downloads().latest();
        TraceEventRecorder.clear();

        download.saveAs(Files.createTempDirectory("shaft-download-trace").resolve("saved.pdf"));
        assertSingleEvent("save-as", "passed", AutomationBackend.SELENIUM_WEBDRIVER);
        TraceEventRecorder.clear();
        Assert.expectThrows(UnsupportedOperationException.class, download::cancel);
        assertSingleEvent("cancel", "failed", AutomationBackend.SELENIUM_WEBDRIVER);
        TraceEventRecorder.clear();
        Assert.expectThrows(UnsupportedOperationException.class, download::delete);
        assertSingleEvent("delete", "failed", AutomationBackend.SELENIUM_WEBDRIVER);
    }

    @Test
    public void playwrightDownloadArtifactOperationsShouldEachOwnOneTraceEvent() throws Exception {
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
        var download = new com.shaft.gui.playwright.browser.BrowserActions(session).downloads().waitFor(() -> { });
        TraceEventRecorder.clear();

        download.saveAs(Files.createTempDirectory("shaft-download-trace").resolve("saved.pdf"));
        assertSingleEvent("save-as", "passed", AutomationBackend.MICROSOFT_PLAYWRIGHT);
        TraceEventRecorder.clear();
        download.cancel();
        assertSingleEvent("cancel", "passed", AutomationBackend.MICROSOFT_PLAYWRIGHT);
        TraceEventRecorder.clear();
        download.delete();
        assertSingleEvent("delete", "passed", AutomationBackend.MICROSOFT_PLAYWRIGHT);
    }

    private static void assertSingleEvent(String name, String status, AutomationBackend backend) {
        var events = TraceEventRecorder.snapshot();
        Assert.assertEquals(events.size(), 1);
        Assert.assertEquals(events.getFirst().category(), "downloads");
        Assert.assertEquals(events.getFirst().name(), name);
        Assert.assertEquals(events.getFirst().status(), status);
        Assert.assertEquals(events.getFirst().backend(), backend);
    }
}
