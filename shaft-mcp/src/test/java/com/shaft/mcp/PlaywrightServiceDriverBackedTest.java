package com.shaft.mcp;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.shaft.driver.SHAFT;
import com.shaft.gui.playwright.browser.BrowserActions;
import com.shaft.gui.playwright.element.ElementActions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.openqa.selenium.Cookie;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Exercises the driver-backed surface of {@link PlaywrightService} (the {@code /record} and
 * {@code /codegen} slash-command business logic) using a Mockito-mocked {@link SHAFT.GUI.Playwright}
 * injected through the package-private test seam, since the class never launches a real browser
 * in tests.
 */
class PlaywrightServiceDriverBackedTest {
    @TempDir
    Path temp;

    private static void inject(PlaywrightService service, SHAFT.GUI.Playwright driver) {
        service.setDriverForTesting(driver);
    }

    private static SHAFT.GUI.Playwright mockDriver(BrowserActions browser, ElementActions element, Page page) {
        SHAFT.GUI.Playwright driver = mock(SHAFT.GUI.Playwright.class);
        when(driver.browser()).thenReturn(browser);
        when(driver.element()).thenReturn(element);
        when(driver.getDriver()).thenReturn(page);
        return driver;
    }

    @Test
    void noDriverErrorsCoverRemainingPlaywrightNavigationScreenshotAndElementTools() {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));

        assertThrows(IllegalStateException.class, () -> service.navigate("https://example.test"));
        assertThrows(IllegalStateException.class, service::refresh);
        assertThrows(IllegalStateException.class, service::navigateBack);
        assertThrows(IllegalStateException.class, service::navigateForward);
        assertThrows(IllegalStateException.class, () -> service.setWindowSize(800, 600));
        assertThrows(IllegalStateException.class, () -> service.newWindow("https://example.test", "TAB"));
        assertThrows(IllegalStateException.class, service::getCurrentUrl);
        assertThrows(IllegalStateException.class, service::getTitle);
        assertThrows(IllegalStateException.class, () -> service.getPageDom(100));
        assertThrows(IllegalStateException.class, () -> service.takeScreenshot("shot.png", false));
        assertThrows(IllegalStateException.class,
                () -> service.clickUsingJavaScript(locatorStrategy.ID, "submit"));
        assertThrows(IllegalStateException.class, () -> service.doubleClick(locatorStrategy.ID, "submit"));
        assertThrows(IllegalStateException.class, () -> service.hover(locatorStrategy.ID, "submit"));
        assertThrows(IllegalStateException.class,
                () -> service.appendText(locatorStrategy.ID, "name", "value"));
        assertThrows(IllegalStateException.class,
                () -> service.setValueUsingJavaScript(locatorStrategy.ID, "name", "value"));
        assertThrows(IllegalStateException.class, () -> service.clear(locatorStrategy.ID, "name"));
        assertThrows(IllegalStateException.class,
                () -> service.uploadFile(locatorStrategy.ID, "upload", "file.txt"));
        assertThrows(IllegalStateException.class, () -> service.dragAndDrop(
                locatorStrategy.ID, "source", locatorStrategy.ID, "target"));
        assertThrows(IllegalStateException.class, () -> service.isDisplayed(locatorStrategy.ID, "submit"));
        assertThrows(IllegalStateException.class, () -> service.isEnabled(locatorStrategy.ID, "submit"));
    }

    @Test
    void quitClosesTheActiveDriverAndClearsTheSession() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        SHAFT.GUI.Playwright driver = mock(SHAFT.GUI.Playwright.class);
        inject(service, driver);

        service.quit();

        verify(driver, times(1)).quit();
        assertThrows(IllegalStateException.class, service::getCurrentUrl);
    }

    @Test
    void driverBackedNavigationAndWindowActionsInvokeBrowserActionsAndRecordCode() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        BrowserActions browser = mock(BrowserActions.class);
        ElementActions element = mock(ElementActions.class);
        Page page = mock(Page.class);
        inject(service, mockDriver(browser, element, page));
        when(browser.getCurrentURL()).thenReturn("https://example.test/home");
        when(browser.getCurrentWindowTitle()).thenReturn("Example Home");

        service.recordStart(temp.resolve("recordings/nav.json").toString(), "playwright", true);

        McpMobileActionResult navigate = service.navigate("https://example.test");
        McpMobileActionResult refresh = service.refresh();
        McpMobileActionResult back = service.navigateBack();
        McpMobileActionResult forward = service.navigateForward();
        McpMobileActionResult resize = service.setWindowSize(1024, 768);
        McpMobileActionResult tab = service.newWindow("https://example.test/tab", "TAB");
        McpMobileActionResult window = service.newWindow("", "WINDOW");
        String currentUrl = service.getCurrentUrl();
        String title = service.getTitle();

        verify(browser).navigateToURL("https://example.test");
        verify(browser).refreshCurrentPage();
        verify(browser).navigateBack();
        verify(browser).navigateForward();
        verify(browser).setWindowSize(1024, 768);
        verify(browser).openNewTab("https://example.test/tab");
        verify(browser).openNewWindow("about:blank");
        assertEquals("https://example.test/home", currentUrl);
        assertEquals("Example Home", title);
        assertTrue(navigate.recorded());
        assertTrue(refresh.recorded());
        assertTrue(back.recorded());
        assertTrue(forward.recorded());
        assertTrue(resize.recorded());
        assertTrue(tab.recorded());
        assertTrue(window.recorded());
        assertTrue(resize.codeBlock().code().contains("setWindowSize(1024, 768)"));
        assertTrue(tab.codeBlock().code().contains("openNewTab"));
        assertTrue(window.codeBlock().code().contains("openNewWindow"));
    }

    @Test
    void driverBackedPageDomTruncatesWhenOverLimitAndReturnsFullContentOtherwise() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        Page page = mock(Page.class);
        inject(service, mockDriver(mock(BrowserActions.class), mock(ElementActions.class), page));
        String longDom = "<html>" + "x".repeat(50) + "</html>";
        when(page.content()).thenReturn(longDom);
        when(page.url()).thenReturn("https://example.test");
        when(page.title()).thenReturn("Example");

        McpPageDomSnapshot truncated = service.getPageDom(10);
        McpPageDomSnapshot full = service.getPageDom(0);

        assertTrue(truncated.truncated());
        assertEquals(10, truncated.dom().length());
        assertFalse(truncated.warnings().isEmpty());
        assertFalse(full.truncated());
        assertEquals(longDom, full.dom());
        assertEquals("https://example.test", full.currentUrl());
        assertEquals("Example", full.title());
    }

    @Test
    void driverBackedScreenshotWritesFileAndHonorsIncludeBase64() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        Page page = mock(Page.class);
        inject(service, mockDriver(mock(BrowserActions.class), mock(ElementActions.class), page));
        byte[] png = {1, 2, 3, 4};
        when(page.screenshot(any(Page.ScreenshotOptions.class))).thenReturn(png);

        McpScreenshotResult withBase64 = service.takeScreenshot("shots/one.png", true);
        McpScreenshotResult withoutPathOrBase64 = service.takeScreenshot("", false);

        assertEquals(Base64.getEncoder().encodeToString(png), withBase64.base64());
        assertTrue(Files.isRegularFile(temp.resolve("shots/one.png")));
        assertEquals(png.length, withBase64.byteLength());
        assertEquals(null, withoutPathOrBase64.base64());
        assertFalse(withoutPathOrBase64.warnings().isEmpty());
    }

    @Test
    void driverBackedStorageStateSaveAndLoadDelegateToBrowserActions() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        BrowserActions browser = mock(BrowserActions.class);
        inject(service, mockDriver(browser, mock(ElementActions.class), mock(Page.class)));
        when(browser.getAllCookies()).thenReturn(Set.of(new Cookie("sid", "abc")));

        String savedPath = service.saveStorageState("state/storage.json");
        // saveStorageState is mocked and never actually writes the file, but loadStorageState
        // requires the source path to exist inside the workspace before it will resolve it.
        Files.createDirectories(Path.of(savedPath).getParent());
        Files.writeString(Path.of(savedPath), "{}");
        String loadMessage = service.loadStorageState("state/storage.json");

        verify(browser).saveStorageState(savedPath);
        verify(browser).loadStorageState(savedPath);
        assertTrue(savedPath.endsWith("storage.json"));
        assertTrue(loadMessage.contains("cookies restored: 1"));
    }

    @Test
    void driverBackedElementActionsRecordCodeAndRedactSensitiveValuesOnlyInThePersistedRecording() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        ElementActions element = mock(ElementActions.class);
        inject(service, mockDriver(mock(BrowserActions.class), element, mock(Page.class)));
        Path recordingPath = temp.resolve("recordings/elements.json");
        service.recordStart(recordingPath.toString(), "playwright", false);

        McpMobileActionResult click = service.click(locatorStrategy.ID, "submit");
        McpMobileActionResult clickSemantic = service.clickSemantic("Submit");
        McpMobileActionResult clickJs = service.clickUsingJavaScript(locatorStrategy.ID, "submit");
        McpMobileActionResult doubleClick = service.doubleClick(locatorStrategy.ID, "submit");
        McpMobileActionResult hover = service.hover(locatorStrategy.ID, "submit");
        McpMobileActionResult type = service.type(locatorStrategy.ID, "email", "alice@example.test");
        McpMobileActionResult typeSemantic = service.typeSemantic("Password", "super-secret");
        McpMobileActionResult append = service.appendText(locatorStrategy.ID, "notes", "more text");
        McpMobileActionResult setValueJs = service.setValueUsingJavaScript(locatorStrategy.ID, "hidden", "value");
        McpMobileActionResult clear = service.clear(locatorStrategy.ID, "email");
        McpMobileActionResult upload = service.uploadFile(locatorStrategy.ID, "upload", "/tmp/file.txt");
        McpMobileActionResult dragDrop = service.dragAndDrop(
                locatorStrategy.ID, "source", locatorStrategy.ID, "target");

        assertTrue(click.recorded());
        assertTrue(clickSemantic.recorded());
        assertTrue(clickJs.recorded());
        assertTrue(doubleClick.recorded());
        assertTrue(hover.recorded());
        assertTrue(type.recorded());
        assertTrue(typeSemantic.recorded());
        assertTrue(append.recorded());
        assertTrue(setValueJs.recorded());
        assertTrue(clear.recorded());
        assertTrue(upload.recorded());
        assertTrue(dragDrop.recorded());
        // The immediate tool response always echoes the value the caller just supplied (it isn't
        // new information to that caller); only the persisted recording redacts it.
        assertTrue(type.codeBlock().code().contains("alice@example.test"));
        assertTrue(type.warnings().stream().anyMatch(warning -> warning.contains("placeholder")));
        assertTrue(typeSemantic.warnings().stream().anyMatch(warning -> warning.contains("placeholder")));
        assertTrue(clickSemantic.codeBlock().code().contains("clickableField(\"Submit\")"));

        McpMobileReplayResult replayCode = service.recordingCodeBlocks(recordingPath.toString(), "driver");
        String generatedReplay = replayCode.codeBlocks().getFirst().code();
        assertTrue(generatedReplay.contains("<redacted>"));
        assertFalse(generatedReplay.contains("alice@example.test"));
        assertFalse(generatedReplay.contains("super-secret"));
    }

    @Test
    void driverBackedIsDisplayedAndIsEnabledResolvePlaywrightLocator() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        Page page = mock(Page.class);
        Locator locator = mock(Locator.class);
        when(page.locator(anyString())).thenReturn(locator);
        when(locator.isVisible()).thenReturn(true);
        when(locator.isEnabled()).thenReturn(false);
        inject(service, mockDriver(mock(BrowserActions.class), mock(ElementActions.class), page));

        assertTrue(service.isDisplayed(locatorStrategy.ID, "submit"));
        assertFalse(service.isEnabled(locatorStrategy.ID, "submit"));
    }

    @Test
    void replayRecordingSilentlySkipsRedactedActionsWithoutRequiringAnActiveDriver() {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        McpPlaywrightRecordingService fixture = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/redacted-only.json");
        fixture.start(recording.toString(), "playwright", false);
        fixture.record("type", locatorStrategy.ID, "email", Map.of("value", "alice@example.test"),
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"alice@example.test\");",
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"<redacted>\");",
                true);
        fixture.record("type_semantic", null, "Password", Map.of("value", "super-secret"),
                PlaywrightService.semanticTypeCode("Password", "super-secret"),
                PlaywrightService.semanticTypeCode("Password", "<redacted>"),
                true);
        fixture.stop(false);

        McpMobileReplayResult replay = service.replayRecording(recording.toString(), "driver");

        assertEquals(0, replay.replayedActionCount());
        assertTrue(replay.successful());
    }

    @Test
    void replayRecordingFailsWithoutActiveDriverWhenActionIsNotRedacted() {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        McpPlaywrightRecordingService fixture = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/navigate-only.json");
        fixture.start(recording.toString(), "playwright", true);
        fixture.record("navigate", null, "", Map.of("url", "https://example.test"),
                "driver.browser().navigateToURL(\"https://example.test\");",
                "driver.browser().navigateToURL(\"https://example.test\");",
                false);
        fixture.stop(false);

        assertThrows(IllegalStateException.class,
                () -> service.replayRecording(recording.toString(), "driver"));
    }

    @Test
    void replayRecordingRejectsUnsupportedActionWithoutRequiringADriver() {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        McpPlaywrightRecordingService fixture = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/unsupported.json");
        fixture.start(recording.toString(), "playwright", true);
        fixture.record("teleport", null, "", Map.of(),
                "driver.teleport();", "driver.teleport();", false);
        fixture.stop(false);

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service.replayRecording(recording.toString(), "driver"));

        assertTrue(failure.getMessage().contains("Unsupported Playwright recording action"));
    }

    @Test
    void replayRecordingExecutesEachSupportedActionAgainstAMockedDriver() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        BrowserActions browser = mock(BrowserActions.class);
        ElementActions element = mock(ElementActions.class);
        inject(service, mockDriver(browser, element, mock(Page.class)));

        McpPlaywrightRecordingService fixture = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/full-journey.json");
        fixture.start(recording.toString(), "playwright", true);
        fixture.record("navigate", null, "", Map.of("url", "https://example.test"),
                "driver.browser().navigateToURL(\"https://example.test\");", "…", false);
        fixture.record("refresh", null, "", Map.of(),
                "driver.browser().refreshCurrentPage();", "…", false);
        fixture.record("navigate_back", null, "", Map.of(),
                "driver.browser().navigateBack();", "…", false);
        fixture.record("navigate_forward", null, "", Map.of(),
                "driver.browser().navigateForward();", "…", false);
        fixture.record("set_window_size", null, "", Map.of("width", "800", "height", "600"),
                "driver.browser().setWindowSize(800, 600);", "…", false);
        fixture.record("new_window", null, "", Map.of("url", "https://example.test", "windowType", "WINDOW"),
                "driver.browser().openNewWindow(\"https://example.test\");", "…", false);
        fixture.record("new_window", null, "", Map.of("url", "https://example.test", "windowType", "TAB"),
                "driver.browser().openNewTab(\"https://example.test\");", "…", false);
        fixture.record("click", locatorStrategy.ID, "submit", Map.of(),
                "driver.element().click(SHAFT.GUI.Locator.id(\"submit\"));", "…", false);
        fixture.record("click_js", locatorStrategy.ID, "submit", Map.of(),
                "driver.element().clickUsingJavascript(SHAFT.GUI.Locator.id(\"submit\"));", "…", false);
        fixture.record("double_click", locatorStrategy.ID, "submit", Map.of(),
                "driver.element().doubleClick(SHAFT.GUI.Locator.id(\"submit\"));", "…", false);
        fixture.record("hover", locatorStrategy.ID, "submit", Map.of(),
                "driver.element().hover(SHAFT.GUI.Locator.id(\"submit\"));", "…", false);
        fixture.record("type", locatorStrategy.ID, "email", Map.of("value", "alice@example.test"),
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"alice@example.test\");", "…", false);
        fixture.record("click_semantic", null, "Submit", Map.of("elementName", "Submit"),
                PlaywrightService.semanticTypeCode("Submit", ""), "…", false);
        fixture.record("type_semantic", null, "Password", Map.of("value", "super-secret"),
                PlaywrightService.semanticTypeCode("Password", "super-secret"), "…", false);
        fixture.record("append_text", locatorStrategy.ID, "notes", Map.of("value", "more"),
                "driver.element().typeAppend(SHAFT.GUI.Locator.id(\"notes\"), \"more\");", "…", false);
        fixture.record("set_value_js", locatorStrategy.ID, "hidden", Map.of("value", "value"),
                "driver.element().setValueUsingJavaScript(SHAFT.GUI.Locator.id(\"hidden\"), \"value\");", "…", false);
        fixture.record("clear", locatorStrategy.ID, "email", Map.of(),
                "driver.element().clear(SHAFT.GUI.Locator.id(\"email\"));", "…", false);
        fixture.record("upload_file", locatorStrategy.ID, "upload", Map.of("value", "/tmp/file.txt"),
                "driver.element().typeFileLocationForUpload(SHAFT.GUI.Locator.id(\"upload\"), \"/tmp/file.txt\");",
                "…", false);
        fixture.record("drag_and_drop", locatorStrategy.ID, "source",
                Map.of("targetStrategy", "ID", "targetValue", "target"),
                "driver.element().dragAndDrop(SHAFT.GUI.Locator.id(\"source\"), SHAFT.GUI.Locator.id(\"target\"));",
                "…", false);
        fixture.stop(false);

        McpMobileReplayResult replay = service.replayRecording(recording.toString(), "driver");

        assertEquals(19, replay.replayedActionCount());
        verify(browser).navigateToURL("https://example.test");
        verify(browser).refreshCurrentPage();
        verify(browser).navigateBack();
        verify(browser).navigateForward();
        verify(browser).setWindowSize(800, 600);
        verify(browser).openNewWindow("https://example.test");
        verify(browser).openNewTab("https://example.test");
        verify(element, times(2)).click(any(org.openqa.selenium.By.class));
        verify(element).clickUsingJavascript(any(org.openqa.selenium.By.class));
        verify(element).doubleClick(any(org.openqa.selenium.By.class));
        verify(element).hover(any(org.openqa.selenium.By.class));
        verify(element, times(2)).type(any(org.openqa.selenium.By.class), anyString());
        verify(element).typeAppend(any(org.openqa.selenium.By.class), anyString());
        verify(element).setValueUsingJavaScript(any(org.openqa.selenium.By.class), anyString());
        verify(element).clear(any(org.openqa.selenium.By.class));
        verify(element).typeFileLocationForUpload(any(org.openqa.selenium.By.class), anyString());
        verify(element).dragAndDrop(any(org.openqa.selenium.By.class), any(org.openqa.selenium.By.class));
    }
}
