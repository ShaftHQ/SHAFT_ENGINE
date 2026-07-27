package com.shaft.mcp;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.shaft.capture.generate.CaptureGenerationReport;
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
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
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
        McpMobileActionResult clickJs = service.clickUsingJavaScript(locatorStrategy.ID, "submit");
        McpMobileActionResult doubleClick = service.doubleClick(locatorStrategy.ID, "submit");
        McpMobileActionResult hover = service.hover(locatorStrategy.ID, "submit");
        McpMobileActionResult type = service.type(locatorStrategy.ID, "email", "alice@example.test");
        McpMobileActionResult append = service.appendText(locatorStrategy.ID, "notes", "more text");
        McpMobileActionResult setValueJs = service.setValueUsingJavaScript(locatorStrategy.ID, "hidden", "value");
        McpMobileActionResult clear = service.clear(locatorStrategy.ID, "email");
        McpMobileActionResult upload = service.uploadFile(locatorStrategy.ID, "upload", "/tmp/file.txt");
        McpMobileActionResult dragDrop = service.dragAndDrop(
                locatorStrategy.ID, "source", locatorStrategy.ID, "target");

        assertTrue(click.recorded());
        assertTrue(clickJs.recorded());
        assertTrue(doubleClick.recorded());
        assertTrue(hover.recorded());
        assertTrue(type.recorded());
        assertTrue(append.recorded());
        assertTrue(setValueJs.recorded());
        assertTrue(clear.recorded());
        assertTrue(upload.recorded());
        assertTrue(dragDrop.recorded());
        // The immediate tool response always echoes the value the caller just supplied (it isn't
        // new information to that caller); only the persisted recording redacts it.
        assertTrue(type.codeBlock().code().contains("alice@example.test"));
        assertTrue(type.warnings().stream().anyMatch(warning -> warning.contains("placeholder")));

        McpMobileReplayResult replayCode = service.recordingCodeBlocks(recordingPath.toString(), "driver");
        String generatedReplay = replayCode.codeBlocks().getFirst().code();
        assertTrue(generatedReplay.contains("<redacted>"));
        assertFalse(generatedReplay.contains("alice@example.test"));
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

        // Zero of the two recorded actions actually ran against a driver -- an unconfirmed/skipped
        // replay must not be reported as a bare success (#4230).
        assertEquals(0, replay.replayedActionCount());
        assertFalse(replay.successful());
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

    /**
     * Issue #4273: {@code McpPlaywrightCaptureAdapter} used to fabricate {@code uniquenessCount=0,
     * stable=false} for every manually-recorded locator (issue #4271's honest-refusal fix), so a
     * truly unique, stable, ID-strategy locator recorded through the live driver still could not
     * reach {@link com.shaft.capture.generate.LocatorPolicy}'s tier 1. Now {@link PlaywrightService}
     * computes real live-DOM evidence (a real element count plus the shared
     * {@code looksAutoGenerated} heuristic) at the moment the action executes, and that evidence
     * must survive all the way through the recording to deterministic codegen.
     */
    @Test
    void uniqueStableIdLocatorRecordedThroughTheLiveDriverGeneratesATierOneIdLocator() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        ElementActions element = mock(ElementActions.class);
        inject(service, mockDriver(mock(BrowserActions.class), element, mock(Page.class)));
        when(element.getElementsCount(any(org.openqa.selenium.By.class))).thenReturn(1);
        Path recording = temp.resolve("recordings/live-evidence-unique.json");
        service.recordStart(recording.toString(), "playwright", true);

        service.click(locatorStrategy.ID, "submit");
        service.recordStop(false);

        McpMobileReplayResult result = service.recordingCodeBlocks(recording.toString(), "driver");

        // codegen here never requests a replay (McpPlaywrightCaptureAdapter#request), so
        // CaptureGenerationReport.Status can only reach UNCONFIRMED, never SUCCESS -- successful()
        // (which requires SUCCESS) is always false on this path regardless of locator tier. What
        // proves the fix is that generation is no longer refused: no FAILED status, and a source file
        // that actually contains the tier-1 id locator.
        assertNotEquals(CaptureGenerationReport.Status.FAILED, result.report().status(), result.warnings().toString());
        assertTrue(Files.isRegularFile(result.sourcePath()));
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("SHAFT.GUI.Locator.hasAnyTagName().hasId(\"submit\").build()"), source);
    }

    /**
     * The other half of the same evidence path: a locator matching zero or multiple live elements
     * must still honestly refuse, exactly like issue #4271/#4265 intended -- real evidence, not a
     * second hardcoded shortcut.
     */
    @Test
    void nonUniqueIdLocatorRecordedThroughTheLiveDriverStillHonestlyRefuses() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        ElementActions element = mock(ElementActions.class);
        inject(service, mockDriver(mock(BrowserActions.class), element, mock(Page.class)));
        when(element.getElementsCount(any(org.openqa.selenium.By.class))).thenReturn(2);
        Path recording = temp.resolve("recordings/live-evidence-nonunique.json");
        service.recordStart(recording.toString(), "playwright", true);

        service.click(locatorStrategy.ID, "duplicate");

        McpMobileReplayResult result = service.recordingCodeBlocks(recording.toString(), "driver");

        assertEquals(CaptureGenerationReport.Status.FAILED, result.report().status());
        assertFalse(result.successful());
        assertFalse(Files.exists(result.sourcePath()));
        assertTrue(result.warnings().stream().anyMatch(
                        warning -> warning.contains("no unique stable id, no self-verified ARIA role")),
                result.warnings().toString());
    }

    /**
     * Issue #4262: NAME-strategy manual recordings used to hard-fail codegen because no
     * {@code replayXpath} was ever computed for them, so none of {@code LocatorPolicy}'s three
     * admission paths accepted the candidate. {@link PlaywrightService} now constructs a candidate
     * XPath from the recorded {@code name} attribute and independently re-verifies it live -- the
     * primary NAME locator (SHAFT's {@code hasAttribute("name", ...)} builder) and the constructed
     * {@code //*[@name="..."]} expression are two different queries, so both must confirm uniqueness
     * before {@code replayXpath} is trusted.
     */
    @Test
    void nameStrategyRecordedThroughTheLiveDriverComputesASelfVerifiedReplayXpath() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        ElementActions element = mock(ElementActions.class);
        inject(service, mockDriver(mock(BrowserActions.class), element, mock(Page.class)));
        when(element.getElementsCount(any(org.openqa.selenium.By.class))).thenReturn(1);
        Path recording = temp.resolve("recordings/name-unique.json");
        service.recordStart(recording.toString(), "playwright", true);

        service.click(locatorStrategy.NAME, "myField");
        service.recordStop(false);

        McpPlaywrightRecordingService fixture = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        McpMobileRecordedAction recorded = fixture.readRecording(recording.toString()).actions().getFirst();
        assertEquals("//*[@name=\"myField\"]", recorded.replayXpath());
    }

    /**
     * The independent re-query is the point of issue #4262's design: a NAME locator that is unique
     * through SHAFT's own builder but whose literally-constructed XPath equivalent resolves
     * differently live must not be trusted as self-verified -- assumed equivalence is exactly the
     * fabricated-evidence defect issue #4273 already fixed once for {@code stable}.
     */
    @Test
    void nameStrategyReplayXpathStaysBlankWhenTheIndependentXpathRequeryDisagreesWithTheLiveLocator() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        ElementActions element = mock(ElementActions.class);
        inject(service, mockDriver(mock(BrowserActions.class), element, mock(Page.class)));
        when(element.getElementsCount(any(org.openqa.selenium.By.class))).thenReturn(1);
        when(element.getElementsCount(eq(org.openqa.selenium.By.xpath("//*[@name=\"myField\"]")))).thenReturn(2);
        Path recording = temp.resolve("recordings/name-disagreement.json");
        service.recordStart(recording.toString(), "playwright", true);

        service.click(locatorStrategy.NAME, "myField");
        service.recordStop(false);

        McpPlaywrightRecordingService fixture = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        McpMobileRecordedAction recorded = fixture.readRecording(recording.toString()).actions().getFirst();
        assertEquals("", recorded.replayXpath());
    }

    /**
     * Issue #4262: an XPATH-strategy manual recording's {@code locatorValue} is already a literal
     * XPath expression -- when it resolves to exactly one live element, that expression IS its own
     * self-verified {@code replayXpath}; no separate construction step is needed (unlike NAME).
     */
    @Test
    void xpathStrategyRecordedThroughTheLiveDriverSetsReplayXpathToTheVerifiedExpression() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        ElementActions element = mock(ElementActions.class);
        inject(service, mockDriver(mock(BrowserActions.class), element, mock(Page.class)));
        when(element.getElementsCount(any(org.openqa.selenium.By.class))).thenReturn(1);
        Path recording = temp.resolve("recordings/xpath-unique.json");
        service.recordStart(recording.toString(), "playwright", true);

        service.click(locatorStrategy.XPATH, "//button[@data-testid='submit-btn']");
        service.recordStop(false);

        McpPlaywrightRecordingService fixture = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        McpMobileRecordedAction recorded = fixture.readRecording(recording.toString()).actions().getFirst();
        assertEquals("//button[@data-testid='submit-btn']", recorded.replayXpath());
    }

    /**
     * The honest-refusal half of the XPATH path: matching zero or multiple live elements must not be
     * trusted as self-verified, mirroring the existing ID-strategy non-unique test above.
     */
    @Test
    void xpathStrategyReplayXpathStaysBlankWhenNotUnique() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        ElementActions element = mock(ElementActions.class);
        inject(service, mockDriver(mock(BrowserActions.class), element, mock(Page.class)));
        when(element.getElementsCount(any(org.openqa.selenium.By.class))).thenReturn(2);
        Path recording = temp.resolve("recordings/xpath-nonunique.json");
        service.recordStart(recording.toString(), "playwright", true);

        service.click(locatorStrategy.XPATH, "//button[@class='btn']");
        service.recordStop(false);

        McpPlaywrightRecordingService fixture = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        McpMobileRecordedAction recorded = fixture.readRecording(recording.toString()).actions().getFirst();
        assertEquals("", recorded.replayXpath());
    }

    /**
     * End-to-end regression for issue #4262: a NAME-strategy manual recording now reaches
     * {@code LocatorPolicy}'s {@code Tier.VERIFIED_XPATH} through the real
     * {@link McpPlaywrightCaptureAdapter}/{@link com.shaft.capture.generate.CaptureGenerator} path --
     * generation is no longer refused, and the emitted source renders the self-verified XPath.
     */
    @Test
    void nameStrategyLocatorRecordedThroughTheLiveDriverGeneratesAVerifiedXpathLocator() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        ElementActions element = mock(ElementActions.class);
        inject(service, mockDriver(mock(BrowserActions.class), element, mock(Page.class)));
        when(element.getElementsCount(any(org.openqa.selenium.By.class))).thenReturn(1);
        Path recording = temp.resolve("recordings/name-codegen.json");
        service.recordStart(recording.toString(), "playwright", true);

        service.click(locatorStrategy.NAME, "myField");
        service.recordStop(false);

        McpMobileReplayResult result = service.recordingCodeBlocks(recording.toString(), "driver");

        assertNotEquals(CaptureGenerationReport.Status.FAILED, result.report().status(), result.warnings().toString());
        assertTrue(Files.isRegularFile(result.sourcePath()));
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("By.xpath(\"//*[@name=\\\"myField\\\"]\")"), source);
    }

    /**
     * Same end-to-end proof for XPATH strategy.
     */
    @Test
    void xpathStrategyLocatorRecordedThroughTheLiveDriverGeneratesAVerifiedXpathLocator() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        ElementActions element = mock(ElementActions.class);
        inject(service, mockDriver(mock(BrowserActions.class), element, mock(Page.class)));
        when(element.getElementsCount(any(org.openqa.selenium.By.class))).thenReturn(1);
        Path recording = temp.resolve("recordings/xpath-codegen.json");
        service.recordStart(recording.toString(), "playwright", true);

        service.click(locatorStrategy.XPATH, "//button[@data-testid='submit-btn']");
        service.recordStop(false);

        McpMobileReplayResult result = service.recordingCodeBlocks(recording.toString(), "driver");

        assertNotEquals(CaptureGenerationReport.Status.FAILED, result.report().status(), result.warnings().toString());
        assertTrue(Files.isRegularFile(result.sourcePath()));
        String source = Files.readString(result.sourcePath());
        assertTrue(source.contains("By.xpath(\"//button[@data-testid='submit-btn']\")"), source);
    }

    /**
     * Scope guard for issue #4262: CSS-strategy manual recordings are deliberately NOT fixed here (a
     * general CSS-to-XPath conversion is out of scope and tracked as a separate follow-up) -- this
     * proves the fix did not accidentally widen admission for CSS too.
     */
    @Test
    void cssStrategyLocatorRecordedThroughTheLiveDriverStillHonestlyRefuses() throws Exception {
        PlaywrightService service = new PlaywrightService(McpWorkspacePolicy.of(temp));
        ElementActions element = mock(ElementActions.class);
        inject(service, mockDriver(mock(BrowserActions.class), element, mock(Page.class)));
        when(element.getElementsCount(any(org.openqa.selenium.By.class))).thenReturn(1);
        Path recording = temp.resolve("recordings/css-still-refused.json");
        service.recordStart(recording.toString(), "playwright", true);

        service.click(locatorStrategy.CSS, "#submit-button");
        service.recordStop(false);

        McpMobileReplayResult result = service.recordingCodeBlocks(recording.toString(), "driver");

        assertEquals(CaptureGenerationReport.Status.FAILED, result.report().status());
        assertFalse(result.successful());
        assertFalse(Files.exists(result.sourcePath()));
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
