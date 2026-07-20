package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import com.shaft.validation.accessibility.AccessibilityActions;
import com.shaft.validation.accessibility.AccessibilityHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Per-engine dispatch matrix for the unified {@code browser_*} tools (design doc amendment A2):
 * asserts each tool reaches the correct underlying implementation for web and Playwright. Mobile
 * (MOBILE_NATIVE/MOBILE_WEB) is not covered by a separate matrix here because it shares the same
 * WebDriver-backed {@code EngineService.getDriver()} field as WEB (ground truth, design doc) --
 * every verb below already resolves through the identical code path for WEB and mobile, so the web
 * case IS the mobile case for these tools.
 */
class BrowserServiceDispatchTest {
    @TempDir
    Path temp;

    @AfterEach
    void resetActiveEngine() {
        EngineService.setActiveEngine(null);
        BrowserObservabilityRecorder.clear();
    }

    @Test
    void navigateDispatchesToWebOrPlaywrightBrowserActionsAndOpensNewWindowWhenRequested() {
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService webService = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);
        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            webService.navigate("https://example.test");
            webService.navigate("https://example.test/new", true);

            verify(webBrowser).navigateToURL("https://example.test");
            verify(webBrowser).openNewWindow("https://example.test/new");
        }

        com.shaft.gui.playwright.browser.BrowserActions pwBrowser = mock(com.shaft.gui.playwright.browser.BrowserActions.class);
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        when(playwrightService.browserActions()).thenReturn(pwBrowser);
        BrowserService pwService = new BrowserService(McpWorkspacePolicy.of(temp), playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        pwService.navigate("https://example.test");
        pwService.navigate("https://example.test/new", true);

        verify(pwBrowser).navigateToURL("https://example.test");
        verify(pwBrowser).openNewWindow("https://example.test/new");
    }

    @Test
    void refreshBackAndForwardDispatchToWebOrPlaywrightBrowserActions() {
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService webService = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);
        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            webService.refreshPage();
            webService.navigateBack();
            webService.navigateForward();

            verify(webBrowser).refreshCurrentPage();
            verify(webBrowser).navigateBack();
            verify(webBrowser).navigateForward();
        }

        com.shaft.gui.playwright.browser.BrowserActions pwBrowser = mock(com.shaft.gui.playwright.browser.BrowserActions.class);
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        when(playwrightService.browserActions()).thenReturn(pwBrowser);
        BrowserService pwService = new BrowserService(McpWorkspacePolicy.of(temp), playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        pwService.refreshPage();
        pwService.navigateBack();
        pwService.navigateForward();

        verify(pwBrowser).refreshCurrentPage();
        verify(pwBrowser).navigateBack();
        verify(pwBrowser).navigateForward();
    }

    @Test
    void setWindowSizeModeDispatchesCustomMaximizeAndFullscreenOnWebAndPlaywright() {
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService webService = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);
        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            webService.setWindowSize(800, 600);
            webService.setWindowSize(0, 0, WindowSizeMode.MAXIMIZE);
            webService.setWindowSize(0, 0, WindowSizeMode.FULLSCREEN);

            verify(webBrowser).setWindowSize(800, 600);
            verify(webBrowser).maximizeWindow();
            verify(webBrowser).fullScreenWindow();
        }

        com.shaft.gui.playwright.browser.BrowserActions pwBrowser = mock(com.shaft.gui.playwright.browser.BrowserActions.class);
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        when(playwrightService.browserActions()).thenReturn(pwBrowser);
        BrowserService pwService = new BrowserService(McpWorkspacePolicy.of(temp), playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        pwService.setWindowSize(0, 0, WindowSizeMode.MAXIMIZE);

        verify(pwBrowser).maximizeWindow();
    }

    @Test
    void deleteCookieAndDeleteAllCookiesDelegateToUnifiedLogicAndDispatchToWebAndPlaywright() {
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService webService = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);
        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            webService.deleteCookie("sid");
            webService.deleteAllCookies();

            verify(webBrowser).deleteCookie("sid");
            verify(webBrowser).deleteAllCookies();
        }

        com.shaft.gui.playwright.browser.BrowserActions pwBrowser = mock(com.shaft.gui.playwright.browser.BrowserActions.class);
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        when(playwrightService.browserActions()).thenReturn(pwBrowser);
        BrowserService pwService = new BrowserService(McpWorkspacePolicy.of(temp), playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        pwService.deleteCookie("sid");
        pwService.deleteAllCookies();

        verify(pwBrowser).deleteCookie("sid");
        verify(pwBrowser).deleteAllCookies();
    }

    @Test
    void getCurrentUrlAndGetTitleDispatchToWebAndPlaywrightBrowserActions() {
        BrowserActions webBrowser = mock(BrowserActions.class);
        when(webBrowser.getCurrentURL()).thenReturn("https://example.test/web");
        when(webBrowser.getCurrentWindowTitle()).thenReturn("Web Title");
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService webService = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);
        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            assertEquals("https://example.test/web", webService.getCurrentUrl());
            assertEquals("Web Title", webService.getTitle());
        }

        com.shaft.gui.playwright.browser.BrowserActions pwBrowser = mock(com.shaft.gui.playwright.browser.BrowserActions.class);
        when(pwBrowser.getCurrentURL()).thenReturn("https://example.test/pw");
        when(pwBrowser.getCurrentWindowTitle()).thenReturn("PW Title");
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        when(playwrightService.browserActions()).thenReturn(pwBrowser);
        BrowserService pwService = new BrowserService(McpWorkspacePolicy.of(temp), playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        assertEquals("https://example.test/pw", pwService.getCurrentUrl());
        assertEquals("PW Title", pwService.getTitle());
    }

    @Test
    void openIntentNavigatesAndOrientsUsingTheActiveEngineBrowserActions() {
        com.shaft.gui.playwright.browser.BrowserActions pwBrowser = mock(com.shaft.gui.playwright.browser.BrowserActions.class);
        when(pwBrowser.getCurrentURL()).thenReturn("https://example.test/pw");
        when(pwBrowser.getCurrentWindowTitle()).thenReturn("PW Title");
        when(pwBrowser.getPageSource()).thenReturn("<html><body><button id=\"go\">Go</button></body></html>");
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        when(playwrightService.browserActions()).thenReturn(pwBrowser);
        BrowserService pwService = new BrowserService(McpWorkspacePolicy.of(temp), playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        Map<String, Object> result = pwService.openForIntent("https://example.test/pw", "click go", 0, 0);

        verify(pwBrowser).navigateToURL("https://example.test/pw");
        assertEquals("https://example.test/pw", result.get("currentUrl"));
    }

    @Test
    void getPageDomDispatchesToPlaywrightServiceOnPlaywrightEngine() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        McpPageDomSnapshot snapshot = new McpPageDomSnapshot("https://x", "t", "<html/>", 6, false, java.util.List.of());
        when(playwrightService.getPageDom(anyInt())).thenReturn(snapshot);
        BrowserService pwService = new BrowserService(McpWorkspacePolicy.of(temp), playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        McpPageDomSnapshot result = pwService.getPageDom(1000);

        verify(playwrightService).getPageDom(1000);
        assertEquals(snapshot, result);
    }

    @Test
    void takeScreenshotDispatchesToPlaywrightServiceOnPlaywrightEngine() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        McpScreenshotResult screenshot = new McpScreenshotResult("image/png", 1, null, null, java.util.List.of());
        when(playwrightService.takeScreenshot(anyString(), anyBoolean())).thenReturn(screenshot);
        BrowserService pwService = new BrowserService(McpWorkspacePolicy.of(temp), playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        McpScreenshotResult result = pwService.takeScreenshot("shot.png", true);

        verify(playwrightService).takeScreenshot("shot.png", true);
        assertEquals(screenshot, result);
    }

    @Test
    void storageStateSaveAndLoadDispatchToPlaywrightServiceOnPlaywrightEngine() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        when(playwrightService.saveStorageState(anyString())).thenReturn("/state.json");
        when(playwrightService.loadStorageState(anyString())).thenReturn("Loaded");
        BrowserService pwService = new BrowserService(McpWorkspacePolicy.of(temp), playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        assertEquals("/state.json", pwService.saveStorageState("state.json"));
        assertEquals("Loaded", pwService.loadStorageState("state.json"));

        verify(playwrightService).saveStorageState("state.json");
        verify(playwrightService).loadStorageState("state.json");
    }

    @Test
    void ariaSnapshotDispatchesToPlaywrightServiceOnPlaywrightEngine() {
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        when(playwrightService.ariaSnapshot(locatorStrategy.ID, "content")).thenReturn("- generic");
        BrowserService pwService = new BrowserService(McpWorkspacePolicy.of(temp), playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        assertEquals("- generic", pwService.ariaSnapshot(locatorStrategy.ID, "content"));

        verify(playwrightService).ariaSnapshot(locatorStrategy.ID, "content");
    }

    @Test
    void routeAndUnrouteDispatchToWebAndPlaywrightBrowserActions() {
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService webService = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);
        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            webService.route("GET", "**/api/**", null, 200, "{}", null);
            webService.unroute(null);

            verify(webBrowser).mock(any(), any());
            verify(webBrowser).clearNetworkInterceptors();
        }

        com.shaft.gui.playwright.browser.BrowserActions pwBrowser = mock(com.shaft.gui.playwright.browser.BrowserActions.class);
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        when(playwrightService.browserActions()).thenReturn(pwBrowser);
        BrowserService pwService = new BrowserService(McpWorkspacePolicy.of(temp), playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        pwService.route("GET", "**/api/**", null, 200, "{}", null);
        pwService.unroute(null);

        verify(pwBrowser).mock(any(), any());
        verify(pwBrowser).clearNetworkInterceptors();
    }

    @Test
    void accessibilityAuditDispatchesToWebAndPlaywrightBrowserActions() {
        AccessibilityActions webAccessibility = mock(AccessibilityActions.class);
        AccessibilityHelper.AccessibilityResult webResult = mock(AccessibilityHelper.AccessibilityResult.class);
        when(webResult.getViolations()).thenReturn(new java.util.ArrayList<>());
        when(webAccessibility.analyzeAndReturn(anyString(), any(), eq(false))).thenReturn(webResult);
        BrowserActions webBrowser = mock(BrowserActions.class);
        when(webBrowser.accessibility()).thenReturn(webAccessibility);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService webService = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);
        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            McpAccessibilityAuditResult result = webService.accessibilityAudit();

            assertEquals(0, result.violationsCount());
            verify(webAccessibility).analyzeAndReturn(anyString(), any(), eq(false));
        }

        AccessibilityActions pwAccessibility = mock(AccessibilityActions.class);
        AccessibilityHelper.AccessibilityResult pwResult = mock(AccessibilityHelper.AccessibilityResult.class);
        when(pwResult.getViolations()).thenReturn(new java.util.ArrayList<>());
        when(pwAccessibility.analyzeAndReturn(anyString(), any(), eq(false))).thenReturn(pwResult);
        com.shaft.gui.playwright.browser.BrowserActions pwBrowser = mock(com.shaft.gui.playwright.browser.BrowserActions.class);
        when(pwBrowser.accessibility()).thenReturn(pwAccessibility);
        PlaywrightService playwrightService = mock(PlaywrightService.class);
        when(playwrightService.browserActions()).thenReturn(pwBrowser);
        BrowserService pwService = new BrowserService(McpWorkspacePolicy.of(temp), playwrightService);
        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);

        McpAccessibilityAuditResult pwAuditResult = pwService.accessibilityAudit();

        assertEquals(0, pwAuditResult.violationsCount());
        verify(pwAccessibility).analyzeAndReturn(anyString(), any(), eq(false));
    }

    @Test
    void networkRequestsThrowsActionableErrorOnPlaywrightAndNarrowsByIdOnWeb() {
        BrowserObservabilityRecorder.clear();
        HttpResponse response = new HttpResponse().setStatus(200);
        var exchangeA = BrowserObservabilityRecorder.startNetwork(new HttpRequest(HttpMethod.GET, "https://example.test/api/a"));
        BrowserObservabilityRecorder.finishNetwork(exchangeA, response, "");
        var exchangeB = BrowserObservabilityRecorder.startNetwork(new HttpRequest(HttpMethod.GET, "https://example.test/api/b"));
        BrowserObservabilityRecorder.finishNetwork(exchangeB, response, "");

        BrowserService webService = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);
        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(mock(SHAFT.GUI.WebDriver.class));

            McpNetworkTransactionList all = webService.networkRequests("", 50);
            assertTrue(all.transactions().size() >= 2);

            int firstId = all.transactions().get(0).id();
            McpNetworkTransactionList narrowed = webService.networkRequests("", 50, firstId);
            assertEquals(1, narrowed.transactions().size());
            assertEquals(firstId, narrowed.transactions().get(0).id());
        }

        EngineService.setActiveEngine(ActiveEngine.PLAYWRIGHT);
        assertThrows(UnsupportedOperationException.class, () -> webService.networkRequests("", 50));
    }
}
