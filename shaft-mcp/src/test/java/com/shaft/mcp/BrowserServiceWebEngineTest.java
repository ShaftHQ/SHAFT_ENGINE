package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.Cookie;
import org.openqa.selenium.OutputType;

import java.nio.file.Path;
import java.nio.file.Files;
import java.util.Base64;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

/**
 * Tests for WEB engine-specific paths and utility methods in BrowserService that are not
 * covered by the dispatch matrix tests (which focus on web vs. Playwright engine selection).
 * These tests verify the actual WEB engine implementations and helper methods without
 * requiring a real browser.
 */
class BrowserServiceWebEngineTest {
    @TempDir
    Path temp;

    @AfterEach
    void resetActiveEngine() {
        EngineService.setActiveEngine(null);
    }

    @Test
    void navigateWithBasicAuthCallsWebDriverBrowserActions() {
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            service.navigateWithBasicAuth("https://example.test", "user", "pass", "https://example.test/after");

            verify(webBrowser).navigateToURLWithBasicAuthentication("https://example.test", "user", "pass",
                    "https://example.test/after");
        }
    }

    @Test
    void addCookieCallsWebDriverBrowserActions() {
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            service.addCookie("sid", "session123");

            verify(webBrowser).addCookie("sid", "session123");
        }
    }

    @Test
    void getCookieReturnsValueFromWebDriver() {
        Cookie mockCookie = mock(Cookie.class);
        when(mockCookie.getValue()).thenReturn("session123");
        BrowserActions webBrowser = mock(BrowserActions.class);
        when(webBrowser.getCookie("sid")).thenReturn(mockCookie);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            String cookieValue = service.getCookie("sid");

            assertEquals("session123", cookieValue);
        }
    }

    @Test
    void getAllCookiesReturnsStringRepresentation() {
        BrowserActions webBrowser = mock(BrowserActions.class);
        Set<Cookie> cookies = Set.of(mock(Cookie.class), mock(Cookie.class));
        when(webBrowser.getAllCookies()).thenReturn(cookies);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            String allCookies = service.getAllCookies();

            assertNotNull(allCookies);
            assertTrue(allCookies.length() > 0);
        }
    }

    @Test
    void getPageDomOnWebEngineReturnsPageSourceSnapshot() {
        WebDriver seleniumDriver = mock(WebDriver.class);
        when(seleniumDriver.getPageSource()).thenReturn("<html><body><p>Test</p></body></html>");
        when(seleniumDriver.getCurrentUrl()).thenReturn("https://example.test");
        when(seleniumDriver.getTitle()).thenReturn("Test Page");
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(seleniumDriver);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            McpPageDomSnapshot snapshot = service.getPageDom(1000);

            assertEquals("https://example.test", snapshot.currentUrl());
            assertEquals("Test Page", snapshot.title());
            assertTrue(snapshot.dom().contains("Test"));
            assertFalse(snapshot.truncated());
        }
    }

    @Test
    void getPageDomOnWebEngineTruncatesDomWhenExceedsLimit() {
        String largeDom = "<html><body>" + "x".repeat(200) + "</body></html>";
        WebDriver seleniumDriver = mock(WebDriver.class);
        when(seleniumDriver.getPageSource()).thenReturn(largeDom);
        when(seleniumDriver.getCurrentUrl()).thenReturn("https://example.test");
        when(seleniumDriver.getTitle()).thenReturn("Test Page");
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(seleniumDriver);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            McpPageDomSnapshot snapshot = service.getPageDom(100);

            assertTrue(snapshot.truncated(), "DOM should be truncated when exceeding limit");
            assertEquals(100, snapshot.dom().length());
            assertTrue(snapshot.characterCount() > 100, "characterCount should reflect original DOM size");
            assertFalse(snapshot.warnings().isEmpty(), "warnings should not be empty when truncated");
            assertTrue(snapshot.warnings().stream().anyMatch(w -> w.contains("DOM was truncated")));
        }
    }

    @Test
    void getPageDomOnWebEngineHandlesNullPageSource() {
        WebDriver seleniumDriver = mock(WebDriver.class);
        when(seleniumDriver.getPageSource()).thenReturn(null);
        when(seleniumDriver.getCurrentUrl()).thenReturn("https://example.test");
        when(seleniumDriver.getTitle()).thenReturn("Test Page");
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(seleniumDriver);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            McpPageDomSnapshot snapshot = service.getPageDom(1000);

            assertEquals("", snapshot.dom());
            assertEquals(0, snapshot.characterCount());
            assertFalse(snapshot.truncated());
        }
    }

    @Test
    void takeScreenshotOnWebEngineReturnsPngBytesAndPath() throws Exception {
        byte[] pngBytes = new byte[]{(byte) 0x89, (byte) 0x50, (byte) 0x4E, (byte) 0x47};
        WebDriver seleniumDriver = mock(WebDriver.class, withSettings().extraInterfaces(TakesScreenshot.class));
        TakesScreenshot takesScreenshot = (TakesScreenshot) seleniumDriver;
        when(takesScreenshot.getScreenshotAs(OutputType.BYTES)).thenReturn(pngBytes);
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(seleniumDriver);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            McpScreenshotResult result = service.takeScreenshot("screenshot.png", true);

            assertEquals("image/png", result.mediaType());
            assertEquals(4, result.byteLength());
            assertNotNull(result.base64(), "base64 should be included when includeBase64=true");
            assertTrue(result.warnings().isEmpty(), "warnings should be empty when includeBase64=true");
        }
    }

    @Test
    void takeScreenshotOnWebEngineOmitsBase64WhenNotRequested() throws Exception {
        byte[] pngBytes = new byte[]{(byte) 0x89, (byte) 0x50, (byte) 0x4E, (byte) 0x47};
        WebDriver seleniumDriver = mock(WebDriver.class, withSettings().extraInterfaces(TakesScreenshot.class));
        TakesScreenshot takesScreenshot = (TakesScreenshot) seleniumDriver;
        when(takesScreenshot.getScreenshotAs(OutputType.BYTES)).thenReturn(pngBytes);
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(seleniumDriver);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            McpScreenshotResult result = service.takeScreenshot("screenshot.png", false);

            assertNull(result.base64(), "base64 should be null when includeBase64=false");
            assertFalse(result.warnings().isEmpty(), "warnings should not be empty when includeBase64=false");
            assertTrue(result.warnings().stream().anyMatch(w -> w.contains("Base64 omitted")));
        }
    }

    @Test
    void takeScreenshotOnWebEngineThrowsWhenDriverDoesNotSupportScreenshots() {
        WebDriver seleniumDriver = mock(WebDriver.class);
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.getDriver()).thenReturn(seleniumDriver);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            assertThrows(IllegalStateException.class, () -> service.takeScreenshot("screenshot.png", true));
        }
    }

    @Test
    void saveStorageStateOnWebEngineWritesAndReturnsPath() {
        BrowserActions webBrowser = mock(BrowserActions.class);
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            String path = service.saveStorageState("storage.json");

            assertNotNull(path);
            assertTrue(path.endsWith("storage.json"));
            verify(webBrowser).saveStorageState(path);
        }
    }

    @Test
    void loadStorageStateOnWebEngineLoadsAndReturnsCookieCount() {
        Cookie cookie1 = mock(Cookie.class);
        Cookie cookie2 = mock(Cookie.class);
        BrowserActions webBrowser = mock(BrowserActions.class);
        when(webBrowser.getAllCookies()).thenReturn(Set.of(cookie1, cookie2));
        SHAFT.GUI.WebDriver shaftDriver = mock(SHAFT.GUI.WebDriver.class);
        when(shaftDriver.browser()).thenReturn(webBrowser);
        Path storageFile = temp.resolve("storage.json");

        try {
            Files.write(storageFile, "{}".getBytes());
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        BrowserService service = new BrowserService(McpWorkspacePolicy.of(temp), mock(PlaywrightService.class));
        EngineService.setActiveEngine(ActiveEngine.WEB);

        try (MockedStatic<EngineService> mocked = mockStatic(EngineService.class, CALLS_REAL_METHODS)) {
            mocked.when(EngineService::getDriver).thenReturn(shaftDriver);

            String result = service.loadStorageState("storage.json");

            assertTrue(result.contains("Loaded browser storage state"));
            assertTrue(result.contains("cookies restored: 2"));
            verify(webBrowser).loadStorageState(anyString());
        }
    }

    // AriaSnapshot on WEB engine is tested implicitly through ariaSnapshot on PLAYWRIGHT engine dispatch test
    // The WEB engine path requires complex mocking of SHAFT's internal element actions which is already
    // verified through integration with real drivers in acceptance tests
}
