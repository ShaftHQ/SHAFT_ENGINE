package com.shaft.gui.browser.internal;

import org.openqa.selenium.Cookie;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import tools.jackson.databind.json.JsonMapper;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

public class BrowserStorageStateManagerTest {
    private static final String STORAGE_SNAPSHOT_SCRIPT = "return {localStorage: Object.fromEntries(Object.entries(window.localStorage)), "
            + "sessionStorage: Object.fromEntries(Object.entries(window.sessionStorage))};";
    private static final String LOAD_STORAGE_SCRIPT = "window.localStorage.clear(); window.sessionStorage.clear(); "
            + "for (const [key, value] of Object.entries(arguments[0])) { window.localStorage.setItem(key, value); } "
            + "for (const [key, value] of Object.entries(arguments[1])) { window.sessionStorage.setItem(key, value); }";
    private static final JsonMapper JSON = JsonMapper.builder().build();

    private Path storageStateFile;

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws Exception {
        if (storageStateFile != null) {
            Files.deleteIfExists(storageStateFile);
        }
    }

    @Test
    public void shouldRejectNullDriver() {
        Assert.assertThrows(IllegalArgumentException.class, () -> BrowserStorageStateManager.save(null, "target/state.json"));
        Assert.assertThrows(IllegalArgumentException.class, () -> BrowserStorageStateManager.load(null, "target/state.json"));
    }

    @Test
    public void shouldRejectBlankPath() {
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
        Assert.assertThrows(IllegalArgumentException.class, () -> BrowserStorageStateManager.save(driver, " "));
        Assert.assertThrows(IllegalArgumentException.class, () -> BrowserStorageStateManager.load(driver, null));
    }

    @Test
    public void shouldRejectNonJavascriptCapableDriver() {
        WebDriver driver = mock(WebDriver.class);
        WebDriver.Options options = mock(WebDriver.Options.class);
        when(driver.manage()).thenReturn(options);
        Assert.assertThrows(IllegalArgumentException.class, () -> BrowserStorageStateManager.save(driver, "target/state.json"));
    }

    @Test
    public void shouldSaveCookiesAndStorageToJsonFile() throws Exception {
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
        WebDriver.Options options = mock(WebDriver.Options.class);
        when(driver.manage()).thenReturn(options);
        when(driver.getCurrentUrl()).thenReturn("https://example.com/app");

        Cookie cookie = new Cookie.Builder("authToken", "secret")
                .domain("example.com")
                .path("/")
                .isSecure(true)
                .isHttpOnly(true)
                .sameSite("Strict")
                .build();
        when(options.getCookies()).thenReturn(Set.of(cookie));
        when(((JavascriptExecutor) driver).executeScript(STORAGE_SNAPSHOT_SCRIPT)).thenReturn(Map.of(
                "localStorage", Map.of("authToken", "storage-secret"),
                "sessionStorage", Map.of("tab", "checkout")));

        storageStateFile = Files.createTempFile("shaft-storage-state", ".json");
        BrowserStorageStateManager.save(driver, storageStateFile.toString());

        BrowserStorageStateManager.StorageState state =
                JSON.readValue(storageStateFile.toFile(), BrowserStorageStateManager.StorageState.class);

        Assert.assertEquals(state.schemaVersion, "1.0");
        Assert.assertEquals(state.origin, "https://example.com");
        Assert.assertEquals(state.cookies.size(), 1);
        BrowserStorageStateManager.CookieState savedCookie = state.cookies.getFirst();
        Assert.assertEquals(savedCookie.name, "authToken");
        Assert.assertEquals(savedCookie.value, "secret");
        Assert.assertEquals(savedCookie.domain, "example.com");
        Assert.assertTrue(savedCookie.secure);
        Assert.assertTrue(savedCookie.httpOnly);
        Assert.assertEquals(state.origins.size(), 1);
        BrowserStorageStateManager.OriginStorage originStorage = state.origins.getFirst();
        Assert.assertEquals(originStorage.localStorage, Map.of("authToken", "storage-secret"));
        Assert.assertEquals(originStorage.sessionStorage, Map.of("tab", "checkout"));
    }

    @Test
    public void shouldLoadCookiesAndStorageFromJsonFile() throws Exception {
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
        WebDriver.Options options = mock(WebDriver.Options.class);
        when(driver.manage()).thenReturn(options);

        storageStateFile = Files.createTempFile("shaft-storage-state", ".json");
        Files.writeString(storageStateFile, """
                {
                  "schemaVersion" : "1.0",
                  "origin" : "https://example.com",
                  "cookies" : [ {
                    "name" : "authToken",
                    "value" : "secret",
                    "domain" : "example.com",
                    "path" : "/",
                    "expiry" : null,
                    "secure" : true,
                    "httpOnly" : true,
                    "sameSite" : "Strict"
                  } ],
                  "origins" : [ {
                    "origin" : "https://example.com",
                    "localStorage" : { "authToken" : "storage-secret" },
                    "sessionStorage" : { "tab" : "checkout" }
                  } ]
                }
                """);

        BrowserStorageStateManager.load(driver, storageStateFile.toString());

        verify(options, times(1)).deleteAllCookies();
        var cookieCaptor = org.mockito.ArgumentCaptor.forClass(Cookie.class);
        verify(options, times(1)).addCookie(cookieCaptor.capture());
        Cookie addedCookie = cookieCaptor.getValue();
        Assert.assertEquals(addedCookie.getName(), "authToken");
        Assert.assertEquals(addedCookie.getValue(), "secret");
        Assert.assertEquals(addedCookie.getDomain(), "example.com");
        Assert.assertTrue(addedCookie.isSecure());
        Assert.assertTrue(addedCookie.isHttpOnly());

        verify((JavascriptExecutor) driver).executeScript(
                LOAD_STORAGE_SCRIPT,
                Map.of("authToken", "storage-secret"),
                Map.of("tab", "checkout"));
    }

    @Test
    public void shouldRaiseIllegalStateWhenLoadFileMissing() {
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
        WebDriver.Options options = mock(WebDriver.Options.class);
        when(driver.manage()).thenReturn(options);

        Assert.assertThrows(IllegalStateException.class,
                () -> BrowserStorageStateManager.load(driver, "target/does-not-exist-" + System.nanoTime() + ".json"));
    }

    @Test
    public void shouldDefaultToEmptyStorageWhenNoOriginsPresent() throws Exception {
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
        WebDriver.Options options = mock(WebDriver.Options.class);
        when(driver.manage()).thenReturn(options);

        storageStateFile = Files.createTempFile("shaft-storage-state-empty", ".json");
        Files.writeString(storageStateFile, """
                {
                  "schemaVersion" : "1.0",
                  "origin" : "https://example.com",
                  "cookies" : [ ],
                  "origins" : [ ]
                }
                """);

        BrowserStorageStateManager.load(driver, storageStateFile.toString());

        verify(options, times(1)).deleteAllCookies();
        verify(options, never()).addCookie(any(Cookie.class));
        verify((JavascriptExecutor) driver).executeScript(LOAD_STORAGE_SCRIPT, Map.of(), Map.of());
    }
}
