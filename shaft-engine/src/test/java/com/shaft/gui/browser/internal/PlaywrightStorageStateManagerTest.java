package com.shaft.gui.browser.internal;

import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.options.Cookie;
import com.microsoft.playwright.options.SameSiteAttribute;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import tools.jackson.databind.json.JsonMapper;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class PlaywrightStorageStateManagerTest {
    private static final String STORAGE_SNAPSHOT_SCRIPT = "() => ({localStorage: Object.fromEntries(Object.entries(window.localStorage)), "
            + "sessionStorage: Object.fromEntries(Object.entries(window.sessionStorage))})";
    private static final String LOAD_STORAGE_SCRIPT = "(state) => { window.localStorage.clear(); window.sessionStorage.clear(); "
            + "for (const [key, value] of Object.entries(state.localStorage)) { window.localStorage.setItem(key, value); } "
            + "for (const [key, value] of Object.entries(state.sessionStorage)) { window.sessionStorage.setItem(key, value); } }";
    private static final JsonMapper JSON = JsonMapper.builder().build();

    private Path storageStateFile;

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws Exception {
        if (storageStateFile != null) {
            Files.deleteIfExists(storageStateFile);
        }
    }

    @Test
    public void shouldRejectNullSession() {
        Page page = mock(Page.class);
        BrowserContext context = mock(BrowserContext.class);
        Assert.assertThrows(IllegalArgumentException.class, () -> PlaywrightStorageStateManager.save(null, page, "target/state.json"));
        Assert.assertThrows(IllegalArgumentException.class, () -> PlaywrightStorageStateManager.save(context, null, "target/state.json"));
        Assert.assertThrows(IllegalArgumentException.class, () -> PlaywrightStorageStateManager.load(null, page, "target/state.json"));
    }

    @Test
    public void shouldRejectBlankPath() {
        Page page = mock(Page.class);
        BrowserContext context = mock(BrowserContext.class);
        Assert.assertThrows(IllegalArgumentException.class, () -> PlaywrightStorageStateManager.save(context, page, " "));
        Assert.assertThrows(IllegalArgumentException.class, () -> PlaywrightStorageStateManager.load(context, page, null));
    }

    @Test
    public void shouldSaveCookiesAndStorageToJsonFile() throws Exception {
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);
        when(page.url()).thenReturn("https://example.com/app");

        Cookie cookie = new Cookie("authToken", "secret")
                .setDomain("example.com")
                .setPath("/")
                .setSecure(true)
                .setHttpOnly(true)
                .setSameSite(SameSiteAttribute.STRICT);
        when(context.cookies()).thenReturn(List.of(cookie));
        when(page.evaluate(STORAGE_SNAPSHOT_SCRIPT)).thenReturn(Map.of(
                "localStorage", Map.of("authToken", "storage-secret"),
                "sessionStorage", Map.of("tab", "checkout")));

        storageStateFile = Files.createTempFile("shaft-pw-storage-state", ".json");
        PlaywrightStorageStateManager.save(context, page, storageStateFile.toString());

        BrowserStorageStateManager.StorageState state =
                JSON.readValue(storageStateFile.toFile(), BrowserStorageStateManager.StorageState.class);

        Assert.assertEquals(state.schemaVersion, "1.0");
        Assert.assertEquals(state.origin, "https://example.com");
        Assert.assertEquals(state.cookies.size(), 1);
        BrowserStorageStateManager.CookieState savedCookie = state.cookies.getFirst();
        Assert.assertEquals(savedCookie.name, "authToken");
        Assert.assertEquals(savedCookie.value, "secret");
        Assert.assertEquals(savedCookie.domain, "example.com");
        Assert.assertEquals(savedCookie.sameSite, "Strict");
        Assert.assertTrue(savedCookie.secure);
        Assert.assertTrue(savedCookie.httpOnly);
        Assert.assertEquals(state.origins.size(), 1);
        BrowserStorageStateManager.OriginStorage originStorage = state.origins.getFirst();
        Assert.assertEquals(originStorage.localStorage, Map.of("authToken", "storage-secret"));
        Assert.assertEquals(originStorage.sessionStorage, Map.of("tab", "checkout"));
    }

    /**
     * The fixture below is the exact JSON shape written by {@link BrowserStorageStateManager} (the WebDriver
     * backend), proving that a storage-state file produced by one backend loads correctly through the other.
     */
    @Test
    public void shouldLoadCookiesAndStorageFromWebDriverProducedJsonFile() throws Exception {
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);
        when(page.url()).thenReturn("https://example.com/app");

        storageStateFile = Files.createTempFile("shaft-pw-storage-state", ".json");
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

        PlaywrightStorageStateManager.load(context, page, storageStateFile.toString());

        verify(context, times(1)).clearCookies();
        var cookiesCaptor = org.mockito.ArgumentCaptor.forClass(List.class);
        verify(context, times(1)).addCookies(cookiesCaptor.capture());
        @SuppressWarnings("unchecked")
        List<Cookie> addedCookies = (List<Cookie>) cookiesCaptor.getValue();
        Assert.assertEquals(addedCookies.size(), 1);
        Cookie addedCookie = addedCookies.getFirst();
        Assert.assertEquals(addedCookie.name, "authToken");
        Assert.assertEquals(addedCookie.value, "secret");
        Assert.assertEquals(addedCookie.domain, "example.com");
        Assert.assertEquals(addedCookie.sameSite, SameSiteAttribute.STRICT);
        Assert.assertEquals(addedCookie.secure, Boolean.TRUE);
        Assert.assertEquals(addedCookie.httpOnly, Boolean.TRUE);
        Assert.assertEquals(addedCookie.expires, Double.valueOf(-1));

        verify(page).evaluate(LOAD_STORAGE_SCRIPT,
                Map.of("localStorage", Map.of("authToken", "storage-secret"),
                        "sessionStorage", Map.of("tab", "checkout")));
    }

    @Test
    public void shouldRaiseIllegalStateWhenLoadFileMissing() {
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);

        Assert.assertThrows(IllegalStateException.class,
                () -> PlaywrightStorageStateManager.load(context, page, "target/does-not-exist-" + System.nanoTime() + ".json"));
    }

    @Test
    public void shouldDefaultToEmptyStorageAndSkipCookiesWhenNonePresent() throws Exception {
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);

        storageStateFile = Files.createTempFile("shaft-pw-storage-state-empty", ".json");
        Files.writeString(storageStateFile, """
                {
                  "schemaVersion" : "1.0",
                  "origin" : "https://example.com",
                  "cookies" : [ ],
                  "origins" : [ ]
                }
                """);

        PlaywrightStorageStateManager.load(context, page, storageStateFile.toString());

        verify(context, times(1)).clearCookies();
        verify(context, never()).addCookies(any());
        verify(page).evaluate(LOAD_STORAGE_SCRIPT, Map.of("localStorage", Map.of(), "sessionStorage", Map.of()));
    }
}
