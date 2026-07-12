package com.shaft.gui.playwright.internal;

import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Page;
import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Covers {@code PlaywrightSessionFactory#loadInitialStorageState(BrowserContext, Page)}, the
 * Playwright-side wiring for the {@code SHAFT.Properties.web.storageStatePath()} driver-init option.
 */
@Test(singleThreaded = true)
public class PlaywrightSessionFactoryStorageStateUnitTest {
    private final String savedStorageStatePath = SHAFT.Properties.web.storageStatePath();
    private final String savedBaseURL = SHAFT.Properties.web.baseURL();
    private Path storageStateFile;

    @AfterMethod(alwaysRun = true)
    public void tearDown() throws Exception {
        SHAFT.Properties.web.set().storageStatePath(savedStorageStatePath).baseURL(savedBaseURL);
        if (storageStateFile != null) {
            Files.deleteIfExists(storageStateFile);
        }
    }

    private static void invokeLoadInitialStorageState(BrowserContext context, Page page) throws Exception {
        Method method = PlaywrightSessionFactory.class.getDeclaredMethod("loadInitialStorageState", BrowserContext.class, Page.class);
        method.setAccessible(true);
        method.invoke(null, context, page);
    }

    @Test
    public void blankPropertyShouldSkipLoadingEntirely() throws Exception {
        SHAFT.Properties.web.set().storageStatePath("");
        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);

        invokeLoadInitialStorageState(context, page);

        verifyNoInteractions(context);
        verifyNoInteractions(page);
    }

    @Test
    public void configuredPropertyShouldNavigateToRecordedOriginThenLoadStorageState() throws Exception {
        storageStateFile = Files.createTempFile("shaft-playwright-init-storage-state", ".json");
        Files.writeString(storageStateFile, "{\"schemaVersion\":\"1.0\",\"origin\":\"https://example.com\","
                + "\"cookies\":[],\"origins\":[{\"origin\":\"https://example.com\",\"localStorage\":{\"k\":\"v\"},\"sessionStorage\":{}}]}");
        SHAFT.Properties.web.set().storageStatePath(storageStateFile.toString());

        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);
        when(page.url()).thenReturn("https://example.com");

        invokeLoadInitialStorageState(context, page);

        verify(page).navigate("https://example.com");
        verify(context).clearCookies();
        verify(page).evaluate(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.any());
    }

    @Test
    public void missingOriginInFileShouldFallBackToConfiguredBaseURL() throws Exception {
        storageStateFile = Files.createTempFile("shaft-playwright-init-storage-state-no-origin", ".json");
        Files.writeString(storageStateFile, "{\"schemaVersion\":\"1.0\",\"cookies\":[],\"origins\":[]}");
        SHAFT.Properties.web.set().storageStatePath(storageStateFile.toString()).baseURL("https://fallback.example.com");

        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);
        when(page.url()).thenReturn("https://fallback.example.com");

        invokeLoadInitialStorageState(context, page);

        verify(page).navigate("https://fallback.example.com");
    }

    @Test
    public void missingFileShouldLogWarningAndNotPropagateException() throws Exception {
        SHAFT.Properties.web.set().storageStatePath("target/does-not-exist-" + System.nanoTime() + ".json").baseURL("");

        BrowserContext context = mock(BrowserContext.class);
        Page page = mock(Page.class);

        // Should not throw despite the file not existing.
        invokeLoadInitialStorageState(context, page);

        verify(page, never()).navigate(org.mockito.ArgumentMatchers.anyString());
        Assert.assertTrue(true, "loadInitialStorageState swallowed the failure as expected.");
    }
}
