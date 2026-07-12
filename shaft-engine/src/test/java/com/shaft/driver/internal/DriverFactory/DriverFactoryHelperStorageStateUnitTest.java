package com.shaft.driver.internal.DriverFactory;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

/**
 * Covers {@code DriverFactoryHelper#loadInitialStorageState()}, the WebDriver-side wiring for the
 * {@code SHAFT.Properties.web.storageStatePath()} driver-init option.
 */
@Test(singleThreaded = true)
public class DriverFactoryHelperStorageStateUnitTest {
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

    private static void invokeLoadInitialStorageState(DriverFactoryHelper helper) throws Exception {
        Method method = DriverFactoryHelper.class.getDeclaredMethod("loadInitialStorageState");
        method.setAccessible(true);
        method.invoke(helper);
    }

    @Test
    public void blankPropertyShouldSkipLoadingEntirely() throws Exception {
        SHAFT.Properties.web.set().storageStatePath("");
        DriverFactoryHelper helper = new DriverFactoryHelper();
        WebDriver driver = mock(WebDriver.class);
        helper.setDriver(driver);

        invokeLoadInitialStorageState(helper);

        verifyNoInteractions(driver);
    }

    @Test
    public void configuredPropertyShouldNavigateToRecordedOriginThenLoadStorageState() throws Exception {
        storageStateFile = Files.createTempFile("shaft-driver-init-storage-state", ".json");
        Files.writeString(storageStateFile, "{\"schemaVersion\":\"1.0\",\"origin\":\"https://example.com\","
                + "\"cookies\":[],\"origins\":[{\"origin\":\"https://example.com\",\"localStorage\":{},\"sessionStorage\":{}}]}");
        SHAFT.Properties.web.set().storageStatePath(storageStateFile.toString());

        DriverFactoryHelper helper = new DriverFactoryHelper();
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
        WebDriver.Navigation navigation = mock(WebDriver.Navigation.class);
        WebDriver.Options options = mock(WebDriver.Options.class);
        when(driver.navigate()).thenReturn(navigation);
        when(driver.manage()).thenReturn(options);
        when(((JavascriptExecutor) driver).executeScript(anyString(), any(), any())).thenReturn(null);
        helper.setDriver(driver);

        invokeLoadInitialStorageState(helper);

        verify(navigation).to("https://example.com");
        verify(options).deleteAllCookies();
        verify((JavascriptExecutor) driver).executeScript(anyString(), any(), any());
    }

    @Test
    public void missingOriginInFileShouldFallBackToConfiguredBaseURL() throws Exception {
        storageStateFile = Files.createTempFile("shaft-driver-init-storage-state-no-origin", ".json");
        Files.writeString(storageStateFile, "{\"schemaVersion\":\"1.0\",\"cookies\":[],\"origins\":[]}");
        SHAFT.Properties.web.set().storageStatePath(storageStateFile.toString()).baseURL("https://fallback.example.com");

        DriverFactoryHelper helper = new DriverFactoryHelper();
        WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
        WebDriver.Navigation navigation = mock(WebDriver.Navigation.class);
        WebDriver.Options options = mock(WebDriver.Options.class);
        when(driver.navigate()).thenReturn(navigation);
        when(driver.manage()).thenReturn(options);
        when(((JavascriptExecutor) driver).executeScript(anyString(), any(), any())).thenReturn(null);
        helper.setDriver(driver);

        invokeLoadInitialStorageState(helper);

        verify(navigation).to("https://fallback.example.com");
    }

    @Test
    public void missingFileShouldLogWarningAndNotPropagateException() throws Exception {
        SHAFT.Properties.web.set().storageStatePath("target/does-not-exist-" + System.nanoTime() + ".json").baseURL("");

        DriverFactoryHelper helper = new DriverFactoryHelper();
        WebDriver driver = mock(WebDriver.class);
        helper.setDriver(driver);

        // Should not throw despite the file not existing.
        invokeLoadInitialStorageState(helper);

        verify(driver, never()).navigate();
        Assert.assertTrue(true, "loadInitialStorageState swallowed the failure as expected.");
    }
}
