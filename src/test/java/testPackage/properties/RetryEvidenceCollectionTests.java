package testPackage.properties;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.RetryAnalyzer;
import com.shaft.tools.internal.logs.ConsoleNetworkLogger;
import org.testng.Assert;
import org.testng.annotations.*;

public class RetryEvidenceCollectionTests {
    
    private SHAFT.GUI.WebDriver driver;
    private static int retryCount = 0;
    
    @BeforeMethod
    public void beforeMethod() {
        // Configure retry properties for testing
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(2);
        SHAFT.Properties.flags.set().retryEnableVideoRecording(true);
        SHAFT.Properties.flags.set().retryEnableNetworkLogging(true);
        SHAFT.Properties.flags.set().retryEnableConsoleLogging(true);
        SHAFT.Properties.flags.set().retryEnableEnhancedStabilization(true);
        
        driver = new SHAFT.GUI.WebDriver();
        RetryAnalyzer.setCurrentDriver(driver.getDriver());
    }
    
    @AfterMethod
    public void afterMethod() {
        RetryAnalyzer.clearCurrentDriver();
        if (driver != null) {
            driver.quit();
        }
    }

    @Test(description = "Test retry evidence collection properties are accessible")
    public void testRetryEvidenceCollectionPropertiesAccessibility() {
        // Test that all new retry properties can be read
        boolean videoRecording = SHAFT.Properties.flags.retryEnableVideoRecording();
        boolean networkLogging = SHAFT.Properties.flags.retryEnableNetworkLogging();
        boolean consoleLogging = SHAFT.Properties.flags.retryEnableConsoleLogging();
        boolean enhancedStabilization = SHAFT.Properties.flags.retryEnableEnhancedStabilization();
        
        // Verify properties are accessible (defaults should be true)
        Assert.assertTrue(videoRecording, "Retry video recording should be accessible and enabled by default");
        Assert.assertTrue(networkLogging, "Retry network logging should be accessible and enabled by default");
        Assert.assertTrue(consoleLogging, "Retry console logging should be accessible and enabled by default");
        Assert.assertTrue(enhancedStabilization, "Retry enhanced stabilization should be accessible and enabled by default");
    }

    @Test(description = "Test retry evidence collection property setters")
    public void testRetryEvidenceCollectionPropertySetters() {
        // Test setting properties to false
        SHAFT.Properties.flags.set().retryEnableVideoRecording(false);
        SHAFT.Properties.flags.set().retryEnableNetworkLogging(false);
        SHAFT.Properties.flags.set().retryEnableConsoleLogging(false);
        SHAFT.Properties.flags.set().retryEnableEnhancedStabilization(false);
        
        // Verify properties were set
        Assert.assertFalse(SHAFT.Properties.flags.retryEnableVideoRecording(), "Video recording should be disabled");
        Assert.assertFalse(SHAFT.Properties.flags.retryEnableNetworkLogging(), "Network logging should be disabled");
        Assert.assertFalse(SHAFT.Properties.flags.retryEnableConsoleLogging(), "Console logging should be disabled");
        Assert.assertFalse(SHAFT.Properties.flags.retryEnableEnhancedStabilization(), "Enhanced stabilization should be disabled");
        
        // Test setting properties back to true
        SHAFT.Properties.flags.set().retryEnableVideoRecording(true);
        SHAFT.Properties.flags.set().retryEnableNetworkLogging(true);
        SHAFT.Properties.flags.set().retryEnableConsoleLogging(true);
        SHAFT.Properties.flags.set().retryEnableEnhancedStabilization(true);
        
        // Verify properties were set back
        Assert.assertTrue(SHAFT.Properties.flags.retryEnableVideoRecording(), "Video recording should be re-enabled");
        Assert.assertTrue(SHAFT.Properties.flags.retryEnableNetworkLogging(), "Network logging should be re-enabled");
        Assert.assertTrue(SHAFT.Properties.flags.retryEnableConsoleLogging(), "Console logging should be re-enabled");
        Assert.assertTrue(SHAFT.Properties.flags.retryEnableEnhancedStabilization(), "Enhanced stabilization should be re-enabled");
    }

    @Test(description = "Test console network logger functionality")
    public void testConsoleNetworkLoggerFunctionality() {
        // Test that ConsoleNetworkLogger can be started and stopped without errors
        Assert.assertFalse(ConsoleNetworkLogger.isLoggingActive(), "Logging should not be active initially");
        
        ConsoleNetworkLogger.startLoggingForRetry(driver.getDriver());
        
        // Note: isLoggingActive might still be false if the driver doesn't support logging
        // This is expected behavior for the test environment
        
        // Navigate to a page to generate some logs
        driver.browser().navigateToURL("https://www.google.com");
        
        // Get current logs (should be empty lists if logging not supported, but shouldn't throw errors)
        var consoleLogs = ConsoleNetworkLogger.getCurrentConsoleLogs();
        var networkLogs = ConsoleNetworkLogger.getCurrentNetworkLogs();
        
        Assert.assertNotNull(consoleLogs, "Console logs should return a non-null list");
        Assert.assertNotNull(networkLogs, "Network logs should return a non-null list");
        
        // Stop logging (should not throw errors)
        ConsoleNetworkLogger.stopLoggingAndAttach("testMethod", 1);
        
        Assert.assertFalse(ConsoleNetworkLogger.isLoggingActive(), "Logging should not be active after stopping");
    }

    @Test(description = "Test retry analyzer thread safety")
    public void testRetryAnalyzerThreadSafety() {
        // Test setting and getting driver for current thread
        RetryAnalyzer.setCurrentDriver(driver.getDriver());
        
        Assert.assertEquals(RetryAnalyzer.getCurrentDriver(), driver.getDriver(), 
                "Current driver should match the set driver");
        
        RetryAnalyzer.clearCurrentDriver();
        Assert.assertNull(RetryAnalyzer.getCurrentDriver(), 
                "Current driver should be null after clearing");
    }

    @Test(description = "Test that retry analyzer integrates with evidence collection")
    public void testRetryAnalyzerEvidenceIntegration() {
        // This is a basic integration test to ensure the RetryAnalyzer can call evidence collection methods
        // without throwing exceptions
        
        RetryAnalyzer.setCurrentDriver(driver.getDriver());
        
        // Test stopping evidence collection (should handle null/not-started gracefully)
        Assert.assertDoesNotThrow(() -> {
            RetryAnalyzer.stopRetryEvidenceCollection("testMethod", 1, true);
        }, "Evidence collection should handle being called without being started");
        
        Assert.assertDoesNotThrow(() -> {
            RetryAnalyzer.stopRetryEvidenceCollection("testMethod", 1, false);
        }, "Evidence collection should handle failed retry scenarios");
    }

    @Test(description = "Test navigation and basic browser functionality with retry setup")
    public void testBasicBrowserFunctionalityWithRetrySetup() {
        // Test that normal browser operations still work with retry enhancement
        RetryAnalyzer.setCurrentDriver(driver.getDriver());
        
        // Basic navigation test
        driver.browser().navigateToURL("https://www.google.com");
        driver.assertThat().browser().title().contains("Google");
        
        // Test that browser operations work normally with retry analyzer setup
        Assert.assertNotNull(driver.getDriver(), "Driver should be available");
        Assert.assertNotNull(RetryAnalyzer.getCurrentDriver(), "Retry analyzer should have driver reference");
    }
}