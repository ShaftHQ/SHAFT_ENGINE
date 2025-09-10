package testPackage.properties;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.RetryAnalyzer;
import org.testng.Assert;
import org.testng.annotations.*;

/**
 * Demonstration test to show the new retry evidence collection functionality.
 * This test intentionally fails on the first attempt to trigger retry with evidence collection.
 */
public class RetryFunctionalityDemoTest {
    
    private SHAFT.GUI.WebDriver driver;
    private static int attemptCount = 0;
    
    @BeforeClass
    public void configureRetrySettings() {
        // Configure retry settings for demonstration
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(2);
        SHAFT.Properties.flags.set().retryEnableVideoRecording(true);
        SHAFT.Properties.flags.set().retryEnableNetworkLogging(true);
        SHAFT.Properties.flags.set().retryEnableConsoleLogging(true);
        SHAFT.Properties.flags.set().retryEnableEnhancedStabilization(true);
    }
    
    @BeforeMethod
    public void setUp() {
        driver = new SHAFT.GUI.WebDriver();
        // Register driver with RetryAnalyzer for evidence collection
        RetryAnalyzer.setCurrentDriver(driver.getDriver());
    }
    
    @AfterMethod
    public void tearDown() {
        RetryAnalyzer.clearCurrentDriver();
        if (driver != null) {
            driver.quit();
        }
    }

    @Test(description = "Demonstration test that fails on first attempt to show retry evidence collection")
    public void demonstrateRetryEvidenceCollection() {
        attemptCount++;
        
        // Navigate to a page to generate network activity and potential console logs
        driver.browser().navigateToURL("https://www.google.com");
        driver.assertThat().browser().title().contains("Google");
        
        // Intentionally fail on the first attempt to trigger retry
        if (attemptCount == 1) {
            // Log some information before failing
            System.out.println("=== FIRST ATTEMPT - THIS WILL FAIL TO DEMONSTRATE RETRY ===");
            System.out.println("Current attempt: " + attemptCount);
            System.out.println("Video recording enabled: " + SHAFT.Properties.flags.retryEnableVideoRecording());
            System.out.println("Network logging enabled: " + SHAFT.Properties.flags.retryEnableNetworkLogging());
            System.out.println("Console logging enabled: " + SHAFT.Properties.flags.retryEnableConsoleLogging());
            System.out.println("Enhanced stabilization enabled: " + SHAFT.Properties.flags.retryEnableEnhancedStabilization());
            
            // Perform some browser actions to generate evidence
            driver.browser().navigateToURL("https://httpbin.org/status/404"); // Generate network error
            
            // Fail intentionally to trigger retry
            Assert.fail("Intentional failure on attempt #" + attemptCount + " to demonstrate retry evidence collection");
        } else {
            // Success on retry attempt
            System.out.println("=== RETRY ATTEMPT SUCCESSFUL ===");
            System.out.println("Current attempt: " + attemptCount);
            System.out.println("This attempt should have video recording, network logs, and console logs attached");
            
            // Navigate to a successful page
            driver.browser().navigateToURL("https://www.google.com");
            driver.assertThat().browser().title().contains("Google");
            
            // This should pass
            Assert.assertTrue(true, "Retry attempt succeeded");
        }
    }

    @Test(description = "Test that shows normal test execution without retry")
    public void normalTestWithoutRetry() {
        // This test should pass on first attempt without triggering retry
        driver.browser().navigateToURL("https://www.google.com");
        driver.assertThat().browser().title().contains("Google");
        
        // Verify search box is present
        driver.element().click(driver.locator().hasTagName("input").hasAttribute("name", "q").build());
        driver.element().type(driver.locator().hasTagName("input").hasAttribute("name", "q").build(), "SHAFT Engine");
        
        Assert.assertTrue(true, "Normal test completed successfully without retry");
    }

    @Test(description = "Test retry configuration properties are working")
    public void verifyRetryConfiguration() {
        // Verify that our retry configuration is active
        int maxRetries = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        boolean videoEnabled = SHAFT.Properties.flags.retryEnableVideoRecording();
        boolean networkEnabled = SHAFT.Properties.flags.retryEnableNetworkLogging();
        boolean consoleEnabled = SHAFT.Properties.flags.retryEnableConsoleLogging();
        boolean stabilizationEnabled = SHAFT.Properties.flags.retryEnableEnhancedStabilization();
        
        System.out.println("=== RETRY CONFIGURATION VERIFICATION ===");
        System.out.println("Max retry attempts: " + maxRetries);
        System.out.println("Video recording: " + videoEnabled);
        System.out.println("Network logging: " + networkEnabled);
        System.out.println("Console logging: " + consoleEnabled);
        System.out.println("Enhanced stabilization: " + stabilizationEnabled);
        
        Assert.assertEquals(maxRetries, 2, "Max retry attempts should be 2");
        Assert.assertTrue(videoEnabled, "Video recording should be enabled");
        Assert.assertTrue(networkEnabled, "Network logging should be enabled");
        Assert.assertTrue(consoleEnabled, "Console logging should be enabled");
        Assert.assertTrue(stabilizationEnabled, "Enhanced stabilization should be enabled");
        
        // Simple browser test to ensure everything is working
        driver.browser().navigateToURL("https://www.google.com");
        driver.assertThat().browser().title().contains("Google");
    }
}