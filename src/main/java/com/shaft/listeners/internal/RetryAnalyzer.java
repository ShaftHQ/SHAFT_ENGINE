package com.shaft.listeners.internal;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.internal.video.RecordManager;
import com.shaft.tools.internal.logs.ConsoleNetworkLogger;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.WebDriver;
import org.testng.IRetryAnalyzer;
import org.testng.ITestResult;

public class RetryAnalyzer implements IRetryAnalyzer {
    private final int maxRetryCount = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
    private int counter = 0;
    private static final ThreadLocal<WebDriver> currentDriver = new ThreadLocal<>();

    @Override
    public boolean retry(ITestResult iTestResult) {
        if (counter < maxRetryCount) {
            counter++;
            String testMethodName = iTestResult.getMethod().getMethodName();
            String threadName = Thread.currentThread().getName();
            
            ReportManager.logDiscrete("=== RETRY #" + counter + " INITIATED ===");
            ReportManager.logDiscrete("Test: " + testMethodName + ", Thread: " + threadName);
            ReportManager.logDiscrete("Gathering evidence for retry analysis...");
            
            // Start evidence collection for retry
            startRetryEvidenceCollection(testMethodName, counter);
            
            // Implement enhanced stabilization for retry
            if (SHAFT.Properties.flags.retryEnableEnhancedStabilization()) {
                implementRetryStabilization(testMethodName, counter);
            }
            
            ReportManager.logDiscrete("Retry #" + counter + " preparation completed for test: " + testMethodName);
            return true;
        }
        return false;
    }

    /**
     * Sets the current WebDriver instance for this thread.
     * This should be called from test setup methods.
     *
     * @param driver the WebDriver instance
     */
    public static void setCurrentDriver(WebDriver driver) {
        currentDriver.set(driver);
    }

    /**
     * Gets the current WebDriver instance for this thread.
     *
     * @return the WebDriver instance, or null if not set
     */
    public static WebDriver getCurrentDriver() {
        return currentDriver.get();
    }

    /**
     * Clears the current WebDriver instance for this thread.
     * This should be called from test teardown methods.
     */
    public static void clearCurrentDriver() {
        currentDriver.remove();
    }

    /**
     * Starts collecting evidence for retry analysis including video recording,
     * network logs, and console logs.
     *
     * @param testMethodName the name of the test method
     * @param retryAttempt the current retry attempt number
     */
    private void startRetryEvidenceCollection(String testMethodName, int retryAttempt) {
        WebDriver driver = getCurrentDriver();
        
        if (driver == null) {
            // Try to get driver from DriverFactoryHelper as fallback
            try {
                driver = DriverFactoryHelper.getDriver();
                if (driver != null) {
                    setCurrentDriver(driver);
                }
            } catch (Exception e) {
                ReportManager.logDiscrete("Could not retrieve WebDriver instance for retry evidence collection: " + e.getMessage());
                return;
            }
        }

        if (driver != null) {
            // Start video recording for retry
            if (SHAFT.Properties.flags.retryEnableVideoRecording()) {
                RecordManager.startVideoRecordingForRetry(driver, testMethodName, retryAttempt);
            }

            // Start network and console logging for retry
            if (SHAFT.Properties.flags.retryEnableNetworkLogging() || SHAFT.Properties.flags.retryEnableConsoleLogging()) {
                ConsoleNetworkLogger.startLoggingForRetry(driver);
            }
        }
    }

    /**
     * Implements enhanced stabilization logic to improve the likelihood of retry success.
     * This includes additional wait strategies and element stability checks.
     *
     * @param testMethodName the name of the test method
     * @param retryAttempt the current retry attempt number
     */
    private void implementRetryStabilization(String testMethodName, int retryAttempt) {
        try {
            // Implement progressive wait strategy - increase wait time with each retry
            int additionalWaitTime = retryAttempt * 2; // 2 seconds per retry attempt
            ReportManager.logDiscrete("Implementing enhanced stabilization with " + additionalWaitTime + "s additional wait time");
            
            Thread.sleep(additionalWaitTime * 1000L);
            
            // Additional stabilization logic can be added here:
            // - Clear browser cache
            // - Refresh page
            // - Reset specific browser settings
            // - Wait for network idle state
            
            WebDriver driver = getCurrentDriver();
            if (driver != null) {
                // Clear any alert dialogs that might be interfering
                try {
                    driver.switchTo().alert().dismiss();
                    ReportManager.logDiscrete("Dismissed alert dialog during retry stabilization");
                } catch (Exception e) {
                    // No alert present, which is expected
                }
                
                // Clear browser cache and cookies for certain retry attempts
                if (retryAttempt > 1) {
                    try {
                        driver.manage().deleteAllCookies();
                        ReportManager.logDiscrete("Cleared browser cookies for retry #" + retryAttempt);
                    } catch (Exception e) {
                        ReportManager.logDiscrete("Could not clear browser cookies: " + e.getMessage());
                    }
                }
            }
            
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            ReportManager.logDiscrete("Retry stabilization interrupted: " + e.getMessage());
        } catch (Exception e) {
            ReportManager.logDiscrete("Error during retry stabilization: " + e.getMessage());
        }
    }

    /**
     * Stops evidence collection and attaches all gathered evidence to the test report.
     * This should be called after the retry attempt is completed.
     *
     * @param testMethodName the name of the test method
     * @param retryAttempt the retry attempt number
     * @param wasSuccessful whether the retry was successful
     */
    public static void stopRetryEvidenceCollection(String testMethodName, int retryAttempt, boolean wasSuccessful) {
        try {
            ReportManager.logDiscrete("=== RETRY #" + retryAttempt + " EVIDENCE COLLECTION ===");
            ReportManager.logDiscrete("Test: " + testMethodName + ", Success: " + wasSuccessful);
            
            // Stop and attach video recording
            if (SHAFT.Properties.flags.retryEnableVideoRecording()) {
                RecordManager.stopVideoRecordingForRetryAndAttach(testMethodName, retryAttempt);
            }

            // Stop and attach network/console logs
            if (SHAFT.Properties.flags.retryEnableNetworkLogging() || SHAFT.Properties.flags.retryEnableConsoleLogging()) {
                ConsoleNetworkLogger.stopLoggingAndAttach(testMethodName, retryAttempt);
            }

            // Log retry summary
            String retryStatus = wasSuccessful ? "PASSED" : "FAILED";
            ReportManager.logDiscrete("Retry #" + retryAttempt + " completed with status: " + retryStatus);
            
            // Attach retry summary information
            String retrySummary = String.format(
                "=== RETRY ATTEMPT #%d SUMMARY ===\n" +
                "Test Method: %s\n" +
                "Status: %s\n" +
                "Thread: %s\n" +
                "Video Recording: %s\n" +
                "Network Logging: %s\n" +
                "Console Logging: %s\n" +
                "Enhanced Stabilization: %s\n" +
                "================================",
                retryAttempt,
                testMethodName,
                retryStatus,
                Thread.currentThread().getName(),
                SHAFT.Properties.flags.retryEnableVideoRecording() ? "Enabled" : "Disabled",
                SHAFT.Properties.flags.retryEnableNetworkLogging() ? "Enabled" : "Disabled",
                SHAFT.Properties.flags.retryEnableConsoleLogging() ? "Enabled" : "Disabled",
                SHAFT.Properties.flags.retryEnableEnhancedStabilization() ? "Enabled" : "Disabled"
            );
            
            java.io.InputStream summaryStream = new java.io.ByteArrayInputStream(retrySummary.getBytes());
            ReportManagerHelper.attach("Retry Summary - Attempt #" + retryAttempt, 
                    testMethodName + "_retry_" + retryAttempt + "_summary", summaryStream);
                    
        } catch (Exception e) {
            ReportManager.logDiscrete("Error during retry evidence collection cleanup: " + e.getMessage());
        }
    }
}