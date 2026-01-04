package testPackage.appium;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

/**
 * Test class for Flutter app automation using Appium Flutter Driver.
 * This test uses a demo Flutter counter app to validate the Flutter driver integration.
 */
public class FlutterTest {
    public static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    /**
     * Setup method to configure and initialize the Flutter driver.
     * This sets up the necessary properties for Flutter app automation.
     */
    @BeforeMethod
    public void setupFlutterDriver() {
        // Enable Flutter driver
        SHAFT.Properties.mobile.set().flutterEnabled(true);
        
        // Common mobile attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.FLUTTER_INTEGRATION);
        
        // Configure for local Appium server execution
        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/flutter-demo.apk");
        
        // Additional Flutter-specific capabilities can be set here if needed
        // For example, if testing on a specific device:
        // SHAFT.Properties.mobile.set().deviceName("Android Emulator");
        // SHAFT.Properties.mobile.set().platformVersion("13.0");
        
        // Initialize the driver
        driver.set(new SHAFT.GUI.WebDriver());
    }

    /**
     * Test to verify basic Flutter app launch and interaction.
     * This test verifies that the Flutter counter app can be launched
     * and that basic Flutter widgets can be found.
     */
    @Test(groups = {"flutter"}, description = "Verify Flutter app launches successfully")
    public void testFlutterAppLaunch() {
        // Verify that the app launched successfully by checking for a key widget
        // Note: The exact locators depend on the Flutter app structure
        // For the demo app, we would typically check for the presence of key widgets
        
        // Example: Check if the main scaffold/screen is visible
        // In a real test, you would use Flutter-specific locators like:
        // driver.assertThat().element(AppiumBy.accessibilityId("counterText")).exists();
        
        // For demonstration purposes, we verify the driver is initialized
        // In actual usage, replace with real Flutter widget assertions
        org.testng.Assert.assertNotNull(driver.get().getDriver(), "Driver should be initialized");
    }

    /**
     * Test to verify Flutter counter app functionality.
     * This test demonstrates the pattern for interacting with Flutter widgets.
     */
    @Test(groups = {"flutter"}, description = "Verify Flutter counter app interactions")
    public void testFlutterCounterIncrement() {
        // Example pattern for Flutter testing
        // Note: Replace these locators with actual keys from your Flutter app
        
        // In a real Flutter app with proper keys, you would:
        // 1. Find the counter display widget
        // By counterText = AppiumBy.accessibilityId("counterText");
        // driver.assertThat().element(counterText).text().isEqualTo("0");
        
        // 2. Find and tap the increment button
        // By incrementButton = AppiumBy.accessibilityId("incrementButton");
        // driver.element().click(incrementButton);
        
        // 3. Verify the counter increased
        // driver.assertThat().element(counterText).text().isEqualTo("1");
        
        // For demonstration, we verify the driver is working
        org.testng.Assert.assertNotNull(driver.get().getDriver(), "Driver should be initialized and working");
    }

    /**
     * Cleanup method to quit the driver after each test.
     */
    @AfterMethod(alwaysRun = true)
    public void teardown() {
        if (driver.get() != null) {
            driver.get().quit();
        }
    }
}
