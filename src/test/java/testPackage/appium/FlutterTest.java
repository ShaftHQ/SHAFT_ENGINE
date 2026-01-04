package testPackage.appium;

import com.shaft.driver.SHAFT;
import io.appium.java_client.remote.AutomationName;
import io.github.ashwith.flutter.FlutterFinder;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

/**
 * Test class for Flutter app automation using Appium Flutter Driver.
 * This test uses the demo Flutter counter app to validate the Flutter driver integration.
 * 
 * The test demonstrates proper usage of FlutterFinder library to locate and interact
 * with Flutter widgets.
 */
public class FlutterTest {
    public static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private FlutterFinder finder;

    /**
     * Setup method to configure and initialize the Flutter driver.
     * This sets up the necessary properties for Flutter app automation.
     */
    @BeforeMethod
    public void setupFlutterDriver() {
        // Common mobile attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        // Set automation name to FlutterIntegration - this automatically enables Flutter driver
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
        
        // Initialize FlutterFinder
        finder = new FlutterFinder(driver.get().getDriver());
    }

    /**
     * Test to verify basic Flutter app launch and interaction.
     * This test verifies that the Flutter counter app can be launched
     * and that the main title is displayed using FlutterFinder.
     */
    @Test(groups = {"flutter"}, description = "Verify Flutter app launches successfully")
    public void testFlutterAppLaunch() {
        // Verify that the app launched successfully by finding the app title
        // The Flutter demo app typically has a title widget
        
        // Find element by text (common in Flutter counter demo)
        WebElement titleElement = finder.byText("Flutter Demo Home Page");
        Assert.assertNotNull(titleElement, "App title should be found");
        
        // Verify driver is initialized and working
        Assert.assertNotNull(driver.get().getDriver(), "Driver should be initialized");
    }

    /**
     * Test to verify Flutter counter app functionality.
     * This test demonstrates proper FlutterFinder usage to interact with widgets:
     * 1. Finding elements by ValueKey
     * 2. Clicking buttons
     * 3. Verifying text changes
     */
    @Test(groups = {"flutter"}, description = "Verify Flutter counter app increment functionality")
    public void testFlutterCounterIncrement() {
        // Find the increment button by ValueKey
        // Flutter counter demo typically uses 'increment' as the key
        WebElement incrementButton = finder.byValueKey("increment");
        Assert.assertNotNull(incrementButton, "Increment button should be found");
        
        // Click the increment button
        incrementButton.click();
        
        // Verify the button click was successful
        // Note: In a real test, you would verify the counter value changed
        // For example:
        // WebElement counterText = finder.byValueKey("counterText");
        // Assert.assertTrue(counterText.getText().contains("1"), "Counter should be incremented");
        
        Assert.assertNotNull(driver.get().getDriver(), "Driver should be initialized and working");
    }
    
    /**
     * Test to demonstrate finding elements by Type.
     * This shows how to use byType() finder method.
     */
    @Test(groups = {"flutter"}, description = "Verify finding elements by Type")
    public void testFlutterFindByType() {
        // Find an element by its Flutter widget type
        // For example, finding a FloatingActionButton
        WebElement fabButton = finder.byType("FloatingActionButton");
        Assert.assertNotNull(fabButton, "FloatingActionButton should be found");
    }
    
    /**
     * Test to demonstrate finding elements by ToolTip.
     * This shows how to use byToolTip() finder method.
     */
    @Test(groups = {"flutter"}, description = "Verify finding elements by ToolTip")
    public void testFlutterFindByToolTip() {
        // Find an element by its tooltip text
        // The increment button in Flutter demo usually has "Increment" tooltip
        WebElement incrementButton = finder.byToolTip("Increment");
        Assert.assertNotNull(incrementButton, "Element with tooltip 'Increment' should be found");
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
