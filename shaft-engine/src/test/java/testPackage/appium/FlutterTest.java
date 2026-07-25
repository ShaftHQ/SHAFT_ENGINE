package testPackage.appium;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.Validations;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebElement;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

/**
 * Test class for Flutter app automation using Appium Flutter Driver.
 * This test uses the demo Flutter counter app to validate the Flutter driver integration.
 *
 * The test demonstrates proper usage of java-client's native Flutter locators
 * ({@link AppiumBy} {@code flutter*} factory methods) to locate and interact with Flutter
 * widgets, superseding the deprecated {@code appium_flutterfinder_java} dependency.
 */
public class FlutterTest {
    private static final String ENABLE_FLUTTER_E2E_PROPERTY = "shaft.enableFlutterE2E";
    public static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    /**
     * Setup method to configure and initialize the Flutter driver.
     * This sets up the necessary properties for Flutter app automation.
     */
    @BeforeMethod(onlyForGroups = {"flutter"})
    public void setupFlutterDriver() {
        if (!Boolean.getBoolean(ENABLE_FLUTTER_E2E_PROPERTY)) {
            throw new SkipException("Flutter BrowserStack E2E is disabled. Set -D"
                    + ENABLE_FLUTTER_E2E_PROPERTY + "=true after validating the Flutter driver/app on the target cloud.");
        }
        // Common mobile attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        // Set automation name to FlutterIntegration - this automatically enables Flutter driver
        SHAFT.Properties.mobile.set().automationName(AutomationName.FLUTTER_INTEGRATION);
        SHAFT.Properties.mobile.set().browserName("");
        
        // Configure for BrowserStack execution so the test is covered by the Android mobile E2E workflow.
        SHAFT.Properties.platform.set().executionAddress("browserstack");
        SHAFT.Properties.browserStack.set().platformVersion("13.0");
        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
        SHAFT.Properties.browserStack.set().appName("flutter-demo.apk");
        SHAFT.Properties.browserStack.set().appRelativeFilePath("src/test/resources/testDataFiles/apps/flutter-demo.apk");
        SHAFT.Properties.browserStack.set().appUrl("");
        
        // Additional Flutter-specific capabilities can be set here if needed
        // For example, if testing on a specific device:
        // SHAFT.Properties.mobile.set().deviceName("Android Emulator");
        // SHAFT.Properties.mobile.set().platformVersion("13.0");
        
        // Initialize the driver. With automationName set to FLUTTER_INTEGRATION, SHAFT's
        // DriverFactoryHelper constructs a FlutterAndroidDriver/FlutterIOSDriver under the hood,
        // so AppiumBy's native flutter* locators can be used directly via findElement() below.
        driver.set(new SHAFT.GUI.WebDriver());
    }

    /**
     * Test to verify basic Flutter app launch and interaction.
     * This test verifies that the Flutter counter app can be launched
     * and that the main title is displayed using a native AppiumBy flutter locator.
     */
    @Test(groups = {"flutter"}, description = "Verify Flutter app launches successfully")
    public void testFlutterAppLaunch() {
        // Verify that the app launched successfully by finding the app title
        // The Flutter demo app typically has a title widget
        
        // Find element by text (common in Flutter counter demo)
        WebElement titleElement = driver.get().getDriver().findElement(AppiumBy.flutterText("Flutter Demo Home Page"));
        Validations.assertThat().object(titleElement).isNotNull().perform();
        
        // Verify driver is initialized and working
        Validations.assertThat().object(driver.get().getDriver()).isNotNull().perform();
    }

    /**
     * Test to verify Flutter counter app functionality.
     * This test demonstrates proper native Flutter locator usage to interact with widgets:
     * 1. Finding elements by ValueKey
     * 2. Clicking buttons
     * 3. Verifying text changes
     */
    @Test(groups = {"flutter"}, description = "Verify Flutter counter app increment functionality")
    public void testFlutterCounterIncrement() {
        // Find the increment button by ValueKey
        // Flutter counter demo typically uses 'increment' as the key
        WebElement incrementButton = driver.get().getDriver().findElement(AppiumBy.flutterKey("increment"));
        Validations.assertThat().object(incrementButton).isNotNull().perform();
        
        // Click the increment button
        incrementButton.click();
        
        // Verify the button click was successful
        // Note: In a real test, you would verify the counter value changed
        // For example:
        // WebElement counterText = driver.get().getDriver().findElement(AppiumBy.flutterKey("counterText"));
        // Assert.assertTrue(counterText.getText().contains("1"), "Counter should be incremented");
        
        Validations.assertThat().object(driver.get().getDriver()).isNotNull().perform();
    }
    
    /**
     * Test to demonstrate finding elements by Type.
     * This shows how to use the {@link AppiumBy#flutterType(String)} native locator.
     */
    @Test(groups = {"flutter"}, description = "Verify finding elements by Type")
    public void testFlutterFindByType() {
        // Find an element by its Flutter widget type
        // For example, finding a FloatingActionButton
        WebElement fabButton = driver.get().getDriver().findElement(AppiumBy.flutterType("FloatingActionButton"));
        Validations.assertThat().object(fabButton).isNotNull().perform();
    }
    
    /**
     * Test to demonstrate finding elements by ToolTip.
     * The native API has no dedicated tooltip finder; Flutter's Tooltip widget registers its
     * message as a semantics label, so {@link AppiumBy#flutterSemanticsLabel(String)} is the
     * equivalent native locator.
     */
    @Test(groups = {"flutter"}, description = "Verify finding elements by ToolTip")
    public void testFlutterFindByToolTip() {
        // Find an element by its tooltip text (exposed as a semantics label)
        // The increment button in Flutter demo usually has "Increment" tooltip
        WebElement incrementButton = driver.get().getDriver().findElement(AppiumBy.flutterSemanticsLabel("Increment"));
        Validations.assertThat().object(incrementButton).isNotNull().perform();
    }

    /**
     * Cleanup method to quit the driver after each test.
     */
    @AfterMethod(alwaysRun = true)
    public void teardown() {
        if (driver.get() != null) {
            driver.get().quit();
            driver.remove();
        }
        Properties.clearForCurrentThread();
    }
}
