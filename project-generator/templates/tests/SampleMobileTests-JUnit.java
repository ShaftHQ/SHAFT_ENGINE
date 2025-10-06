package {{PACKAGE_NAME}};

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;

public class SampleMobileTests {
    private SHAFT.GUI.WebDriver driver;

    // Sample locators for a demo mobile app
    By loginButton = By.id("com.example.app:id/loginButton");
    By usernameField = By.id("com.example.app:id/username");
    By passwordField = By.id("com.example.app:id/password");
    By submitButton = By.id("com.example.app:id/submit");
    By welcomeMessage = By.id("com.example.app:id/welcomeText");

    @BeforeAll
    public static void beforeAll() {
        // Configure mobile properties before tests
        // For local execution with Appium
        // SHAFT.Properties.platform.set().executionAddress("local");
        // SHAFT.Properties.mobile.set().app("path/to/your/app.apk");
        
        // For BrowserStack execution
        // SHAFT.Properties.platform.set().executionAddress("browserstack");
        // SHAFT.Properties.browserStack.set().platformVersion("13.0");
        // SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
        // SHAFT.Properties.browserStack.set().appName("YourApp.apk");
    }

    @BeforeEach
    public void beforeEach() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @Test
    @DisplayName("Sample mobile test - Navigate and verify element is displayed")
    public void testMobileAppNavigation() {
        // This is a sample test structure
        // Replace with your actual app elements and test logic
        driver.element().click(loginButton);
        driver.assertThat(usernameField).exists();
    }

    @Test
    @DisplayName("Sample mobile test - Perform login action")
    public void testMobileAppLogin() {
        // This is a sample test structure
        // Replace with your actual app elements and test logic
        driver.element().type(usernameField, "testuser")
                .and().element().type(passwordField, "testpassword")
                .and().element().click(submitButton);
        
        // Verify login success
        driver.assertThat(welcomeMessage).exists();
    }

    @AfterEach
    public void afterEach() {
        driver.quit();
    }

    @AfterAll
    public static void afterAll() {
        // Optional: Cleanup actions that run once after all tests
    }
}
