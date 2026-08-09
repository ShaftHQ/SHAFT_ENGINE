package junitTestPackage;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;

public class JunitTest {
    private static final By USERNAME = By.id("user-name");
    private static final By PASSWORD = By.id("password");
    private static final By LOGIN = By.id("login-button");
    private static final By INVENTORY = By.id("inventory_container");
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    void testMethod() {
        loginAndCaptureEvidence();
        driver.get().assertThat().url().contains("inventory.html").perform();
    }

    @Test
    void testMethod2() {
        loginAndCaptureEvidence();
        driver.get().element().captureScreenshot(INVENTORY)
                .and().browser().captureScreenshot();
        driver.get().assertThat().element(INVENTORY).exists().perform();
    }

    private void loginAndCaptureEvidence() {
        driver.get().element().type(USERNAME, "standard_user")
                .type(PASSWORD, "secret_sauce")
                .captureScreenshot(PASSWORD)
                .and().browser().captureScreenshot();
        driver.get().element().click(LOGIN);
    }

    @BeforeEach
    void beforeEach() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://www.saucedemo.com/");
    }

    @AfterEach
    void afterEach() {
        if (driver.get() != null) {
            driver.get().quit();
        }
    }
}
