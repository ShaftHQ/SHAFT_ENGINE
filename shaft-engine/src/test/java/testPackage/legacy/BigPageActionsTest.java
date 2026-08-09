package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class BigPageActionsTest {
    private static final By USERNAME = By.id("user-name");
    private static final By PASSWORD = By.id("password");
    private static final By LOGIN = By.id("login-button");
    private static final By INVENTORY = By.id("inventory_container");
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void virtualThreads_1_sequential() {
        loginAndCaptureEvidence();
        driver.get().assertThat().browser().url().contains("inventory.html").perform();
    }

    @Test
    public void bigTest_1_Sequential() {
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

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://www.saucedemo.com/");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (driver.get() != null) {
            driver.get().quit();
        }
    }
}
