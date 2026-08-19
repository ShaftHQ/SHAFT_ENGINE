package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class BigPageActionsTest {
    private static final String FORM_URL = "https://www.selenium.dev/selenium/web/web-form.html";
    private static final By TEXT_INPUT = By.id("my-text-id");
    private static final By PASSWORD = By.name("my-password");
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void virtualThreads_1_sequential() {
        enterFormAndCaptureEvidence();
        driver.get().assertThat().browser().url().contains("web-form.html").perform();
    }

    @Test
    public void bigTest_1_Sequential() {
        enterFormAndCaptureEvidence();
        driver.get().element().captureScreenshot(TEXT_INPUT)
                .and().browser().captureScreenshot();
        driver.get().assertThat().element(TEXT_INPUT).exists().perform();
    }

    private void enterFormAndCaptureEvidence() {
        driver.get().element().type(TEXT_INPUT, "SHAFT")
                .type(PASSWORD, "stable fixture")
                .captureScreenshot(PASSWORD)
                .and().browser().captureScreenshot();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL(FORM_URL);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (driver.get() != null) {
            driver.get().quit();
        }
    }
}
