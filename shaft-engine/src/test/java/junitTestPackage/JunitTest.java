package junitTestPackage;

import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import testPackage.TestPageServer;

public class JunitTest {
    private static final By TEXT_INPUT = By.id("my-text-id");
    private static final By PASSWORD = By.name("my-password");
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    void testMethod() {
        enterFormAndCaptureEvidence();
        driver.get().assertThat().browser().url().contains("smartWebFormFixture.html").perform();
    }

    @Test
    void testMethod2() {
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

    @BeforeEach
    void beforeEach() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL(TestPageServer.url("smartWebFormFixture.html"));
    }

    @AfterEach
    void afterEach() {
        if (driver.get() != null) {
            driver.get().quit();
        }
    }
}
