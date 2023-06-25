package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;
import org.testng.annotations.*;
import poms.GoogleSearch;

public class ElementActionsTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private static final double defaultElementIdentificationTimeout = SHAFT.Properties.timeouts.defaultElementIdentificationTimeout();

    @Test
    public void waitForElementToBePresent_true_expectedToPass() {
        driver.get().browser().navigateToURL("https://www.google.com/ncr", "www.google.com");
        driver.get().element().waitToBeReady(GoogleSearch.googleLogo_image);
        driver.get().assertThat()
                .element(GoogleSearch.googleLogo_image)
                .matchesReferenceImage()
                .withCustomReportMessage("Using Visual AI; OpenCV")
                .perform();
    }

    //@Test
    public void waitForElementToBePresent_true_expectedToPass2() {
        driver.get().browser().navigateToURL("https://www.google.com/ncr", "www.google.com");

        driver.get().assertThat()
                .element(By.xpath("//div[@class='RNNXgb']"))
                .matchesReferenceImage(ValidationEnums.VisualValidationEngine.STRICT_EYES)
                .withCustomReportMessage("NEW - Using Visual AI; Applitools Eyes")
                .perform();
        driver.get().assertThat()
                .element(By.xpath("//div[@class='RNNXgb']"))
                .matchesReferenceImage(ValidationEnums.VisualValidationEngine.STRICT_EYES)
                .withCustomReportMessage("PASSED - Using Visual AI; Applitools Eyes")
                .perform();
        driver.get().element().type(By.xpath("//input[@name='q']"), "SHAFT_Engine Aplitools Test");
        driver.get().assertThat()
                .element(By.xpath("//div[@class='RNNXgb']"))
                .matchesReferenceImage(ValidationEnums.VisualValidationEngine.STRICT_EYES)
                .withCustomReportMessage("FAILED/MISMATCH - Using Visual AI; Applitools Eyes")
                .perform();
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void waitForElementToBePresent_true_expectedToFail() {
        driver.get().browser().navigateToURL("https://www.google.com/ncr", "www.google.com");
        driver.get().element().waitToBeReady(By.id("bla"));
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void waitForElementToBePresent_false_expectedToFail() {
        driver.get().browser().navigateToURL("https://www.google.com/ncr", "www.google.com");
        driver.get().element().waitToBeInvisible(GoogleSearch.googleLogo_image);
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void waitForElementToBePresent_moreThanOneElement_expectedToFail() {
        driver.get().browser().navigateToURL("https://www.google.com/ncr", "www.google.com");
        driver.get().element().waitToBeInvisible(By.xpath("//*"));
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }

    @BeforeClass
    public void beforeClass() {
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(2);
    }

    @AfterClass(alwaysRun = true)
    public void afterClass() {
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(defaultElementIdentificationTimeout);
    }
}
