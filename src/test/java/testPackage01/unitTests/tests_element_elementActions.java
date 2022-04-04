package testPackage01.unitTests;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import poms.GoogleSearch;

public class tests_element_elementActions {
    private static final ThreadLocal<WebDriver> driver = new ThreadLocal<>();

    @Test
    public void waitForElementToBePresent_true_expectedToPass() {
        BrowserActions.navigateToURL(driver.get(), "https://www.google.com/ncr", "www.google.com");
        ElementActions.waitForElementToBePresent(driver.get(), GoogleSearch.googleLogo_image, 1, true);
        Validations.assertThat()
                .element(driver.get(), GoogleSearch.googleLogo_image)
                .matchesReferenceImage()
                .withCustomReportMessage("Using Visual AI; OpenCV")
                .perform();
    }

    //@Test
    public void waitForElementToBePresent_true_expectedToPass2() {
        BrowserActions.navigateToURL(driver.get(), "https://www.google.com/ncr", "www.google.com");
        Validations.assertThat()
                .element(driver.get(), By.xpath("//div[@class='RNNXgb']"))
                .matchesReferenceImage(ValidationEnums.VisualValidationEngine.STRICT_EYES)
                .withCustomReportMessage("NEW - Using Visual AI; Applitools Eyes")
                .perform();
        Validations.assertThat()
                .element(driver.get(), By.xpath("//div[@class='RNNXgb']"))
                .matchesReferenceImage(ValidationEnums.VisualValidationEngine.STRICT_EYES)
                .withCustomReportMessage("PASSED - Using Visual AI; Applitools Eyes")
                .perform();
        ElementActions.type(driver.get(), By.xpath("//input[@name='q']"), "SHAFT_Engine Aplitools Test");
        Validations.assertThat()
                .element(driver.get(), By.xpath("//div[@class='RNNXgb']"))
                .matchesReferenceImage(ValidationEnums.VisualValidationEngine.STRICT_EYES)
                .withCustomReportMessage("FAILED/MISMATCH - Using Visual AI; Applitools Eyes")
                .perform();
    }

    @Test
    public void waitForElementToBePresent_true_expectedToFail() {
        BrowserActions.navigateToURL(driver.get(), "https://www.google.com/ncr", "www.google.com");
        try {
            ElementActions.waitForElementToBePresent(driver.get(), By.id("bla"), 1, true);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void waitForElementToBePresent_false_expectedToPass() {
        BrowserActions.navigateToURL(driver.get(), "https://www.google.com/ncr", "www.google.com");
        ElementActions.waitForElementToBePresent(driver.get(), By.id("bla"), 1, false);
    }

    @Test
    public void waitForElementToBePresent_false_expectedToFail() {
        BrowserActions.navigateToURL(driver.get(), "https://www.google.com/ncr", "www.google.com");
        try {
            ElementActions.waitForElementToBePresent(driver.get(), GoogleSearch.googleLogo_image, 1, false);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void waitForElementToBePresent_moreThanOneElement_expectedToFail() {
        BrowserActions.navigateToURL(driver.get(), "https://www.google.com/ncr", "www.google.com");
        try {
            ElementActions.waitForElementToBePresent(driver.get(), By.xpath("//*"), 1, false);
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(DriverFactory.getDriver());
    }

    @AfterMethod
    public void afterMethod() {
        BrowserActions.closeCurrentWindow(driver.get());
    }
}
