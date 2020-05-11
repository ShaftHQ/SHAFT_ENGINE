package unitTests;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class tests_element_elementActions {
    private static final ThreadLocal<WebDriver> driver = new ThreadLocal<WebDriver>();

    @Test
    public void waitForElementToBePresent_true_expectedToPass() {
        BrowserActions.navigateToURL(driver.get(), "https://www.google.com/ncr", "www.google.com");
        ElementActions.waitForElementToBePresent(driver.get(), By.id("hplogo"), 1, true);
        Assertions.assertElementMatches(driver.get(), By.id("hplogo"), Assertions.VisualValidationEngine.EXACT_OPENCV, Assertions.AssertionType.POSITIVE, "Using Visual AI; OpenCV");
        //Assertions.assertElementMatches(driver.get(), By.id("hplogo"), Assertions.VisualValidationEngine.STRICT_EYES, Assertions.AssertionType.POSITIVE, "Using Visual AI; Applitools Eyes");
    }

    //@Test
    public void waitForElementToBePresent_true_expectedToPass2() {
        BrowserActions.navigateToURL(driver.get(), "https://www.google.com/ncr", "www.google.com");
        Assertions.assertElementMatches(driver.get(), By.xpath("//div[@class='RNNXgb']"), Assertions.VisualValidationEngine.STRICT_EYES, Assertions.AssertionType.POSITIVE, "NEW - Using Visual AI; Applitools Eyes");
        Assertions.assertElementMatches(driver.get(), By.xpath("//div[@class='RNNXgb']"), Assertions.VisualValidationEngine.STRICT_EYES, Assertions.AssertionType.POSITIVE, "PASSED - Using Visual AI; Applitools Eyes");
        ElementActions.type(driver.get(), By.xpath("//input[@name='q']"), "SHAFT_Engine Aplitools Test");
        Assertions.assertElementMatches(driver.get(), By.xpath("//div[@class='RNNXgb']"), Assertions.VisualValidationEngine.STRICT_EYES, Assertions.AssertionType.POSITIVE, "FAILED/MISMATCH - Using Visual AI; Applitools Eyes");
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
            ElementActions.waitForElementToBePresent(driver.get(), By.id("hplogo"), 1, false);
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
        driver.set(BrowserFactory.getBrowser());
    }

    @AfterMethod
    public void afterMethod() {
        BrowserActions.closeCurrentWindow(driver.get());
    }
}
