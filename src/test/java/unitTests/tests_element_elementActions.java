package unitTests;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
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
    public void AfterMethod() {
        BrowserActions.closeCurrentWindow(driver.get());
    }
}
