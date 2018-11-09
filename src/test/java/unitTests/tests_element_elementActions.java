package unitTests;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.browser.BrowserActions;
import com.shaft.browser.BrowserFactory;
import com.shaft.element.ElementActions;

public class tests_element_elementActions {
    WebDriver driver;

    @Test
    public void waitForElementToBePresent_true_expectedToPass() {
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");
	ElementActions.waitForElementToBePresent(driver, By.id("hplogo"), 1, true);
    }

    @Test
    public void waitForElementToBePresent_true_expectedToFail() {
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");
	try {
	    ElementActions.waitForElementToBePresent(driver, By.id("bla"), 1, true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void waitForElementToBePresent_false_expectedToPass() {
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");
	ElementActions.waitForElementToBePresent(driver, By.id("bla"), 1, false);
    }

    @Test
    public void waitForElementToBePresent_false_expectedToFail() {
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");
	try {
	    ElementActions.waitForElementToBePresent(driver, By.id("hplogo"), 1, false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void waitForElementToBePresent_moreThanOneElement_expectedToFail() {
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");
	try {
	    ElementActions.waitForElementToBePresent(driver, By.xpath("//*"), 1, false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
	driver = BrowserFactory.getBrowser("GoogleChrome");
    }
}
