package testPackage01;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.browser.BrowserActions;
import com.shaft.browser.BrowserFactory;
import com.shaft.element.ElementActions;
import com.shaft.validation.Verifications;

public class Test_verifyEquals {
    // Declaring webdriver instance
    WebDriver driver;

    @Test
    public void test_assertElementAttribute() {
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
	ElementActions.type(driver, By.name("q"),
		"INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
	Verifications.verifyElementAttribute(driver, By.name("q"), "text",
		"INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.", 2, true);
	Verifications.verifyElementAttribute(driver, By.name("q"), "text",
		"INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.aaaaaa", 2,
		true);
	Verifications.verifyElementAttribute(driver, By.name("q"), "text",
		"INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.", 2, true);
	Verifications.verifyElementAttribute(driver, By.name("q"), "text",
		"INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.aaaaaa", 2,
		true);
    }

    // @Test
    public void test_assertEquals() {
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
	ElementActions.type(driver, By.name("q"),
		"INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
	String actualValue = ElementActions.getText(driver, By.name("q"));
	Verifications.verifyEquals(
		"INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.",
		actualValue, 2, true);

    }

    // @Test
    public void test_verifyElementAttribute() {
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
	ElementActions.type(driver, By.name("q"),
		"© Copyright 2014-2017 Incorta, Inc Version: Rel3.3-dev Build May 29, 2018 15:30");
	Verifications.verifyElementAttribute(driver, By.name("q"), "text", "([\\s\\S]*Rel3.3[\\s\\S]*)", 2, true);
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
	driver = BrowserFactory.getBrowser();
    }
}
