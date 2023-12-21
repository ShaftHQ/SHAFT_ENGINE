package testPackage;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import poms.GoogleSearch;

public class Test_verifyEquals {
    // Declaring webdriver instance
    WebDriver driver;

    @Test
    public void test_assertElementAttribute() {
        new ElementActions(driver).type(GoogleSearch.getSearchBox_textField(),
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        Validations.verifyThat().element(driver, GoogleSearch.getSearchBox_textField())
                .text()
                .matchesRegex("INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.")
                .perform();
    }

    @Test
    public void test_assertEquals() {
        new ElementActions(driver).type(GoogleSearch.getSearchBox_textField(),
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        String actualValue = new ElementActions(driver).getText(GoogleSearch.getSearchBox_textField());
        Validations.verifyThat()
                .object(actualValue)
                .matchesRegex("INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.")
                .perform();
    }

    @Test
    public void test_verifyElementAttribute() {
        new ElementActions(driver).type(GoogleSearch.getSearchBox_textField(),
                "© Copyright 2014-2017 Incorta, Inc Version: Rel3.3-dev Build May 29, 2018 15:30");
        Validations.verifyThat().element(driver, GoogleSearch.getSearchBox_textField())
                .text()
                .matchesRegex("([\\s\\S]*Rel3.3[\\s\\S]*)")
                .perform();
    }

    @BeforeMethod // Set-up method, to be run once before the first test
    public void beforeMethod() {
        driver = DriverFactory.getHelper().getDriver();
        new BrowserActions(driver).navigateToURL("https://www.google.com/ncr", "https://www.google.com/");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        new BrowserActions(driver).closeCurrentWindow();
    }
}
