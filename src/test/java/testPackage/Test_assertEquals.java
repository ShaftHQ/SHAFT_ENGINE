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

public class Test_assertEquals {
    // Declaring webdriver instance
    WebDriver driver;

    @Test
    public void test_assertElementAttribute() {
        ElementActions.type(driver, GoogleSearch.getSearchBox_textField(),
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        Validations.assertThat().element(driver, GoogleSearch.getSearchBox_textField()).text().matchesRegex("INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.").perform();
    }

    @Test
    public void test_assertEquals() {
        ElementActions.type(driver, GoogleSearch.getSearchBox_textField(),
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        String actualValue = ElementActions.getText(driver, GoogleSearch.getSearchBox_textField());
        Validations.assertThat().object(actualValue).matchesRegex("INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.").perform();
    }

    @Test
    public void test_verifyElementAttribute() {
        ElementActions.type(driver, GoogleSearch.getSearchBox_textField(),
                "© Copyright 2014-2017 Incorta, Inc Version: Rel3.3-dev Build May 29, 2018 15:30");
        Validations.verifyThat().element(driver, GoogleSearch.getSearchBox_textField())
                .text()
                .matchesRegex("([\\s\\S]*Rel3.3[\\s\\S]*)")
                .perform();
    }

    @BeforeMethod // Set-up method, to be run once before the first test
    public void beforeMethod() {
        driver = DriverFactory.getDriver();
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com/");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        BrowserActions.closeCurrentWindow(driver);
    }
}
