package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import poms.GoogleSearch;

public class AssertEqualsTests {
    // Declaring webdriver instance
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void test_assertElementAttribute() {
        new ElementActions(driver.get().getDriver()).type(GoogleSearch.getSearchBox_textField(),
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        Validations.assertThat().element(driver.get().getDriver(), GoogleSearch.getSearchBox_textField()).text().matchesRegex("INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.").perform();
    }

    @Test
    public void test_assertEquals() {
        new ElementActions(driver.get().getDriver()).type(GoogleSearch.getSearchBox_textField(),
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        String actualValue = new ElementActions(driver.get().getDriver()).getText(GoogleSearch.getSearchBox_textField());
        Validations.assertThat().object(actualValue).matchesRegex("INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.").perform();
    }

    @Test
    public void test_verifyElementAttribute() {
        new ElementActions(driver.get().getDriver()).type(GoogleSearch.getSearchBox_textField(),
                "© Copyright 2014-2017 Incorta, Inc Version: Rel3.3-dev Build May 29, 2018 15:30");
        Validations.verifyThat().element(driver.get().getDriver(), GoogleSearch.getSearchBox_textField())
                .text()
                .matchesRegex("([\\s\\S]*Rel3.3[\\s\\S]*)")
                .perform();
    }

    @BeforeMethod // Set-up method, to be run once before the first test
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        new BrowserActions(driver.get().getDriver()).navigateToURL("https://www.google.com/ncr", "https://www.google.com/");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        new BrowserActions(driver.get().getDriver()).closeCurrentWindow();
    }
}
