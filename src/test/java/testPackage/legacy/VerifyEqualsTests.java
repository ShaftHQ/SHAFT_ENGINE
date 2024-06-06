package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import poms.GoogleSearch;

public class VerifyEqualsTests {
    // Declaring webdriver instance
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void test_assertElementAttribute() {
        driver.get().element().type(GoogleSearch.getSearchBox_textField(),
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        Validations.verifyThat().element(driver.get().getDriver(), GoogleSearch.getSearchBox_textField())
                .text()
                .matchesRegex("INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.")
                .perform();
    }

    @Test
    public void test_assertEquals() {
        driver.get().element().type(GoogleSearch.getSearchBox_textField(),
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        String actualValue = driver.get().element().getText(GoogleSearch.getSearchBox_textField());
        Validations.verifyThat()
                .object(actualValue)
                .matchesRegex("INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.")
                .perform();
    }

    @Test
    public void test_verifyElementAttribute() {
        driver.get().element().type(GoogleSearch.getSearchBox_textField(),
                "© Copyright 2014-2017 Incorta, Inc Version: Rel3.3-dev Build May 29, 2018 15:30");
        Validations.verifyThat().element(driver.get().getDriver(), GoogleSearch.getSearchBox_textField())
                .text()
                .matchesRegex("([\\s\\S]*Rel3.3[\\s\\S]*)")
                .perform();
    }

    @BeforeMethod // Set-up method, to be run once before the first test
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://www.google.com/ncr", "https://www.google.com/");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
