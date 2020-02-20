package testPackage01;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionComparisonType;
import com.shaft.validation.Assertions.AssertionType;
import com.shaft.validation.Verifications;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_assertEquals {
    // Declaring webdriver instance
    WebDriver driver;

    @Test
    public void test_assertElementAttribute() {
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr");
        ElementActions.type(driver, By.id("lst-ib"),
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        Assertions.assertElementAttribute(driver, By.id("lst-ib"), "text",
                "INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.",
                AssertionComparisonType.MATCHES, AssertionType.POSITIVE);
    }

    // @Test
    public void test_assertEquals() {
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr");
        ElementActions.type(driver, By.id("lst-ib"),
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        String actualValue = ElementActions.getText(driver, By.id("lst-ib"));
        Assertions.assertEquals(
                "INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.",
                actualValue, AssertionComparisonType.MATCHES, AssertionType.POSITIVE);

    }

    // @Test
    public void test_verifyElementAttribute() {
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr");
        ElementActions.type(driver, By.id("lst-ib"),
                "© Copyright 2014-2017 Incorta, Inc Version: Rel3.3-dev Build May 29, 2018 15:30");
        Verifications.verifyElementAttribute(driver, By.id("lst-ib"), "text", "([\\s\\S]*Rel3.3[\\s\\S]*)", 2, true);
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
        driver = BrowserFactory.getBrowser();
    }
}
