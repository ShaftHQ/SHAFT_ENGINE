package testPackage01;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Verifications;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_verifyEquals {
    // Declaring webdriver instance
    WebDriver driver;

    @Test
    public void test_assertElementAttribute() {
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
        ElementActions.type(driver, By.name("q"),
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        Verifications.verifyElementAttribute(driver, By.name("q"), "text",
                "INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.", Verifications.VerificationComparisonType.MATCHES, Verifications.VerificationType.POSITIVE);
        Verifications.verifyElementAttribute(driver, By.name("q"), "text",
                "INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.aaaaaa", Verifications.VerificationComparisonType.MATCHES,
                Verifications.VerificationType.POSITIVE);
        Verifications.verifyElementAttribute(driver, By.name("q"), "text",
                "INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.", Verifications.VerificationComparisonType.MATCHES, Verifications.VerificationType.POSITIVE);
        Verifications.verifyElementAttribute(driver, By.name("q"), "text",
                "INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.aaaaaa", Verifications.VerificationComparisonType.MATCHES,
                Verifications.VerificationType.POSITIVE);
    }

    // @Test
    public void test_assertEquals() {
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
        ElementActions.type(driver, By.name("q"),
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        String actualValue = ElementActions.getText(driver, By.name("q"));
        Verifications.verifyEquals(
                "INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.",
                actualValue, Verifications.VerificationComparisonType.MATCHES, Verifications.VerificationType.POSITIVE);

    }

    // @Test
    public void test_verifyElementAttribute() {
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
        ElementActions.type(driver, By.name("q"),
                "© Copyright 2014-2017 Incorta, Inc Version: Rel3.3-dev Build May 29, 2018 15:30");
        Verifications.verifyElementAttribute(driver, By.name("q"), "text", "([\\s\\S]*Rel3.3[\\s\\S]*)", Verifications.VerificationComparisonType.MATCHES, Verifications.VerificationType.POSITIVE);
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
        driver = BrowserFactory.getBrowser();
    }
}
