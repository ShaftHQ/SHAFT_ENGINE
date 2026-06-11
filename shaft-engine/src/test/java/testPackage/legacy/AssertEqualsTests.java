package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.Tests;

public class AssertEqualsTests extends Tests {
    private static final By SEARCH_BOX = By.name("q");

    @Test
    public void test_assertElementAttribute() {
        driver.get().element().type(SEARCH_BOX,
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        Validations.assertThat().element(driver.get().getDriver(), SEARCH_BOX).text().matchesRegex("INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.").perform();
    }

    @Test
    public void test_assertEquals() {
        driver.get().element().type(SEARCH_BOX,
                "INC_004010050:Another SCHEDULER with the same name [Duplicate Job Name] already exists.");
        String actualValue = driver.get().element().get().text(SEARCH_BOX);
        Validations.assertThat().object(actualValue).matchesRegex("INC_004010050:Another SCHEDULER with the same name \\[Duplicate Job Name\\] already exists.").perform();
    }

    @Test
    public void test_verifyElementAttribute() {
        driver.get().element().type(SEARCH_BOX,
                "© Copyright 2014-2017 Incorta, Inc Version: Rel3.3-dev Build May 29, 2018 15:30");
        Validations.verifyThat().element(driver.get().getDriver(), SEARCH_BOX)
                .text()
                .matchesRegex("([\\s\\S]*Rel3.3[\\s\\S]*)")
                .perform();
    }

    @BeforeMethod // Set-up method, to be run once before the first test
    public void beforeMethod() {
        driver.get().browser().navigateToURL(SHAFT.Properties.paths.testData() + "searchFixture.html");
    }
}
