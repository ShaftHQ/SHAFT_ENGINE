package com.shaft.validation;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.validation.Validations.ValidationComparisonType;
import com.shaft.validation.Validations.ValidationType;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterGroups;
import org.testng.annotations.BeforeGroups;
import org.testng.annotations.Test;

class NewValidationsTests {
    WebDriver driver;

    @Test
    public void forceFail() {
        try {
            Validations.assertFail();
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void forceFail2() {
        try {
            Validations.assertFail("Custom Message");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f1() {
        String longString = "1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss";
        Validations.assertEquals(longString, longString, ValidationComparisonType.EQUALS, ValidationType.POSITIVE,
                "Checking valid user data");
    }

    @Test
    public void f2() throws Exception {
        try {
            Validations.assertEquals(1, 2, ValidationComparisonType.EQUALS, ValidationType.POSITIVE,
                    "Checking valid user data");
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f3() {
        Validations.assertEquals(1, 2, ValidationComparisonType.EQUALS, ValidationType.NEGATIVE);
    }

    @Test
    public void f3_1() {
        Validations.assertEquals(null, null, ValidationComparisonType.EQUALS, ValidationType.POSITIVE,
                "Checking valid user data");
    }

    @Test
    public void f4() {
        Validations.assertEquals("1", "123", ValidationComparisonType.CONTAINS, ValidationType.POSITIVE);
    }

    @Test
    public void f5() {
        Validations.assertNull(1, ValidationType.NEGATIVE);
    }

    @Test
    public void f6() throws Exception {
        try {
            Validations.assertNull("NULL", ValidationType.POSITIVE,
                    "Making sure that null string is not equal to null object");
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f6_1() throws Exception {
        try {
            Validations.assertNull(null, ValidationType.NEGATIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f6_2() throws Exception {
        try {
            Validations.assertNull("not null", ValidationType.POSITIVE,
                    "Making sure that null string is not equal to null object");
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f7() {
        Validations.assertNull(null, ValidationType.POSITIVE);
    }

    @Test(groups = {"browserBasedTests"})
    public void f8() {
        Validations.assertElementExists(driver, By.tagName("h1"), ValidationType.POSITIVE);

    }

    @Test(groups = {"browserBasedTests"})
    public void f9() throws Exception {
        try {
            Validations.assertElementExists(driver, By.tagName("h3"), ValidationType.POSITIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f10() throws Exception {
        try {
            Validations.assertElementExists(driver, By.tagName("div"), ValidationType.POSITIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f11() {
        Validations.assertElementExists(driver, By.tagName("h3"), ValidationType.NEGATIVE,
                "Checking that false tag doesn't exist");
    }

    @Test(groups = {"browserBasedTests"})
    public void f12() throws Exception {
        try {
            Validations.assertElementExists(driver, By.tagName("h1"), ValidationType.NEGATIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f13() throws Exception {
        try {
            Validations.assertElementExists(driver, By.tagName("div"), ValidationType.NEGATIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f14() {
        Validations.assertElementAttribute(driver, By.tagName("h1"), "text", "Welcome to the-internet",
                ValidationComparisonType.EQUALS, ValidationType.POSITIVE, "Asserting that the header text is correct");
    }

    @Test(groups = {"browserBasedTests"})
    public void f15() throws Exception {
        try {
            Validations.assertElementAttribute(driver, By.tagName("h1"), "text", "Welcome",
                    ValidationComparisonType.CONTAINS, ValidationType.NEGATIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f16() throws Exception {
        try {
            Validations.assertElementAttribute(driver, By.tagName("h3"), "text", "Welcome",
                    ValidationComparisonType.CONTAINS, ValidationType.NEGATIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f17() {
        Validations.assertElementAttribute(driver, By.tagName("h1"), "text2", "Welcome",
                ValidationComparisonType.CONTAINS, ValidationType.NEGATIVE);
    }

    @Test(groups = {"browserBasedTests"})
    public void f18() {
        Validations.assertElementAttribute(driver, By.tagName("h1"), "text", "no", ValidationComparisonType.CONTAINS,
                ValidationType.NEGATIVE);
    }

    @Test(groups = {"browserBasedTests"})
    public void f19() {
        Validations.assertElementAttribute(driver, By.tagName("h1"), "tagname", "h", ValidationComparisonType.CONTAINS,
                ValidationType.POSITIVE);
    }

    @Test(groups = {"browserBasedTests"})
    public void f20() {
        Validations.assertElementAttribute(driver, By.tagName("h1"), "size", "a", ValidationComparisonType.CONTAINS,
                ValidationType.NEGATIVE);
    }

    @Test(groups = {"browserBasedTests"})
    public void f21() throws Exception {
        try {
            Validations.assertElementAttribute(driver, By.tagName("h3"), "text", "Welcome",
                    ValidationComparisonType.CONTAINS, ValidationType.POSITIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f22() throws Exception {
        try {
            Validations.assertElementAttribute(driver, By.tagName("h1"), "text", "yyy",
                    ValidationComparisonType.CONTAINS, ValidationType.POSITIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @BeforeGroups(groups = {"browserBasedTests"})
    public void openBrowser() {
        driver = BrowserFactory.getBrowser();
        BrowserActions.navigateToURL(driver, "https://the-internet.herokuapp.com/");
    }

    @AfterGroups(groups = {"browserBasedTests"})
    public void closeBrowser() {
        BrowserFactory.closeAllDrivers();
    }
}
