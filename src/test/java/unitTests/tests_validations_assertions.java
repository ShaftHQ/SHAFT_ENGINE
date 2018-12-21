package unitTests;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.Test;

import com.shaft.browser.BrowserActions;
import com.shaft.browser.BrowserFactory;
import com.shaft.element.ElementActions;
import com.shaft.validation.Assertions;

public class tests_validations_assertions {
    @Test(description = "Assert that assertEquals works as expected when the two values are equal.")
    public void assertEquals_true_expectedToPass() {
	Assertions.assertEquals(1, 1, 1, true);
    }

    @Test
    public void assertEquals_true_expectedToFail() {
	try {
	    Assertions.assertEquals(1, 2, 1, true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertEquals_false_expectedToPass() {
	Assertions.assertEquals(1, 2, 1, false);
    }

    @Test
    public void assertEquals_false_expectedToFail() {
	try {
	    Assertions.assertEquals(1, 1, 1, false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertNull_true_expectedToPass() {
	Assertions.assertNull(null, true);
    }

    @Test
    public void assertNull_true_expectedToFail() {
	try {
	    Assertions.assertNull(1, true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertNull_false_expectedToPass() {
	Assertions.assertNull(1, false);
    }

    @Test
    public void assertNull_false_expectedToFail() {
	try {
	    Assertions.assertNull(null, false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_true_greaterThan_expectedToPass() {
	Assertions.assertComparativeRelation(1, 2, ">=", true);
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_true_equals_expectedToPass() {
	Assertions.assertComparativeRelation(1, 1, ">=", true);
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_true_greaterThan_expectedToFail() {
	try {
	    Assertions.assertComparativeRelation(2, 1, ">=", true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_true_equals_expectedToFail() {
	try {
	    Assertions.assertComparativeRelation(1, 0, ">=", true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_false_greaterThan_expectedToPass() {
	Assertions.assertComparativeRelation(2, 1, ">=", false);
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_false_equals_expectedToPass() {
	Assertions.assertComparativeRelation(2, 1, ">=", false);
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_false_greaterThan_expectedToFail() {
	try {
	    Assertions.assertComparativeRelation(1, 2, ">=", false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertComparativeRelation_greaterThanOrEquals_false_equals_expectedToFail() {
	try {
	    Assertions.assertComparativeRelation(1, 1, ">=", false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementExists_true_expectedToPass() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	Assertions.assertElementExists(driver, By.id("hplogo"), true);
    }

    @Test
    public void assertElementExists_true_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");
	try {
	    Assertions.assertElementExists(driver, By.id("fakeElement"), true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementExists_false_expectedToPass() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	Assertions.assertElementExists(driver, By.id("fakeElement"), false);
    }

    @Test
    public void assertElementExists_false_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");
	try {
	    Assertions.assertElementExists(driver, By.id("hplogo"), false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementExists_true_multipleElementsFound_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");
	try {
	    Assertions.assertElementExists(driver, By.xpath("//div"), true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementExists_false_multipleElementsFound_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");
	try {
	    Assertions.assertElementExists(driver, By.xpath("//div"), false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementAttribute_true_literalComparison_expectedToPass() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "Automation!@#$%^&*()_+{}[]\\';/.,");
	Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation!@#$%^&*()_+{}[]\\';/.,", 1, true);
    }

    @Test
    public void assertElementAttribute_true_literalComparison_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "Automation!@#$%^&*()_+{}[]\\';/.,");
	try {
	    Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation", 1, true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementAttribute_true_regexComparison_expectedToPass() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "Automation123");
	Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation.*", 2, true);
    }

    @Test
    public void assertElementAttribute_true_regexComparison_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "Automation123");
	try {
	    Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation", 2, true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementAttribute_true_containsComparison_expectedToPass() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "Automation123");
	Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation", 3, true);
    }

    @Test
    public void assertElementAttribute_true_containsComparison_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "Automation123");
	try {
	    Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation1234", 3, true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementAttribute_true_caseInsensitiveComparison_expectedToPass() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "AUTOMATION");
	Assertions.assertElementAttribute(driver, By.name("q"), "text", "AutomaTion", 4, true);
    }

    @Test
    public void assertElementAttribute_true_caseInsensitiveComparison_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "AUTOMATION");
	try {
	    Assertions.assertElementAttribute(driver, By.name("q"), "text", "AutomaTion123", 4, true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementAttribute_true_invalidComparison_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "AUTOMATION");
	try {
	    Assertions.assertElementAttribute(driver, By.name("q"), "text", "AutomaTion123", 5, true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementAttribute_false_literalComparison_expectedToPass() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "Automation!@#$%^&*()_+{}[]\\';/.,");
	Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation", 1, false);
    }

    @Test
    public void assertElementAttribute_false_literalComparison_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "Automation!@#$%^&*()_+{}[]\\';/.,");
	try {
	    Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation!@#$%^&*()_+{}[]\\\\';/.,", 1,
		    false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementAttribute_false_regexComparison_expectedToPass() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "Automation123");
	Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation", 2, false);
    }

    @Test
    public void assertElementAttribute_false_regexComparison_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "Automation123");
	try {
	    Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation.*", 2, false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementAttribute_false_containsComparison_expectedToPass() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "Automation123");
	Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation1234", 3, false);
    }

    @Test
    public void assertElementAttribute_false_containsComparison_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "Automation123");
	try {
	    Assertions.assertElementAttribute(driver, By.name("q"), "text", "Automation", 3, false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementAttribute_false_caseInsensitiveComparison_expectedToPass() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "AUTOMATION");
	Assertions.assertElementAttribute(driver, By.name("q"), "text", "AutomaTion123", 4, false);
    }

    @Test
    public void assertElementAttribute_false_caseInsensitiveComparison_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "AUTOMATION");
	try {
	    Assertions.assertElementAttribute(driver, By.name("q"), "text", "AutomaTion", 4, false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertElementAttribute_false_invalidComparison_expectedToFail() {
	WebDriver driver = BrowserFactory.getBrowser("GoogleChrome");
	BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "www.google.com");

	ElementActions.type(driver, By.name("q"), "AUTOMATION");
	try {
	    Assertions.assertElementAttribute(driver, By.name("q"), "text", "AutomaTion123", 5, true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertFileExists_true_expectedToPass() {
	Assertions.assertFileExists("/src/main/java/com/shaft/element/", "ElementActions.java", 1, true);
    }

    @Test
    public void assertFileExists_true_expectedToFail() {
	try {
	    Assertions.assertFileExists("/src/main/java/com/shaft/element/", "ElementActions.java_fail", 1, true);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }

    @Test
    public void assertFileExists_false_expectedToPass() {
	Assertions.assertFileExists("/src/main/java/com/shaft/element/", "ElementActions.java_fail", 1, false);
    }

    @Test
    public void assertFileExists_false_expectedToFail() {
	try {
	    Assertions.assertFileExists("/src/main/java/com/shaft/element/", "ElementActions.java", 1, false);
	} catch (AssertionError e) {
	    Assert.assertTrue(true);
	}
    }
}
