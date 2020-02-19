package testPackage01;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.validation.Assertions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterGroups;
import org.testng.annotations.BeforeGroups;
import org.testng.annotations.Test;

public class Test_NewValidations {
    WebDriver driver;

    @Test
    public void forceFail() {
        try {
            Assertions.assertFail();
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void forceFail2() {
        try {
            Assertions.assertFail("Custom Message");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f1() {
        String longString = "1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss";
        Assertions.assertEquals(longString, longString, Assertions.AssertionComparisonType.EQUALS, Assertions.AssertionType.POSITIVE,
                "Checking valid user data");
    }

    @Test
    public void f2() throws Exception {
        try {
            Assertions.assertEquals(1, 2, Assertions.AssertionComparisonType.EQUALS, Assertions.AssertionType.POSITIVE,
                    "Checking valid user data");
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f3() {
        Assertions.assertEquals(1, 2, Assertions.AssertionComparisonType.EQUALS, Assertions.AssertionType.NEGATIVE);
    }

    @Test
    public void f3_1() {
        Assertions.assertEquals(null, null, Assertions.AssertionComparisonType.EQUALS, Assertions.AssertionType.POSITIVE,
                "Checking valid user data");
    }

    @Test
    public void f4() {
        Assertions.assertEquals("1", "123", Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.POSITIVE);
    }

    @Test
    public void f5() {
        Assertions.assertNull(1, Assertions.AssertionType.NEGATIVE);
    }

    @Test
    public void f6() throws Exception {
        try {
            Assertions.assertNull("NULL", Assertions.AssertionType.POSITIVE,
                    "Making sure that null string is not equal to null object");
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f6_1() throws Exception {
        try {
            Assertions.assertNull(null, Assertions.AssertionType.NEGATIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f6_2() throws Exception {
        try {
            Assertions.assertNull("not null", Assertions.AssertionType.POSITIVE,
                    "Making sure that null string is not equal to null object");
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f7() {
        Assertions.assertNull(null, Assertions.AssertionType.POSITIVE);
    }

    @Test(groups = {"browserBasedTests"})
    public void f8() {
        Assertions.assertElementExists(driver, By.tagName("h1"), Assertions.AssertionType.POSITIVE);

    }

    @Test(groups = {"browserBasedTests"})
    public void f9() throws Exception {
        try {
            Assertions.assertElementExists(driver, By.tagName("h3"), Assertions.AssertionType.POSITIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f10() throws Exception {
        try {
            Assertions.assertElementExists(driver, By.tagName("div"), Assertions.AssertionType.POSITIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f11() {
        Assertions.assertElementExists(driver, By.tagName("h3"), Assertions.AssertionType.NEGATIVE,
                "Checking that false tag doesn't exist");
    }

    @Test(groups = {"browserBasedTests"})
    public void f12() throws Exception {
        try {
            Assertions.assertElementExists(driver, By.tagName("h1"), Assertions.AssertionType.NEGATIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f13() throws Exception {
        try {
            Assertions.assertElementExists(driver, By.tagName("div"), Assertions.AssertionType.NEGATIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f14() {
        Assertions.assertElementAttribute(driver, By.tagName("h1"), "text", "Welcome to the-internet",
                Assertions.AssertionComparisonType.EQUALS, Assertions.AssertionType.POSITIVE, "Asserting that the header text is correct");
    }

    @Test(groups = {"browserBasedTests"})
    public void f15() throws Exception {
        try {
            Assertions.assertElementAttribute(driver, By.tagName("h1"), "text", "Welcome",
                    Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.NEGATIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f16() throws Exception {
        try {
            Assertions.assertElementAttribute(driver, By.tagName("h3"), "text", "Welcome",
                    Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.NEGATIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f17() {
        Assertions.assertElementAttribute(driver, By.tagName("h1"), "text2", "Welcome",
                Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.NEGATIVE);
    }

    @Test(groups = {"browserBasedTests"})
    public void f18() {
        Assertions.assertElementAttribute(driver, By.tagName("h1"), "text", "no", Assertions.AssertionComparisonType.CONTAINS,
                Assertions.AssertionType.NEGATIVE);
    }

    @Test(groups = {"browserBasedTests"})
    public void f19() {
        Assertions.assertElementAttribute(driver, By.tagName("h1"), "tagname", "h", Assertions.AssertionComparisonType.CONTAINS,
                Assertions.AssertionType.POSITIVE);
    }

    @Test(groups = {"browserBasedTests"})
    public void f20() {
        Assertions.assertElementAttribute(driver, By.tagName("h1"), "size", "a", Assertions.AssertionComparisonType.CONTAINS,
                Assertions.AssertionType.NEGATIVE);
    }

    @Test(groups = {"browserBasedTests"})
    public void f21() throws Exception {
        try {
            Assertions.assertElementAttribute(driver, By.tagName("h3"), "text", "Welcome",
                    Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.POSITIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f22() throws Exception {
        try {
            Assertions.assertElementAttribute(driver, By.tagName("h1"), "text", "yyy",
                    Assertions.AssertionComparisonType.CONTAINS, Assertions.AssertionType.POSITIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f23() {
        Assertions.assertComparativeRelation(1, 1, Assertions.ComparativeRelationType.GREATER_THAN_OR_EQUALS, "confirming that the actual is greater than or equal to the expected");
    }

    @Test
    public void f24() {
        Assertions.assertComparativeRelation(1, 10, Assertions.ComparativeRelationType.GREATER_THAN_OR_EQUALS);
    }

    @Test
    public void f25() {
        Assertions.assertComparativeRelation(1, 1, Assertions.ComparativeRelationType.EQUALS);
    }

    @Test
    public void f26() {
        Assertions.assertComparativeRelation(1, 5, Assertions.ComparativeRelationType.GREATER_THAN);
    }

    @Test
    public void f27() {
        Assertions.assertComparativeRelation(11, 5, Assertions.ComparativeRelationType.LESS_THAN);
    }

    @Test
    public void f28() {
        Assertions.assertComparativeRelation(11, 5, Assertions.ComparativeRelationType.LESS_THAN_OR_EQUALS);
    }

    @Test
    public void f29() {
        Assertions.assertComparativeRelation(11, 11, Assertions.ComparativeRelationType.LESS_THAN_OR_EQUALS);
    }

    @Test
    public void f30() throws Exception {
        try {
            Assertions.assertComparativeRelation(-9, 11, Assertions.ComparativeRelationType.LESS_THAN_OR_EQUALS);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f31() {
        Assertions.assertTrue(true, Assertions.AssertionType.POSITIVE);
    }

    @Test
    public void f32() {
        Assertions.assertTrue(false, Assertions.AssertionType.NEGATIVE);
    }

    @Test
    public void f33() throws Exception {
        try {
            Assertions.assertTrue(true, Assertions.AssertionType.NEGATIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f34() throws Exception {
        try {
            Assertions.assertTrue(false, Assertions.AssertionType.POSITIVE);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f35() {
        Assertions.assertFileExists("", "pom.xml");
    }

    @Test
    public void f36() {
        Assertions.assertFileExists("src/test/java/testPackage01/", "Test_NewValidations.java");
    }

    @Test
    public void f37() throws Exception {
        try {
            Assertions.assertFileExists("", "pom.xml", 5, Assertions.AssertionType.NEGATIVE, "confirming that the file does't exist after 5 retries");
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

