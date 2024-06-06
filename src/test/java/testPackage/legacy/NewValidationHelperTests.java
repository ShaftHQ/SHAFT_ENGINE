package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class NewValidationHelperTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void forceFail() {
        try {
            Validations.assertThat().forceFail().perform();
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void forceFail2() {
        try {
            Validations.assertThat().forceFail().withCustomReportMessage("Custom Message").perform();
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f1() {
        String longString = "1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss1dassssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss";
        Validations.assertThat().object(longString).isEqualTo(longString).withCustomReportMessage("Checking valid user data").perform();
    }

    @Test
    public void f2() throws Exception {
        try {
            Validations.assertThat().number(2).equals(1);
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f3() {
        Validations.assertThat().number(2).doesNotEqual(1).perform();
    }

    @Test
    public void f3_1() {
        Validations.assertThat().object(null).equals(null);
    }

    @Test
    public void f4() {
        Validations.assertThat().object("123").contains("1").perform();
    }

    @Test
    public void f5() {
        Validations.assertThat().object(1).isNotNull().perform();
    }

    @Test
    public void f6() throws Exception {
        try {
            Validations.assertThat().object("NULL").isNull()
                    .withCustomReportMessage("Making sure that null string is not equal to null object").perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f6_1() throws Exception {
        try {
            Validations.assertThat().object(null).isNotNull().perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f6_2() throws Exception {
        try {
            Validations.assertThat().object("not null").isNull()
                    .withCustomReportMessage("Making sure that null string is not equal to null object").perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f7() {
        Validations.assertThat().object(null).isNull();
    }

    @Test(groups = {"browserBasedTests"})
    public void f8() {
        driver.get().element().assertThat(By.tagName("h1")).exists().perform();
    }

    @Test(groups = {"browserBasedTests"})
    public void f9() throws Exception {
        try {
            driver.get().element().assertThat(By.tagName("h3")).exists().perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f10() throws Exception {
        try {
            driver.get().element().assertThat(By.tagName("div")).exists().perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f11() {
        driver.get().element().assertThat(By.tagName("h3")).doesNotExist()
                .withCustomReportMessage("Checking that false tag doesn't exist").perform();
    }

    @Test(groups = {"browserBasedTests"})
    public void f12() throws Exception {
        try {
            driver.get().element().assertThat(By.tagName("h1")).doesNotExist().perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f13() throws Exception {
        try {
            driver.get().element().assertThat(By.tagName("div")).doesNotExist().perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f14() {
        driver.get().element().assertThat(By.tagName("h1")).text().isEqualTo("Welcome to the-internet")
                .withCustomReportMessage("Asserting that the header text is correct").perform();
    }

    @Test(groups = {"browserBasedTests"})
    public void f15() throws Exception {
        try {
            driver.get().element().assertThat(By.tagName("h1")).text().doesNotContain("Welcome").perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f16() throws Exception {
        try {
            driver.get().element().assertThat(By.tagName("h3")).text().doesNotContain("Welcome").perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f17() {
        driver.get().element().assertThat(By.tagName("h1")).attribute("text2").doesNotContain("Welcome").perform();
    }

    @Test(groups = {"browserBasedTests"})
    public void f18() {
        driver.get().element().assertThat(By.tagName("h1")).attribute("text").doesNotContain("no").perform();
    }

    @Test(groups = {"browserBasedTests"})
    public void f20() {
        driver.get().element().assertThat(By.tagName("h1")).attribute("size").doesNotContain("a").perform();
    }

    @Test(groups = {"browserBasedTests"})
    public void f21() throws Exception {
        try {
            driver.get().element().assertThat(By.tagName("h3")).attribute("text").contains("Welcome").perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test(groups = {"browserBasedTests"})
    public void f22() throws Exception {
        try {
            driver.get().element().assertThat(By.tagName("h1")).attribute("text").contains("yyy").perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f23() {
        Validations.assertThat().number(1).isGreaterThanOrEquals(1).withCustomReportMessage("confirming that the actual is greater than or equal to the expected").perform();
    }

    @Test
    public void f24() {
        Validations.assertThat().number(10).isGreaterThanOrEquals(1).perform();
    }

    @Test
    public void f25() {
        Validations.assertThat().number(1).equals(1);
    }

    @Test
    public void f26() {
        Validations.assertThat().number(5).isGreaterThan(1).perform();
    }

    @Test
    public void f27() {
        Validations.assertThat().number(5).isLessThan(11).perform();
    }

    @Test
    public void f28() {
        Validations.assertThat().number(5).isLessThanOrEquals(11).perform();
    }

    @Test
    public void f29() {
        Validations.assertThat().number(11).isLessThanOrEquals(11).perform();
    }

    @Test
    public void f30() throws Exception {
        try {
            Validations.assertThat().number(11).isLessThanOrEquals(-9).perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f31() {
        Validations.assertThat().object(true).isTrue().perform();
    }

    @Test
    public void f32() {
        Validations.assertThat().object(false).isFalse().perform();
    }

    @Test
    public void f33() throws Exception {
        try {
            Validations.assertThat().object(true).isFalse().perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f34() throws Exception {
        try {
            Validations.assertThat().object(false).isTrue().perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @Test
    public void f35() {
        Validations.assertThat().file("", "pom.xml").exists().perform();
    }

    @Test
    public void f37() throws Exception {
        try {
            Validations.assertThat().file("", "pom.xml").doesNotExist().perform();
            throw new Exception("Expected to fail but passed.");
        } catch (AssertionError e) {
            // pass
        }
    }

    @BeforeMethod(onlyForGroups = {"browserBasedTests"})
    public void openBrowser() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://the-internet.herokuapp.com/");
    }

    @AfterMethod(onlyForGroups = {"browserBasedTests"}, alwaysRun = true)
    public void closeBrowser() {
        driver.get().quit();
    }
}

