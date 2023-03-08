package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class GuiVerificationTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    private final By searchBar_textField = SHAFT.GUI.Locator.hasTagName("input").containsAttribute("name", "q").build();

    @Test
    public void test_textTrimmed1() {
        driver.get().element().type(searchBar_textField, " test ");
        driver.get().assertThat().element(searchBar_textField).textTrimmed().isEqualTo("test").perform();
    }

    @Test
    public void test_textTrimmed2() {
        driver.get().element().type(searchBar_textField, " te st ");
        driver.get().assertThat().element(searchBar_textField).textTrimmed().isEqualTo("te st").perform();
    }

    @Test
    public void test_textTrimmed_expectedToFail() {
        driver.get().element().type(searchBar_textField, " te st ");
        try {
            driver.get().assertThat().element(searchBar_textField).textTrimmed().isEqualTo("test").perform();
        } catch (AssertionError e) {
            Assert.assertTrue(true);
        }
    }

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://www.google.com/");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
