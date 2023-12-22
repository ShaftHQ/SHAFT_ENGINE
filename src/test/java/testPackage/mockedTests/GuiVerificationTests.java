package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class GuiVerificationTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private final By locator = SHAFT.GUI.Locator.hasTagName("input").build();
    private double defaultElementIdentificationTimeout;

    @Test
    public void test_textTrimmed1() {
        driver.get().element().type(locator, " test ");
        driver.get().assertThat().element(locator).textTrimmed().isEqualTo("test").perform();
    }

    @Test
    public void test_textTrimmed2() {
        driver.get().element().type(locator, " te st ");
        driver.get().assertThat().element(locator).textTrimmed().isEqualTo("te st").perform();
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void test_textTrimmed_expectedToFail() {
        driver.get().element().type(locator, " te st ");
        driver.get().assertThat().element(locator).textTrimmed().isEqualTo("test").perform();
    }

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() {
        SHAFT.Properties.web.set().headlessExecution(true);
        defaultElementIdentificationTimeout = SHAFT.Properties.timeouts.defaultElementIdentificationTimeout();
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(2);
        driver.set(new SHAFT.GUI.WebDriver());
        String testElement = "data:text/html,<input type=\"text\"><br><br>";
        driver.get().browser().navigateToURL(testElement);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(defaultElementIdentificationTimeout);
        driver.get().quit();
    }
}
