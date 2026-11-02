package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class ChainableElementActionsTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeMethod
    public void beforeMethod() {
        SHAFT.Properties.flags.set().clearBeforeTypingMode("native");
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @Test
    public void chainElementActions() {
        driver.get().browser().navigateToURL("data:text/html;charset=utf-8,<html><body><input name='q'></body></html>");

        By searchBox = By.name("q");

        driver.get().element().type(searchBox, "chained type 1")
                .type(searchBox, "chained type 2")
                .typeAppend(searchBox, "345");

        driver.get().assertThat().element(searchBox).text().isEqualTo("chained type 2345").perform();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod(){
        if (driver.get() != null) {
            driver.get().quit();
        }
        driver.remove();
        Properties.clearForCurrentThread();
    }
}
