package testPackage;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;

public class MultipleElementsFailureTest {
    SHAFT.GUI.WebDriver driver;

    //@Test
    public void test_ClickUsingJavaScript() {
        driver.browser().navigateToURL("https://www.saucedemo.com");
        driver.element().type(By.xpath("//input"), "standard_user");
    }

    //@BeforeMethod
    void beforeMethod() {
        driver = SHAFT.GUI.WebDriver.getInstance();
    }

    //@AfterMethod
    void afterMethod() {
        driver.quit();
    }
}
