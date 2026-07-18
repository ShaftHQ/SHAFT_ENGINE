package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class SmartLocatorsTests {
    SHAFT.GUI.WebDriver driver;

    @Test
    public void testMethod(){
        By usernameInput = SHAFT.GUI.Locator.inputField("Username");
        By passwordInput = SHAFT.GUI.Locator.inputField("Password");
        By loginButton = SHAFT.GUI.Locator.clickableField("Login");

        driver.browser().navigateToURL("https://www.saucedemo.com/")
                .and().element().type(usernameInput, "standard_user")
                        .type(passwordInput, "secret_sauce")
                        .click(loginButton)
                .and().assertThat(By.cssSelector(".title")).text().isEqualTo("Products");
    }

    @BeforeMethod
    public void beforeMethod(){
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod
    public void afterMethod(){
        driver.quit();
    }
}
