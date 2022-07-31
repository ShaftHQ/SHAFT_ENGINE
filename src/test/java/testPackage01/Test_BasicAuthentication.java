package testPackage01;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_BasicAuthentication {
    SHAFT.GUI.WebDriver driver;

    @Test
    public void basicAuthentication_traditional(){
        driver.browser().navigateToURL("https://user:pass@authenticationtest.com/HTTPAuth/", "https://authenticationtest.com/loginSuccess/");
        driver.assertThat().element(By.tagName("h1")).text().equals("Login Success");
    }

    @Test
    public void basicAuthentication_webdriverBiDi(){
        driver.browser().navigateToURLWithBasicAuthentication("https://authenticationtest.com/HTTPAuth/", "user", "pass", "https://authenticationtest.com/loginSuccess/");
        driver.assertThat().element(By.tagName("h1")).text().equals("Login Success");
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
