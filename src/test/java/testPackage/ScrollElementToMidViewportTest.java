package testpackage;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

@Test public class ScrollElementToMidViewportTest {
    ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    public void smoothScrollElementToMiddleOfViewport(){
        driver.get().browser().navigateToURL("https://www.w3schools.com/");
        driver.get().element().click(By.xpath("//h1[.=\"JavaScript\"]"));
    }

    @BeforeMethod public void beforeMethod(){
        SHAFT.Properties.web.set().targetBrowserName(Browser.CHROME.browserName());
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod void afterMethod(){
        driver.get().quit();
    }
}
