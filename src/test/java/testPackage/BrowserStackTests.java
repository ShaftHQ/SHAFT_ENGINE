package testPackage;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

@Test
public class BrowserStackTests {
    SHAFT.GUI.WebDriver driver;

    public void basicWebBrowserInteractions() {
        var handle1 = driver.browser().navigateToURL("https://github.com/shafthq/SHAFT_ENGINE").getWindowHandle();
        var handles = driver.element().click(By.xpath("//h2/following-sibling::div//a[@title='https://shafthq.github.io/']")).and().browser().getWindowHandles();
        handles.remove(handle1);
        var handle2 = handles.getLast();
        driver.browser().switchToWindow(handle2)
                .and().element().click(SHAFT.GUI.Locator.hasTagName("a").containsText("Upgrade Now").isLast().build())
                .and().assertThat(By.tagName("h1")).text().isEqualTo("First Steps").perform();
    }

    @BeforeMethod
    void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod
    void afterMethod() {
        driver.quit();
    }
}
