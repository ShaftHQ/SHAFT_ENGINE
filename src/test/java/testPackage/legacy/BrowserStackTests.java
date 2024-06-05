package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

@Test
public class BrowserStackTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    public void basicWebBrowserInteractions() {
        var handle1 = driver.get().browser().navigateToURL("https://github.com/shafthq/SHAFT_ENGINE").getWindowHandle();
        var handles = driver.get().element().click(By.xpath("//h2/following-sibling::div//a[@title='https://shafthq.github.io/']")).and().browser().getWindowHandles();
        handles.remove(handle1);
        var handle2 = handles.getLast();
        driver.get().browser().switchToWindow(handle2)
                .and().element().click(SHAFT.GUI.Locator.hasTagName("a").containsText("Upgrade Now").isLast().build())
                .and().assertThat(By.tagName("h1")).text().isEqualTo("About SHAFT").perform();
    }

    @BeforeMethod
    void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    void afterMethod() {
        driver.get().quit();
    }
}
