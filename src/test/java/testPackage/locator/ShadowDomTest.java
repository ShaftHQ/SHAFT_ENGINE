package testPackage.locator;

import com.shaft.driver.SHAFT;
import io.github.shafthq.shaft.gui.locator.LocatorBuilder;
import io.github.shafthq.shaft.gui.locator.Locators;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class ShadowDomTest {
    SHAFT.GUI.WebDriver driver;

    //    @Test
    public void wrappedNativeSelenium() {
        // https://github.com/ShaftHQ/SHAFT_ENGINE/issues/874
        // https://titusfortner.com/2021/11/22/shadow-dom-selenium.html?fbclid=IwAR1lYByXhUC8o7C81X89mIQVcSF-OIg8KWWs_eLIRvXzxkXqWKulmcsf810
        driver.browser().navigateToURL("http://watir.com/examples/shadow_dom.html");
        WebElement shadowContent = driver.getDriver().findElement(By.cssSelector("#shadow_host")).getShadowRoot().findElement(By.cssSelector("#shadow_content"));
        SHAFT.Validations.assertThat().object(shadowContent.getText()).isEqualTo("some text").perform();
    }

    @Test
    public void shaftLocator() {
        if (!SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.FIREFOX.browserName())) {
            driver.browser().navigateToURL("http://watir.com/examples/shadow_dom.html");
            By shadowDom = SHAFT.GUI.Locator.hasAnyTagName().hasId("shadow_host").build();
            By shadowElement = SHAFT.GUI.Locator.hasAnyTagName().hasId("shadow_content").insideShadowDom(shadowDom).build();
            driver.assertThat().element(shadowElement).text().isEqualTo("some text").perform();
        }
    }

    @BeforeMethod
    public void beforeMethod() {
        if (!SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.FIREFOX.browserName())) {
            LocatorBuilder.setMode(Locators.CSS);
            driver = new SHAFT.GUI.WebDriver();
        }
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (!SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.FIREFOX.browserName())) {
            LocatorBuilder.setMode(Locators.XPATH);
            driver.quit();
        }
    }
}