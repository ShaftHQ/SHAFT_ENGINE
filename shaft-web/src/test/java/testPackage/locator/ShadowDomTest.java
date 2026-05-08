package testPackage.locator;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class ShadowDomTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    //    @Test
    public void wrappedNativeSelenium() {
        // https://github.com/ShaftHQ/SHAFT_ENGINE/issues/874
        // https://titusfortner.com/2021/11/22/shadow-dom-selenium.html?fbclid=IwAR1lYByXhUC8o7C81X89mIQVcSF-OIg8KWWs_eLIRvXzxkXqWKulmcsf810
        driver.get().browser().navigateToURL("http://watir.com/examples/shadow_dom.html");
        WebElement shadowContent = driver.get().getDriver().findElement(By.cssSelector("#shadow_host")).getShadowRoot().findElement(By.cssSelector("#shadow_content"));
        SHAFT.Validations.assertThat().object(shadowContent.getText()).isEqualTo("some text").perform();
    }

    @Test(enabled = false)
    public void shaftLocator() {
        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.CHROME.browserName())
                || SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.EDGE.browserName())) {
            driver.get().browser().navigateToURL("http://watir.com/examples/shadow_dom.html");
            By shadowDom = SHAFT.GUI.Locator.hasAnyTagName().hasId("shadow_host").build();
            By shadowElement = SHAFT.GUI.Locator.hasAnyTagName().hasId("shadow_content").insideShadowDom(shadowDom).build();
            driver.get().assertThat().element(shadowElement).text().isEqualTo("some text").perform();
        }
    }

    @BeforeMethod
    public void beforeMethod() {
        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.CHROME.browserName())
                || SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.EDGE.browserName())) {
            driver.set(new SHAFT.GUI.WebDriver());
        }
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.CHROME.browserName())
                || SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.EDGE.browserName())) {
            driver.get().quit();
        }
    }

    //@Test
    public void shaftLocator_2() {
        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.CHROME.browserName())
                || SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.EDGE.browserName())) {
            driver.get().browser().navigateToURL("https://usercentrics.com");
            By shadowDom = By.id("usercentrics-root");
            By shadowElement = SHAFT.GUI.Locator.hasTagName("button")
                    .hasAttribute("data-testid", "uc-accept-all-button")
                    .insideShadowDom(shadowDom).build();
            driver.get().element().click(shadowElement);
        }
    }
}