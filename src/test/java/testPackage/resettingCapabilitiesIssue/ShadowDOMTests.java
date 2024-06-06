package testPackage.resettingCapabilitiesIssue;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class ShadowDOMTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void shadowDom() {
        String targetText = "Typing into SHADOW DOM...";
        driver.get().browser().navigateToURL("https://mdn.github.io/web-components-examples/popup-info-box-web-component/");
        driver.get().element().type(By.id("cvc"), targetText);
        driver.get().element().assertThat(By.id("cvc")).text().isEqualTo(targetText).perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
