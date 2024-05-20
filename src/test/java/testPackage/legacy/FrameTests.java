package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.chrome.ChromeDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class FrameTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    By iframe_1 = By.xpath("//iframe[@title='SHAFT User Guide']");
    By nestedElement = By.className("hero__title");
    String testPage = """
             data:text/html,
             <html>
                 <div id="parentFrameDiv">Parent Frame
                 <iframe src="https://shafthq.github.io/" height="500" width="500" title="SHAFT User Guide" name="iframe Name"></iframe>
                 </div>
             </html>
            """;

    //@Test()
    public void switchToIframeAndAssertContent_native_fluent() {
        var nativeWebDriver = new ChromeDriver();
        nativeWebDriver.navigate().to(testPage);
        var actualText = nativeWebDriver.switchTo().frame(nativeWebDriver.findElement(iframe_1)).findElement(nestedElement).getText();
        Assert.assertEquals(actualText, "SHAFT User Guide");
    }

    //@Test()
    public void switchToIframeAndAssertContent_native_standard() {
        var nativeWebDriver = new ChromeDriver();
        nativeWebDriver.navigate().to(testPage);
        nativeWebDriver.switchTo().frame(nativeWebDriver.findElement(iframe_1));
        var actualText = nativeWebDriver.findElement(nestedElement).getText();
        Assert.assertEquals(actualText, "SHAFT User Guide");
    }

    @Test()
    public void switchToIframeAndAssertContent() {
        driver.get().browser().navigateToURL(testPage)
                .and().element().switchToIframe(iframe_1)
                .and().assertThat(nestedElement).text().isEqualTo("SHAFT: Unified Test Automation Engine").perform();
    }

    @Test()
    public void switchToIframeAndBackToDefaultContent() {
        driver.get().browser().navigateToURL(testPage);
        driver.get().element().switchToIframe(iframe_1);
        var currentFrame = driver.get().element().getCurrentFrame();
        driver.get().element().switchToDefaultContent();
        var currentFrameAfterSwitchingBackToParentFrame = driver.get().element().getCurrentFrame();
        SHAFT.Validations.assertThat().object(currentFrameAfterSwitchingBackToParentFrame).doesNotEqual(currentFrame).perform();
    }

    @Test()
    public void switchToIFrameAndTypeIntoElement() {
        driver.get().browser().navigateToURL("https://stripe-payments-demo.appspot.com/")
                .and().element().switchToIframe(By.xpath("//iframe[@title='Secure card payment input frame']"))
                .and().type(By.xpath("//input[@name='cardnumber']"), "1234")
                .and().assertThat(By.xpath("//input[@name='cardnumber']")).text().contains("1234").perform();
    }

    @BeforeMethod
    public void beforeClass() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterClass() {
        driver.get().quit();
    }
}