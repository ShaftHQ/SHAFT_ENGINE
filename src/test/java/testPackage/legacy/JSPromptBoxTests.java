/*
package testPackage;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebElement;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class JSPromptBoxTests {
    private static final By JS_PromptAlert = By.xpath("//button[contains(text(),'Click for JS Prompt')]");
    private static final By JS_ResultText = By.xpath("//p[@id='result']");
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    public void clickUsingJS(By Element) {
        WebElement element = driver.get().getDriver().findElement(Element);
        JavascriptExecutor executor = (JavascriptExecutor) driver.get().getDriver();
        executor.executeScript("arguments[0].click();", element);
    }

    @BeforeClass
    public void navigateToJSAlertPage() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("http://the-internet.herokuapp.com/javascript_alerts");
    }

    @Test
    public void dismissAlert() {
        clickUsingJS(JS_PromptAlert);
        driver.alert().dismissAlert();

    }

    @Test(dependsOnMethods = "dismissAlert")
    public void assertOnCancelAlertResultText() {
        driver.get().assertThat().element(JS_ResultText).text().isEqualTo("You entered: null").perform();
    }

    @Test(dependsOnMethods = "assertOnCancelAlertResultText")
    public void getAlertText() {
        clickUsingJS(JS_PromptAlert);
        ReportManager.logDiscrete("Alert text is: [" + driver.alert().getAlertText() + "]");
        Validations.assertThat().object(driver.alert().getAlertText()).isEqualTo("I am a JS prompt").perform();
    }

    @Test(dependsOnMethods = "getAlertText")
    public void acceptPromptAlertWithoutTypingTextMessage() {
        clickUsingJS(JS_PromptAlert);
        driver.alert().acceptAlert();
    }

    @Test(dependsOnMethods = "acceptPromptAlertWithoutTypingTextMessage")
    public void assertOnConfirmPromptAlertWithoutTypingTextMessageResultText() {
        driver.get().assertThat().element(JS_ResultText).text().contains("You entered:").perform();
    }

    @Test(dependsOnMethods = "assertOnConfirmPromptAlertWithoutTypingTextMessageResultText")
    public void acceptPromptAlertWithTextMessage() {
        clickUsingJS(JS_PromptAlert);
        driver.alert().typeIntoPromptAlert("Prompt Alert text message");
        driver.alert().acceptAlert();
    }

    @Test(dependsOnMethods = "acceptPromptAlertWithTextMessage")
    public void assertOnConfirmPromptAlertWithTextMessageResultText() {
        driver.get().assertThat().element(JS_ResultText).text().isEqualTo("You entered: Prompt Alert text message").perform();
    }

    @AfterClass(alwaysRun = true)
    public void tearDownDriver() {
        driver.get().browser().closeCurrentWindow();
    }

}
*/
