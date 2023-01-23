/*
package testPackage01.appium;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.*;

import java.time.Duration;

public class Test_BasicActionsOnBrowserStack {

    SHAFT.GUI.WebDriver driver;
    SHAFT.TestData.JSON testData;

    By accessibilityNodeProvider = AppiumBy.accessibilityId("Accessibility Node Provider");
    By accessibility = AppiumBy.accessibilityId("Access'ibility");
    By app = AppiumBy.accessibilityId("App");
    By activity = AppiumBy.accessibilityId("Activity");
    By customTitle = AppiumBy.accessibilityId("Custom Title");
    By changeLeft = AppiumBy.accessibilityId("Change Left");
    By leftIsBest = AppiumBy.xpath("//android.widget.TextView[@content-desc=\"Left is best\"]");
    By LeftTopTitle_text = By.id("io.appium.android.apis:id/left_text_edit");



    @Test
    public void testTaping() {
        driver.element().click(accessibility);
        driver.assertThat().element(accessibilityNodeProvider).text().contains("Accessibility Node Provider").perform();
    }
    @Test()
    public void testTyping() {
        driver.element().click(app);
        driver.element().click(activity);
        driver.element().click(customTitle);
        driver.element().type(LeftTopTitle_text,"Appium");
        driver.element().click(changeLeft);
        driver.assertThat().element(leftIsBest).text().contains("Appium").perform();
    }

    @BeforeMethod
    public void beforeClass() {
        driver = new SHAFT.GUI.WebDriver();
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @AfterMethod
    public void afterClass() {
        driver.quit();
    }
}
*/
