package testPackage;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.TouchActions;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.android.AndroidDriver;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.time.Duration;

public class TestClass {
    SHAFT.GUI.WebDriver driver;
    SHAFT.TestData.JSON testData;

    @Test
    public void navigateToDuckDuckGoAndAssertBrowserTitleIsDisplayedCorrectly() {
        ((AndroidDriver) driver.getDriver()).runAppInBackground(Duration.ofSeconds(5));
        driver.touch()
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Expandable Lists"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Expandable Lists"))
                .swipeElementIntoView(AppiumBy.accessibilityId("3. Simple Adapter"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("3. Simple Adapter"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 18']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.TextView[@text='Group 18']"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Child 13']"), TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 1']"), TouchActions.SwipeDirection.UP)
                .sendAppToBackground(1)
                .and()
                .assertThat(By.xpath("//android.widget.TextView[@text='Group 1']")).exists();
    }

    @BeforeClass
    public void beforeClass() {
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod
    public void afterMethod() {
        driver.quit();
    }
}
