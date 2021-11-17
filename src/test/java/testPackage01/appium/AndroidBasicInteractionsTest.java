package testPackage01.appium;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.validation.Validations;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.android.Activity;
import io.appium.java_client.android.AndroidDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.*;

public class AndroidBasicInteractionsTest {
    private WebDriver driver;
    private final String SEARCH_ACTIVITY = ".app.SearchInvoke";
    private final String ALERT_DIALOG_ACTIVITY = ".app.AlertDialogSamples";
    private final String PACKAGE = "io.appium.android.apis";

    @Test
    public void scrollToElement_insideScreen(){
        By targetElement = AppiumBy.accessibilityId("ImageButton");
        ElementActions.performTouchAction(driver)
                        .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(targetElement, TouchActions.SwipeDirection.DOWN);
        Validations.assertThat()
                .element(driver, targetElement)
                .exists()
                .perform();
    }

    @Test
    public void scrollInExpandableLists_verticalScrolling(){
        ElementActions.performTouchAction(driver)
                .tap(AppiumBy.accessibilityId("Views"))
                .tap(AppiumBy.accessibilityId("Expandable Lists"))
                .tap(AppiumBy.accessibilityId("3. Simple Adapter"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 18']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.TextView[@text='Group 18']"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Child 13']"), TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 1']"), TouchActions.SwipeDirection.UP);
    }

    @Test
    public void scrollInExpandableLists_verticalScrolling_insideElement(){
        ElementActions.performTouchAction(driver)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Splitting Touches across Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Splitting Touches across Views"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list1"), By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Blue']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Blue']"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list1"), By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Abbaye de Belloc']"), TouchActions.SwipeDirection.UP)
                .tap(By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Abbaye de Belloc']"));
    }

    @Test
    public void scrollInExpandableLists_horizontalScrolling_insideElement(){
        ElementActions.performTouchAction(driver)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Tabs"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Tabs"))
                .tap(AppiumBy.accessibilityId("5. Scrollable"))
                .swipeElementIntoView(By.xpath("//android.widget.HorizontalScrollView"), By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 12']"), TouchActions.SwipeDirection.RIGHT)
                .tap(By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 12']"))
                .swipeElementIntoView(By.xpath("//android.widget.HorizontalScrollView"), By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 1']"), TouchActions.SwipeDirection.LEFT)
                .tap(By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 1']"));
    }

//    @Test
    public void scrollToElement_insideScrollableElement(){
        By scrollableElement = By.xpath("(//android.widget.TextView[@content-desc=\"The Android platform is a software stack for mobile devices including an operating system, middleware and key applications. Developers can create applications for the platform using the Android SDK. Applications are written using the Java programming language and run on Dalvik, a custom virtual machine designed for embedded use which runs on top of a Linux kernel. If you want to know how to develop applications for Android, you're in the right place. This site provides a variety of documentation that will help you learn about Android and develop mobile applications for the platform. An early look at the the Android SDK is also available. It includes sample projects with source code, development tools, an emulator, and of course all the libraries you'll need to build an Android application. What would it take to build a better mobile phone?\"])[4]");
        By targetElement = AppiumBy.accessibilityId("ImageButton");
        ElementActions.performTouchAction(driver)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("ScrollBars"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("ScrollBars"))
                .tap(AppiumBy.accessibilityId("3. Style"))
                .swipeElementIntoView(scrollableElement, targetElement, TouchActions.SwipeDirection.DOWN);
    }

    @Test
    public void testSendKeys() {
            ((AndroidDriver) driver).startActivity(new Activity(PACKAGE, SEARCH_ACTIVITY));
            ElementActions.type(driver, By.id("txt_query_prefill"), "Hello world!");
            ElementActions.performTouchAction(driver).tap(By.id("btn_start_search"));
            Validations.assertThat()
                    .element(driver, By.id("android:id/search_src_text"))
                    .text()
                    .isEqualTo("Hello world!")
                    .perform();
    }

    @Test
    public void testOpensAlert() {
        // Open the "Alert Dialog" activity of the android app
        ((AndroidDriver) driver).startActivity(new Activity(PACKAGE, ALERT_DIALOG_ACTIVITY));

        // Click button that opens a dialog
        ElementActions.performTouchAction(driver).tap(By.id("io.appium.android.apis:id/two_buttons"));

        // Check that the dialog is there
        Validations.verifyThat()
                .element(driver, By.id("android:id/alertTitle"))
                .text()
                .isEqualTo("Lorem ipsum dolor sit aie consectetur adipiscing\nPlloaso mako nuto siwuf cakso dodtos anr koop.")
                .perform();

        // Close the dialog
        ElementActions.performTouchAction(driver).tap(By.id("android:id/button1"));
    }

    @BeforeMethod
    public void setup() {
        // common attributes
        System.setProperty("targetOperatingSystem", "Android");
        System.setProperty("mobile_automationName", "UIAutomator2");
        System.setProperty("mobile_appWaitActivity","*");

        // local appium server (for local and github actions execution)
        System.setProperty("executionAddress", "0.0.0.0:4723");
        System.setProperty("mobile_app", "src/test/resources/TestDataFiles/apps/ApiDemos-debug.apk");
        driver = DriverFactory.getDriver();
    }

    @AfterMethod
    public void teardown() {
        BrowserFactory.closeAllBrowsers();
    }
}