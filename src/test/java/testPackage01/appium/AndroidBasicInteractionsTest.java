package testPackage01.appium;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.validation.Validations;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.android.Activity;
import io.appium.java_client.android.AndroidDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.time.Duration;

public class AndroidBasicInteractionsTest {
    private WebDriver driver;
    private SHAFT.GUI.WebDriver shaftDriver;
    private final String PACKAGE = "io.appium.android.apis";

    @Test(groups = {"Wizard"})
    public void wizard_scrollInExpandableLists_verticalScrolling_insideScreen() {
        ((AndroidDriver) shaftDriver.getDriver()).runAppInBackground(Duration.ofSeconds(5));
        shaftDriver.element().performTouchAction()
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Expandable Lists"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Expandable Lists"))
                .swipeElementIntoView(AppiumBy.accessibilityId("3. Simple Adapter"), TouchActions.SwipeDirection.DOWN)
//                .swipeElementIntoView(AppiumBy.accessibilityId("3. Simple Adapter"), TouchActions.SwipeDirection.DOWN);
//        shaftDriver.verifyThat().element(AppiumBy.accessibilityId("3. Simple Adapter")).matchesReferenceImage().perform();
//        shaftDriver.element().performTouchAction()
                .tap(AppiumBy.accessibilityId("3. Simple Adapter"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 18']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.TextView[@text='Group 18']"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Child 13']"), TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 1']"), TouchActions.SwipeDirection.UP);
    }

    @BeforeMethod(onlyForGroups = {"Wizard"})
    public void beforeMethod_wizard() {
//        Properties.platform.setProperty("executionAddress", "0.0.0.0:4723");
//        Properties.platform.setProperty("targetOperatingSystem", "Android");
//        Properties.mobile.setProperty("mobile_automationName", "UIAutomator2");
//        Properties.mobile.setProperty("mobile_appWaitActivity", "*");
//        Properties.mobile.setProperty("mobile_disableWindowAnimation", "true");
//        Properties.mobile.setProperty("mobile_app", Properties.paths.testData() + "apps/ApiDemos-debug.apk");

        shaftDriver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod(onlyForGroups = {"Wizard"})
    public void afterMethod_wizard() {
        shaftDriver.quit();
    }

    @Test(groups = {"Legacy"})
    public void scrollInExpandableLists_verticalScrolling_insideScreen() {
        ElementActions.performTouchAction(driver)
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Expandable Lists"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Expandable Lists"))
                .swipeElementIntoView(AppiumBy.accessibilityId("3. Simple Adapter"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("3. Simple Adapter"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 18']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.TextView[@text='Group 18']"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Child 13']"), TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 1']"), TouchActions.SwipeDirection.UP);
    }

    @Test(groups = {"Legacy"})
    public void scrollInExpandableLists_verticalScrolling_insideElement(){
        ElementActions.performTouchAction(driver)
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Splitting Touches across Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Splitting Touches across Views"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list2"), By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Blue']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Blue']"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list2"), By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Abbaye de Belloc']"), TouchActions.SwipeDirection.UP)
                .tap(By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Abbaye de Belloc']"));
    }

    @Test(groups = {"Legacy"})
    public void scrollInExpandableLists_verticalScrolling_insideElement2(){
        ElementActions.performTouchAction(driver)
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Splitting Touches across Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Splitting Touches across Views"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list1"), By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Blue']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Blue']"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list1"), By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Abbaye de Belloc']"), TouchActions.SwipeDirection.UP)
                .tap(By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Abbaye de Belloc']"));
    }

    @Test(groups = {"Legacy"})
    public void scrollInExpandableLists_horizontalScrolling_insideElement(){
        ElementActions.performTouchAction(driver)
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Tabs"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Tabs"))
                .swipeElementIntoView(AppiumBy.accessibilityId("5. Scrollable"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("5. Scrollable"))
                .swipeElementIntoView(By.xpath("//android.widget.HorizontalScrollView"), By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 12']"), TouchActions.SwipeDirection.RIGHT)
                .tap(By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 12']"))
                .swipeElementIntoView(By.xpath("//android.widget.HorizontalScrollView"), By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 1']"), TouchActions.SwipeDirection.LEFT)
                .tap(By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 1']"));
    }

    @Test(groups = {"Legacy"})
    public void visualElementIdentification_samedpi() {
        ElementActions.performTouchAction(driver)
                .swipeElementIntoView("src/main/resources/dynamicObjectRepository/content.png", TouchActions.SwipeDirection.DOWN)
                .tap("src/main/resources/dynamicObjectRepository/content.png");
        Validations.assertThat()
                .element(driver, AppiumBy.accessibilityId("Assets"))
                .exists()
                .perform();
    }

    //@Test(groups = {"Legacy"})
    public void visualElementIdentification_requiresProcessing() {
        ElementActions.performTouchAction(driver)
                .swipeElementIntoView("src/main/resources/dynamicObjectRepository/content2.png", TouchActions.SwipeDirection.DOWN)
                .tap("src/main/resources/dynamicObjectRepository/content2.png");
        Validations.assertThat()
                .element(driver, AppiumBy.accessibilityId("Assets"))
                .exists()
                .perform();
    }

    @Test(groups = {"Legacy"})
    public void testSendKeys() {
        String SEARCH_ACTIVITY = ".app.SearchInvoke";
        ((AndroidDriver) driver).startActivity(new Activity(PACKAGE, SEARCH_ACTIVITY));
            ElementActions.type(driver, By.id("txt_query_prefill"), "Hello world!");
            ElementActions.performTouchAction(driver).tap(By.id("btn_start_search"));
            Validations.assertThat()
                    .element(driver, By.id("android:id/search_src_text"))
                    .text()
                    .isEqualTo("Hello world!")
                    .perform();
    }

    @Test(groups = {"Legacy"})
    public void testOpensAlert() {
        // Open the "Alert Dialog" activity of the android app
        String ALERT_DIALOG_ACTIVITY = ".app.AlertDialogSamples";
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

    @SuppressWarnings("CommentedOutCode")
    @BeforeMethod(onlyForGroups = {"Legacy"})
    public void setup() {
//        // common attributes
//        System.setProperty("targetOperatingSystem", "Android");
//        System.setProperty("mobile_automationName", "UIAutomator2");
//        System.setProperty("mobile_appWaitActivity","*");
//        System.setProperty("mobile_disableWindowAnimation","true");
//
//        // local appium server (for local and github actions execution)
//        System.setProperty("executionAddress", "0.0.0.0:4723");
//        System.setProperty("mobile_app", System.getProperty("testDataFolderPath")+"apps/ApiDemos-debug.apk");
        driver = DriverFactory.getDriver();

        // remote browserstack server (new app version)
//        System.setProperty("executionAddress", "browserstack");
//        System.setProperty("browserStack.platformVersion", "11.0");
//        System.setProperty("browserStack.deviceName", "Google Pixel 4");
//        System.setProperty("browserStack.appName", "ApiDemos-debug.apk");
//        System.setProperty("browserStack.appRelativeFilePath", System.getProperty("testDataFolderPath")+"apps/ApiDemos-debug.apk");
//        System.setProperty("browserStack.appUrl", "");
//        driver = DriverFactory.getDriver();

        // remote browserstack server (existing app version)
//        System.setProperty("browserStack.platformVersion", "11.0");
//        System.setProperty("browserStack.deviceName", "Google Pixel 4");
//        System.setProperty("browserStack.appName", "ApiDemos-debug.apk");
//        System.setProperty("browserStack.appRelativeFilePath", "");
//        System.setProperty("browserStack.appUrl", "bs://030ae95f0aa6d82ca804e342adde364c2614b419");
//        driver = DriverFactory.getDriver();
    }

    @AfterMethod(onlyForGroups = {"Legacy"})
    public void teardown() {
        DriverFactory.closeAllDrivers();
    }
}