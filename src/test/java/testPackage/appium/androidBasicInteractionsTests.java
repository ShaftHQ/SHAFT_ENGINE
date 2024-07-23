package testPackage.appium;

import com.google.common.collect.ImmutableMap;
import com.shaft.driver.SHAFT;
import com.shaft.gui.element.TouchActions;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.time.Duration;

public class androidBasicInteractionsTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private final String PACKAGE = "io.appium.android.apis";

    @Test
    public void wizard_scrollInExpandableLists_verticalScrolling_insideScreen() {
        ((AndroidDriver) driver.get().getDriver()).runAppInBackground(Duration.ofSeconds(5));
        driver.get().element().performTouchAction()
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
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 1']"), TouchActions.SwipeDirection.UP)
                .sendAppToBackground(1);
    }

    @Test
    public void scrollInExpandableLists_verticalScrolling_insideScreen() {
        driver.get().touch()
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
                .sendAppToBackground();
    }

    @Test
    public void scrollInExpandableLists_verticalScrolling_insideElement(){
        driver.get().touch()
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Splitting Touches across Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Splitting Touches across Views"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list2"), By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Blue']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Blue']"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list2"), By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Abbaye de Belloc']"), TouchActions.SwipeDirection.UP)
                .tap(By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Abbaye de Belloc']"));
    }

    @Test
    public void scrollInExpandableLists_verticalScrolling_insideElement2(){
        driver.get().touch()
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
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
        driver.get().touch()
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

    @Test
    public void visualElementIdentification_samedpi() {
        var referenceImageFile = "content.png";
        if (SHAFT.Properties.platform.executionAddress().toLowerCase().contains("browserstack")) {
            referenceImageFile = "content_local.png";
        }

        var elementReferenceFilePath = "src/main/resources/dynamicObjectRepository/Android/" + referenceImageFile;
        driver.get().touch()
                .swipeElementIntoView(elementReferenceFilePath, TouchActions.SwipeDirection.DOWN)
                .waitUntilElementIsVisible(elementReferenceFilePath)
                .tap(elementReferenceFilePath);

        driver.get().assertThat().element(AppiumBy.accessibilityId("Assets")).exists().perform();
    }

    //    @Test
    public void visualElementIdentification_requiresProcessing() {
        driver.get().touch()
                .swipeElementIntoView("src/main/resources/dynamicObjectRepository/content2.png", TouchActions.SwipeDirection.DOWN)
                .tap("src/main/resources/dynamicObjectRepository/content2.png");

        driver.get().assertThat().element(AppiumBy.accessibilityId("Assets")).exists().perform();
    }

    @Test
    public void testSendKeys() {
        String SEARCH_ACTIVITY = ".app.SearchInvoke";

        ((AndroidDriver) driver.get().getDriver()).executeScript("mobile:startActivity", ImmutableMap.of("intent", PACKAGE + "/" + SEARCH_ACTIVITY));
//        ((AndroidDriver) driver.get().getDriver()).startActivity(new Activity(PACKAGE, SEARCH_ACTIVITY));

        driver.get().element().type(By.id("txt_query_prefill"), "Hello world!")
                .and().touch().tap(By.id("btn_start_search"))
                .and().assertThat(By.id("android:id/search_src_text")).text().isEqualTo("Hello world!").perform();
    }

    @Test
    public void testOpensAlert() {
        // Open the "Alert Dialog" activity of the android app
        String ALERT_DIALOG_ACTIVITY = ".app.AlertDialogSamples";

        ((AndroidDriver) driver.get().getDriver()).executeScript("mobile:startActivity", ImmutableMap.of("intent", PACKAGE + "/" + ALERT_DIALOG_ACTIVITY));
//        ((AndroidDriver) driver.get().getDriver()).startActivity(new Activity(PACKAGE, ALERT_DIALOG_ACTIVITY));

        // Click button that opens a dialog
        driver.get().element().touch().tap(By.id("io.appium.android.apis:id/two_buttons"));

        // Check that the dialog is there
        driver.get().verifyThat()
                .element(By.id("android:id/alertTitle"))
                .text()
                .isEqualTo("Lorem ipsum dolor sit aie consectetur adipiscing\nPlloaso mako nuto siwuf cakso dodtos anr koop.")
                .perform();

        // Close the dialog
        driver.get().element().touch().tap(By.id("android:id/button1"));
    }

    @SuppressWarnings("CommentedOutCode")
    @BeforeMethod
    public void setup() {
        System.setProperty("mobile_autoGrantPermissions", "true");
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2);

        // self-managed execution
//        SHAFT.Properties.mobile.set().selfManaged(true);
//        SHAFT.Properties.mobile.set().selfManagedAndroidSDKVersion(31);

        // local appium server (for local and GitHub actions execution)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
//        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");

        // local appium server (android-emulator docker-compose)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4725");
//        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");

        // remote browserstack server (new app version)
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.browserStack.set().platformVersion("13.0");
//        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
//        SHAFT.Properties.browserStack.set().appName("ApiDemos-debug.apk");
//        SHAFT.Properties.browserStack.set().appRelativeFilePath("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");
//        SHAFT.Properties.browserStack.set().appUrl("");

        // remote browserstack server (existing app version)
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.browserStack.set().platformVersion("13.0");
//        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
//        SHAFT.Properties.browserStack.set().appName("ApiDemos-debug.apk");
//        SHAFT.Properties.browserStack.set().appRelativeFilePath("");
//        SHAFT.Properties.browserStack.set().appUrl("bs://e744ef24a081b0d4cb5f9699a5dd69d6a3a2dbce");
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        driver.get().quit();
    }
}