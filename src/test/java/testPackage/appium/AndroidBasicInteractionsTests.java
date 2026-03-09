package testPackage.appium;

import com.google.common.collect.ImmutableMap;
import com.shaft.driver.SHAFT;
import com.shaft.gui.element.TouchActions;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.android.AndroidDriver;
import org.openqa.selenium.By;
import org.testng.annotations.Test;

import java.time.Duration;

public class AndroidBasicInteractionsTests extends MobileTest {
    private final String PACKAGE = "io.appium.android.apis";

    @Test(groups = {"ApiDemosDebug"})
    public void wizard_scrollInExpandableLists_verticalScrolling_insideScreen() {
        ((AndroidDriver) driver.get().getDriver()).runAppInBackground(Duration.ofSeconds(5));
        driver.get().element().performTouchAction()
                .swipeElementIntoView("Views")
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView("Expandable Lists")
                .tap(AppiumBy.accessibilityId("Expandable Lists"))
                .swipeElementIntoView("3. Simple Adapter")
                .tap(AppiumBy.accessibilityId("3. Simple Adapter"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 18']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.TextView[@text='Group 18']"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Child 13']"), TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 1']"), TouchActions.SwipeDirection.UP)
                .sendAppToBackground(1)
                .assertThat(By.xpath("//android.widget.TextView[@text='Group 1']")).exists();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void scrollInExpandableLists_verticalScrolling_insideScreen() {
        driver.get().touch()
                .swipeElementIntoView("Views")
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView("Expandable Lists")
                .tap(AppiumBy.accessibilityId("Expandable Lists"))
                .swipeElementIntoView("3. Simple Adapter")
                .tap(AppiumBy.accessibilityId("3. Simple Adapter"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 18']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.TextView[@text='Group 18']"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Child 13']"), TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 1']"), TouchActions.SwipeDirection.UP)
                .sendAppToBackground();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void scrollInExpandableLists_verticalScrolling_insideElement(){
        driver.get().touch()
                .swipeElementIntoView("Views")
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView("Splitting Touches across Views")
                .tap(AppiumBy.accessibilityId("Splitting Touches across Views"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list2"), By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Blue']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Blue']"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list2"), By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Abbaye de Belloc']"), TouchActions.SwipeDirection.UP)
                .tap(By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Abbaye de Belloc']"))
                .assertThat(By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Abbaye de Belloc']")).exists();

    }

    @Test(groups = {"ApiDemosDebug"})
    public void scrollInExpandableLists_verticalScrolling_insideElement2(){
        driver.get().touch()
                .swipeElementIntoView("Views")
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView("Splitting Touches across Views")
                .tap(AppiumBy.accessibilityId("Splitting Touches across Views"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list1"), By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Blue']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Blue']"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list1"), By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Abbaye de Belloc']"), TouchActions.SwipeDirection.UP)
                .assertThat(By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Abbaye de Belloc']")).exists();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void scrollInExpandableLists_horizontalScrolling_insideElement(){
        driver.get().touch()
                .swipeElementIntoView("Views")
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView("Tabs")
                .tap(AppiumBy.accessibilityId("Tabs"))
                .swipeElementIntoView("5. Scrollable")
                .tap(AppiumBy.accessibilityId("5. Scrollable"))
                .swipeElementIntoView(By.xpath("//android.widget.HorizontalScrollView"), By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 12']"), TouchActions.SwipeDirection.RIGHT)
                .tap(By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 12']"))
                .swipeElementIntoView(By.xpath("//android.widget.HorizontalScrollView"), By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 1']"), TouchActions.SwipeDirection.LEFT)
                .assertThat(By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 1']")).exists();
    }

    @Test(groups = {"ApiDemosDebug"})
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

    //    @Test(groups = {"ApiDemosDebug"})
    public void visualElementIdentification_requiresProcessing() {
        driver.get().touch()
                .swipeElementIntoView("src/main/resources/dynamicObjectRepository/content2.png", TouchActions.SwipeDirection.DOWN)
                .tap("src/main/resources/dynamicObjectRepository/content2.png");

        driver.get().assertThat().element(AppiumBy.accessibilityId("Assets")).exists().perform();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void testSendKeys() {
        String SEARCH_ACTIVITY = ".app.SearchInvoke";

        // Use appPackage + appActivity (absolute path) for Appium 3.x compatibility.
        // The "intent" shorthand format (pkg/.Activity) was unreliable in Appium 3.x on BrowserStack.
        ((AndroidDriver) driver.get().getDriver()).executeScript("mobile: startActivity",
                ImmutableMap.of("appPackage", PACKAGE, "appActivity", PACKAGE + SEARCH_ACTIVITY));

        driver.get().element().type(By.id("txt_query_prefill"), "Hello world!")
                .and().touch().tap(By.id("btn_start_search"))
                .and().assertThat(By.id("android:id/search_src_text")).text().isEqualTo("Hello world!").perform();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void testOpensAlert() {
        // Open the "Alert Dialog" activity of the android app
        String ALERT_DIALOG_ACTIVITY = ".app.AlertDialogSamples";

        // Use appPackage + appActivity (absolute path) for Appium 3.x compatibility.
        // The "intent" shorthand format (pkg/.Activity) was unreliable in Appium 3.x on BrowserStack.
        ((AndroidDriver) driver.get().getDriver()).executeScript("mobile: startActivity",
                ImmutableMap.of("appPackage", PACKAGE, "appActivity", PACKAGE + ALERT_DIALOG_ACTIVITY));

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
}