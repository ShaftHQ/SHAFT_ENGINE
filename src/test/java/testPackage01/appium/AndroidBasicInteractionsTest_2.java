package testPackage01.appium;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import io.appium.java_client.android.Activity;
import io.appium.java_client.android.AndroidDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class AndroidBasicInteractionsTest_2 {
    private WebDriver driver;
    private final String SEARCH_ACTIVITY = ".app.SearchInvoke";
    private final String ALERT_DIALOG_ACTIVITY = ".app.AlertDialogSamples";
    private final String PACKAGE = "io.appium.android.apis";

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

    @BeforeClass
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

    @AfterClass
    public void teardown() {
        BrowserFactory.closeAllBrowsers();
    }
}