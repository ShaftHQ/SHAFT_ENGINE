package testPackage.appium;

import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class DragAndDropTest {
    private SHAFT.GUI.WebDriver driver;
    private final AppiumBy dragBtn = new AppiumBy.ByAndroidUIAutomator("UiSelector().text(\"Drag\")");
    private final By toBeDragged =  By.xpath("//android.view.ViewGroup[@content-desc=\"drag-l1\"]/android.widget.ImageView");
    private final By toBeDropped =By.xpath("//android.view.ViewGroup[@content-desc=\"drop-l1\"]/android.view.ViewGroup");
    @BeforeMethod
    public void setupUp(){
        System.setProperty("mobile_autoGrantPermissions", "true");
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2);
        SHAFT.Properties.platform.set().executionAddress("127.0.0.1:4754");
        SHAFT.Properties.mobile.set().deviceName("Pixel 4 API 28");
        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/wdio-demo.apk");
        driver = new SHAFT.GUI.WebDriver();
    }

    @Test
    public void testDragAndDrop(){
        driver.element().touch().tap(dragBtn);
        driver.element().touch().swipeToElement(toBeDragged, toBeDropped);
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        driver.quit();
    }
}
