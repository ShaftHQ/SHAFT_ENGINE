package testPackage.appium;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.internal.FlutterBy;
import org.openqa.selenium.Platform;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class FlutterTest {
    private SHAFT.GUI.WebDriver driver;
    private final FlutterBy enter_btn = new FlutterBy.ByText("ENTER");
    private final FlutterBy button = FlutterBy.descendant(FlutterBy.valueKey("silverList"),FlutterBy.text("ADD"), true, true);



    @Test
    public void testFlutter() {
        driver.element().getText(enter_btn);
        driver.element().click(enter_btn);
        driver.element().click(button);

    }

    @BeforeMethod
    public void setUp() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @BeforeClass
    public void setConfigurations() {
        System.setProperty("mobile_autoGrantPermissions", "true");
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().platformVersion("14.0");
        SHAFT.Properties.mobile.set().automationName("Flutter");
        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/shopping_provider1.apk");
        SHAFT.Properties.mobile.set().deviceName("nightwatch-android-11");
        SHAFT.Properties.platform.set().executionAddress("http://127.0.0.1:4327/");
    }

    @AfterMethod
    public void tearDown() {
        if (driver != null) {
            driver.quit();
        }
    }
}
