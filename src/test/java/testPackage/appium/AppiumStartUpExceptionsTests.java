package testPackage.appium;

import com.shaft.driver.SHAFT;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.Platform;
import org.openqa.selenium.SessionNotCreatedException;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("unused")
public class AppiumStartUpExceptionsTests {
    private SHAFT.GUI.WebDriver driver;

    @Test
    public void apkNotFoundException() {
        Assert.assertThrows("sds",SessionNotCreatedException.class,
                () -> {
                    System.setProperty("mobile_autoGrantPermissions", "true");
                    // common attributes
                    SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
                    SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2);

                    SHAFT.Properties.platform.set().executionAddress("localhost:4723");
                    SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/ApiDemoss-debug.apk");
                    driver = new SHAFT.GUI.WebDriver();
                });
    }

    @Test
    public void deviceNotFoundException() {
        Assert.assertThrows("sds",SessionNotCreatedException.class,
                () -> {
                    System.setProperty("mobile_autoGrantPermissions", "true");
                    // common attributes
                    SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
                    SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2);

                    SHAFT.Properties.platform.set().executionAddress("localhost:4723");
                    SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");
                    driver = new SHAFT.GUI.WebDriver();
        });
    }

    @Test
    public void appiumServerNotCreatedException() {
        Assert.assertThrows("sds",SessionNotCreatedException.class,
                () -> {
                    System.setProperty("mobile_autoGrantPermissions", "true");
                    // common attributes
                    SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
                    SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2);

                    SHAFT.Properties.platform.set().executionAddress("localhost:4723");
                    SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");
                    driver = new SHAFT.GUI.WebDriver();
        });
    }
}
