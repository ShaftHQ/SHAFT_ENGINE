package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class MobileTests {
    String platformName;
    String platformVersion;
    String deviceName;
    String automationName;
    String udid;
    String browserName;
    String browserVersion;
    String app;
    String appPackage;
    String appActivity;


    @BeforeClass
    public void beforeClass() {
        platformName = SHAFT.Properties.mobile.platformName();
        platformVersion = SHAFT.Properties.mobile.platformVersion();
        deviceName = SHAFT.Properties.mobile.deviceName();
        automationName = SHAFT.Properties.mobile.automationName();
        udid = SHAFT.Properties.mobile.udid();
        browserName = SHAFT.Properties.mobile.browserName();
        browserVersion = SHAFT.Properties.mobile.browserVersion();
        app = SHAFT.Properties.mobile.app();
        appPackage = SHAFT.Properties.mobile.appPackage();
        appActivity = SHAFT.Properties.mobile.appActivity();
    }

    @Test
    public void test() {
        SHAFT.Properties.mobile.set().platformName(platformName);
        SHAFT.Properties.mobile.set().platformVersion(platformVersion);
        SHAFT.Properties.mobile.set().deviceName(deviceName);
        SHAFT.Properties.mobile.set().automationName(automationName);
        SHAFT.Properties.mobile.set().udid(udid);
        SHAFT.Properties.mobile.set().browserName(browserName);
        SHAFT.Properties.mobile.set().browserVersion(browserVersion);
        SHAFT.Properties.mobile.set().app(app);
        SHAFT.Properties.mobile.set().appPackage(appPackage);
        SHAFT.Properties.mobile.set().appActivity(appActivity);
    }
}
