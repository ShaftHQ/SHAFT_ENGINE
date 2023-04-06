package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class BrowserStackTests {
    String username;
    String accessKey;
    String platformVersion;
    String deviceName;
    String appUrl;
    String customID;
    String appName;
    String appRelativeFilePath;
    String osVersion;
    String browserVersion;
    boolean local;

    @BeforeClass
    public void beforeClass() {
        username = SHAFT.Properties.browserStack.username();
        accessKey = SHAFT.Properties.browserStack.accessKey();
        platformVersion = SHAFT.Properties.browserStack.platformVersion();
        deviceName = SHAFT.Properties.browserStack.deviceName();
        appUrl = SHAFT.Properties.browserStack.appUrl();
        customID = SHAFT.Properties.browserStack.customID();
        appName = SHAFT.Properties.browserStack.appName();
        appRelativeFilePath = SHAFT.Properties.browserStack.appRelativeFilePath();
        osVersion = SHAFT.Properties.browserStack.osVersion();
        browserVersion = SHAFT.Properties.browserStack.browserVersion();
        local = SHAFT.Properties.browserStack.local();
    }

    @Test
    public void test() {
        SHAFT.Properties.browserStack.set().username(username);
        SHAFT.Properties.browserStack.set().accessKey(accessKey);
        SHAFT.Properties.browserStack.set().platformVersion(platformVersion);
        SHAFT.Properties.browserStack.set().deviceName(deviceName);
        SHAFT.Properties.browserStack.set().appUrl(appUrl);
        SHAFT.Properties.browserStack.set().customID(customID);
        SHAFT.Properties.browserStack.set().appName(appName);
        SHAFT.Properties.browserStack.set().appRelativeFilePath(appRelativeFilePath);
        SHAFT.Properties.browserStack.set().osVersion(osVersion);
        SHAFT.Properties.browserStack.set().browserVersion(browserVersion);
        SHAFT.Properties.browserStack.set().local(local);
    }
}
