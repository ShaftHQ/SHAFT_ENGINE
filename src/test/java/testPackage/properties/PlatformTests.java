package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.Platform;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class PlatformTests {
    String crossBrowserMode;
    String executionAddress;
    String targetPlatform;
    String proxy;

    @BeforeClass
    public void beforeClass() {
        crossBrowserMode = SHAFT.Properties.platform.crossBrowserMode();
        executionAddress = SHAFT.Properties.platform.executionAddress();
        targetPlatform = SHAFT.Properties.platform.targetPlatform();
        proxy = SHAFT.Properties.platform.proxy();

    }

    @Test
    public void test() {
        SHAFT.Properties.platform.set().crossBrowserMode(crossBrowserMode);
        SHAFT.Properties.platform.set().executionAddress(executionAddress);
        SHAFT.Properties.platform.set().targetPlatform(targetPlatform)
        SHAFT.Properties.platform.set().proxySettings(proxy);
    }
}
