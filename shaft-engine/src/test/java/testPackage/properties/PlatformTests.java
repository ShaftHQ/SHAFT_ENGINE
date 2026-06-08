package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class PlatformTests {
    String crossBrowserMode;
    String executionAddress;
    String targetPlatform;
    String proxy;
    Boolean driverProxy;
    Boolean jvmProxy;
    Boolean enableBiDi;

    @BeforeClass
    public void beforeClass() {
        crossBrowserMode = SHAFT.Properties.platform.crossBrowserMode();
        executionAddress = SHAFT.Properties.platform.executionAddress();
        targetPlatform = SHAFT.Properties.platform.targetPlatform();
        proxy = SHAFT.Properties.platform.proxy();
        driverProxy = SHAFT.Properties.platform.driverProxySettings();
        jvmProxy = SHAFT.Properties.platform.jvmProxySettings();
        enableBiDi = SHAFT.Properties.platform.enableBiDi();
    }

    @Test
    public void test() {
        SHAFT.Properties.platform.set().crossBrowserMode(crossBrowserMode);
        SHAFT.Properties.platform.set().executionAddress(executionAddress);
        SHAFT.Properties.platform.set().targetPlatform(targetPlatform);
        SHAFT.Properties.platform.set().proxySettings(proxy);
        SHAFT.Properties.platform.set().driverProxySettings(driverProxy);
        SHAFT.Properties.platform.set().jvmProxySettings(jvmProxy);
        SHAFT.Properties.platform.set().enableBiDi(enableBiDi);
    }
}
