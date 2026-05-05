package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.LambdaTestHelper;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.MutableCapabilities;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.util.HashMap;

public class LambdaTestHelperUnitTest {
    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void reportActionResultShouldBuildPassAndFailMessages() throws Exception {
        Method method = LambdaTestHelper.class.getDeclaredMethod("reportActionResult", String.class, String.class, Boolean.class, Throwable[].class);
        method.setAccessible(true);

        String passMessage = (String) method.invoke(null, "setup", "test-data", true, new Throwable[]{});
        Assert.assertTrue(passMessage.contains("successfully performed"));

        String failMessage = (String) method.invoke(null, "setup", "test-data", false, new Throwable[]{new RuntimeException("boom")});
        Assert.assertTrue(failMessage.contains("failed"));
    }

    @Test
    public void setLambdaTestPropertiesShouldPopulateCapabilitiesMapAndExecutionAddress() throws Exception {
        Method method = LambdaTestHelper.class.getDeclaredMethod("setLambdaTestProperties", String.class, String.class, String.class, String.class, String.class);
        method.setAccessible(true);

        @SuppressWarnings("unchecked")
        HashMap<String, Object> result = (HashMap<String, Object>) method.invoke(null, "user", "key", "device", "17", "lt://app");

        Assert.assertTrue(SHAFT.Properties.platform.executionAddress().contains("hub.lambdatest.com/wd/hub"));
        Assert.assertEquals(SHAFT.Properties.mobile.deviceName(), "device");
        Assert.assertEquals(SHAFT.Properties.mobile.platformVersion(), "17");
        Assert.assertEquals(SHAFT.Properties.mobile.app(), "lt://app");
        Assert.assertTrue(result.containsKey("appiumVersion"));
        Assert.assertTrue(result.containsKey("acceptInsecureCerts"));
        Assert.assertTrue(result.containsKey("debug"));
        Assert.assertTrue(result.containsKey("networkLogs"));
    }

    @Test
    public void setupDesktopAndMobileWebExecutionShouldReturnLtOptions() throws Exception {
        SHAFT.Properties.lambdaTest.set().username("user").accessKey("key");
        SHAFT.Properties.web.set().targetBrowserName("chrome");
        SHAFT.Properties.platform.set().targetPlatform("Windows");

        Method desktopMethod = LambdaTestHelper.class.getDeclaredMethod("setupDesktopWebExecution");
        desktopMethod.setAccessible(true);
        MutableCapabilities desktopCaps = (MutableCapabilities) desktopMethod.invoke(null);
        Assert.assertNotNull(desktopCaps.getCapability("lt:options"));
        Assert.assertEquals(desktopCaps.getCapability("browserName"), "chrome");

        SHAFT.Properties.platform.set().targetPlatform("Android");
        Method mobileMethod = LambdaTestHelper.class.getDeclaredMethod("setupMobileWebExecution");
        mobileMethod.setAccessible(true);
        MutableCapabilities mobileCaps = (MutableCapabilities) mobileMethod.invoke(null);
        Assert.assertNotNull(mobileCaps.getCapability("lt:options"));
        Assert.assertEquals(mobileCaps.getCapability("browserName"), "chrome");
    }

    @Test
    public void setupNativeAppExecutionWithExistingAppUrlShouldBuildCapabilities() throws Exception {
        SHAFT.Properties.platform.set().targetPlatform("Android");
        Method method = LambdaTestHelper.class.getDeclaredMethod("setupNativeAppExecution", String.class, String.class, String.class, String.class, String.class);
        method.setAccessible(true);
        MutableCapabilities caps = (MutableCapabilities) method.invoke(null, "user", "key", "device", "16", "lt://app");
        Object options = caps.getCapability("lt:options");
        Assert.assertTrue(options instanceof HashMap);
        Assert.assertTrue(((HashMap<?, ?>) options).containsKey("app"));
    }
}
