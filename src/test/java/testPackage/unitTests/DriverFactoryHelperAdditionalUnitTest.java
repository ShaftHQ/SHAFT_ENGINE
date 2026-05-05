package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.util.List;

public class DriverFactoryHelperAdditionalUnitTest {
    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test
    public void mobileExecutionGuardsShouldReflectPlatformAndBrowserProperties() {
        SHAFT.Properties.platform.set().targetPlatform("Android");
        SHAFT.Properties.mobile.set().browserName("");
        Assert.assertTrue(DriverFactoryHelper.isMobileNativeExecution());
        Assert.assertFalse(DriverFactoryHelper.isMobileWebExecution());
        Assert.assertFalse(DriverFactoryHelper.isNotMobileExecution());

        SHAFT.Properties.mobile.set().browserName("chrome");
        Assert.assertFalse(DriverFactoryHelper.isMobileNativeExecution());
        Assert.assertTrue(DriverFactoryHelper.isMobileWebExecution());
    }

    @Test
    public void getDriverTypeFromNameAndNormalizeShouldHandleEdgeCases() throws Exception {
        Method getDriverType = DriverFactoryHelper.class.getDeclaredMethod("getDriverTypeFromName", String.class);
        getDriverType.setAccessible(true);
        Object chrome = getDriverType.invoke(null, " chrome ");
        Assert.assertEquals(chrome.toString(), "CHROME");

        Method normalize = DriverFactoryHelper.class.getDeclaredMethod("normalizeRemoteServerPingBaseUrl", String.class);
        normalize.setAccessible(true);
        Assert.assertEquals(normalize.invoke(null, "localhost:4444/wd/hub"), "http://localhost:4444/wd/hub/");
        Assert.assertThrows(Exception.class, () -> normalize.invoke(null, "http://bad host"));
    }

    @Test
    public void redactAndCaptureScreenshotHelpersShouldHandleSuccessAndFailure() throws Exception {
        Method redact = DriverFactoryHelper.class.getDeclaredMethod("redactUriCredentials", String.class);
        redact.setAccessible(true);
        Assert.assertEquals(redact.invoke(null, "https://user:key@example.com/wd/hub"), "https://***:***@example.com/wd/hub");
        Assert.assertEquals(redact.invoke(null, "not-a-valid-uri%%%"), "not-a-valid-uri%%%");

        DriverFactoryHelper helper = new DriverFactoryHelper();
        Method capture = DriverFactoryHelper.class.getDeclaredMethod("captureLaunchScreenshot");
        capture.setAccessible(true);

        WebDriver screenshotDriver = org.mockito.Mockito.mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        org.mockito.Mockito.when(((TakesScreenshot) screenshotDriver).getScreenshotAs(OutputType.BYTES)).thenReturn("x".getBytes());
        helper.setDriver(screenshotDriver);
        Object success = capture.invoke(helper);
        Assert.assertTrue(success instanceof List);

        WebDriver emptyScreenshotDriver = org.mockito.Mockito.mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        org.mockito.Mockito.when(((TakesScreenshot) emptyScreenshotDriver).getScreenshotAs(OutputType.BYTES)).thenReturn(new byte[0]);
        helper.setDriver(emptyScreenshotDriver);
        Assert.assertNull(capture.invoke(helper));
    }

    @Test
    public void closeDriverShouldHandleNullAndDriverExceptions() {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        helper.closeDriver(null);

        WebDriver driver = org.mockito.Mockito.mock(WebDriver.class);
        org.mockito.Mockito.doThrow(new RuntimeException("close fail")).when(driver).close();
        helper.closeDriver(driver);
        org.mockito.Mockito.verify(driver).close();
        org.mockito.Mockito.verify(driver).quit();
    }

    @Test
    public void initializeDriverWithExistingDriverShouldAttachReference() {
        DriverFactoryHelper helper = new DriverFactoryHelper();
        WebDriver driver = org.mockito.Mockito.mock(WebDriver.class);
        helper.initializeDriver(driver);
        Assert.assertEquals(helper.getDriver(), driver);
    }
}
