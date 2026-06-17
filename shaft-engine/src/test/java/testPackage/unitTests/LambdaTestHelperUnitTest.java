package testPackage.unitTests;

import com.shaft.api.RequestBuilder;
import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.LambdaTestHelper;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.MutableCapabilities;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.InvocationTargetException;
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
        Assert.assertTrue(passMessage.contains("completed"));

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
    public void setupDesktopAndMobileWebExecutionShouldPopulateOptionalCapabilitiesWhenConfigured() throws Exception {
        SHAFT.Properties.lambdaTest.set().username("user").accessKey("key").browserVersion("133").geoLocation("EG").osVersion("14");
        SHAFT.Properties.web.set().targetBrowserName("chrome");

        SHAFT.Properties.platform.set().targetPlatform("mac");
        Method desktopMethod = LambdaTestHelper.class.getDeclaredMethod("setupDesktopWebExecution");
        desktopMethod.setAccessible(true);
        MutableCapabilities desktopCaps = (MutableCapabilities) desktopMethod.invoke(null);

        Assert.assertEquals(desktopCaps.getCapability("platformName"), "MacOS 14");
        Assert.assertEquals(desktopCaps.getCapability("browserVersion"), "133");
        Assert.assertEquals(((HashMap<?, ?>) desktopCaps.getCapability("lt:options")).get("geoLocation"), "EG");

        SHAFT.Properties.platform.set().targetPlatform("IOS");
        SHAFT.Properties.mobile.set().browserName("safari");
        Method mobileMethod = LambdaTestHelper.class.getDeclaredMethod("setupMobileWebExecution");
        mobileMethod.setAccessible(true);
        MutableCapabilities mobileCaps = (MutableCapabilities) mobileMethod.invoke(null);
        Assert.assertEquals(mobileCaps.getCapability("browserVersion"), "133");
        Assert.assertEquals(((HashMap<?, ?>) mobileCaps.getCapability("lt:options")).get("platformName").toString(), "IOS");
        Assert.assertEquals(((HashMap<?, ?>) mobileCaps.getCapability("lt:options")).get("geoLocation"), "EG");
    }

    @Test
    public void getLambdaTestDriverShouldInitializeDesktopAndMobileWebBranches() {
        SHAFT.Properties.lambdaTest.set().username("user").accessKey("key").appUrl("").appRelativeFilePath("");
        SHAFT.Properties.web.set().targetBrowserName("chrome");

        try (MockedConstruction<DriverFactoryHelper> helperConstruction = Mockito.mockConstruction(DriverFactoryHelper.class)) {
            SHAFT.Properties.platform.set().targetPlatform("windows");
            SHAFT.Properties.mobile.set().browserName("");
            MutableCapabilities desktopInput = new MutableCapabilities();
            desktopInput.setCapability("custom", "desktop");
            DriverFactoryHelper desktopHelper = LambdaTestHelper.getLambdaTestDriver(desktopInput);

            DriverFactoryHelper firstConstructed = helperConstruction.constructed().get(0);
            Assert.assertSame(desktopHelper, firstConstructed);
            ArgumentCaptor<MutableCapabilities> desktopCaptor = ArgumentCaptor.forClass(MutableCapabilities.class);
            Mockito.verify(firstConstructed).initializeDriver(desktopCaptor.capture());
            Assert.assertEquals(desktopCaptor.getValue().getCapability("custom"), "desktop");

            SHAFT.Properties.platform.set().targetPlatform("android");
            SHAFT.Properties.mobile.set().browserName("chrome");
            MutableCapabilities mobileInput = new MutableCapabilities();
            mobileInput.setCapability("custom", "mobile");
            DriverFactoryHelper mobileHelper = LambdaTestHelper.getLambdaTestDriver(mobileInput);

            DriverFactoryHelper secondConstructed = helperConstruction.constructed().get(1);
            Assert.assertSame(mobileHelper, secondConstructed);
            ArgumentCaptor<MutableCapabilities> mobileCaptor = ArgumentCaptor.forClass(MutableCapabilities.class);
            Mockito.verify(secondConstructed).initializeDriver(mobileCaptor.capture());
            Assert.assertEquals(mobileCaptor.getValue().getCapability("custom"), "mobile");
        }
    }

    @Test
    public void getLambdaTestDriverShouldInitializeNativeBranchForExistingAppUrl() {
        SHAFT.Properties.lambdaTest.set().username("user").accessKey("key").deviceName("device")
                .platformVersion("16").appUrl("lt://existing").appRelativeFilePath("");
        SHAFT.Properties.platform.set().targetPlatform("android");

        try (MockedConstruction<DriverFactoryHelper> helperConstruction = Mockito.mockConstruction(DriverFactoryHelper.class)) {
            MutableCapabilities input = new MutableCapabilities();
            input.setCapability("custom", "existing");
            DriverFactoryHelper helper = LambdaTestHelper.getLambdaTestDriver(input);

            DriverFactoryHelper constructedHelper = helperConstruction.constructed().getFirst();
            Assert.assertSame(helper, constructedHelper);
            ArgumentCaptor<MutableCapabilities> nativeCaptor = ArgumentCaptor.forClass(MutableCapabilities.class);
            Mockito.verify(constructedHelper).initializeDriver(Mockito.eq(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE), nativeCaptor.capture());
            Assert.assertEquals(nativeCaptor.getValue().getCapability("custom"), "existing");
            Assert.assertEquals(((HashMap<?, ?>) nativeCaptor.getValue().getCapability("lt:options")).get("app"), "lt://existing");
        }
    }

    @Test
    public void getLambdaTestDriverShouldUploadNewNativeAppAndUseUploadedUrl() {
        SHAFT.Properties.lambdaTest.set().username("user").accessKey("key").deviceName("device")
                .platformVersion("17").appName("Sample App").appUrl("").appRelativeFilePath("apps/my.apk").customID("")
                .geoLocation("US");
        SHAFT.Properties.platform.set().targetPlatform("IOS");

        RequestBuilder requestBuilder = Mockito.mock(RequestBuilder.class);
        SHAFT.API performedApi = Mockito.mock(SHAFT.API.class);
        FileActions fileActions = Mockito.mock(FileActions.class);

        Mockito.when(requestBuilder.setContentType(Mockito.anyString())).thenReturn(requestBuilder);
        Mockito.when(requestBuilder.setParameters(Mockito.anyMap(), Mockito.eq(RestActions.ParametersType.FORM))).thenReturn(requestBuilder);
        Mockito.when(requestBuilder.setAuthentication(Mockito.anyString(), Mockito.anyString(), Mockito.eq(RequestBuilder.AuthenticationType.BASIC))).thenReturn(requestBuilder);
        Mockito.when(requestBuilder.perform()).thenReturn(performedApi);
        Mockito.when(fileActions.getAbsolutePath("apps/my.apk")).thenReturn("/tmp/apps/my.apk");

        try (MockedConstruction<SHAFT.API> apiConstruction = Mockito.mockConstruction(SHAFT.API.class,
                (mock, context) -> Mockito.when(mock.post("app/upload/realDevice")).thenReturn(requestBuilder));
             MockedStatic<FileActions> fileActionsMock = Mockito.mockStatic(FileActions.class);
             MockedStatic<RestActions> restActionsMock = Mockito.mockStatic(RestActions.class);
             MockedConstruction<DriverFactoryHelper> helperConstruction = Mockito.mockConstruction(DriverFactoryHelper.class)) {

            fileActionsMock.when(() -> FileActions.getInstance(true)).thenReturn(fileActions);
            restActionsMock.when(() -> RestActions.getResponseJSONValue(Mockito.any(Object.class), Mockito.eq("app_url"))).thenReturn("lt://uploaded");

            MutableCapabilities input = new MutableCapabilities();
            input.setCapability("custom", "new");
            LambdaTestHelper.getLambdaTestDriver(input);

            Assert.assertEquals(apiConstruction.constructed().size(), 1);
            DriverFactoryHelper constructedHelper = helperConstruction.constructed().getFirst();
            ArgumentCaptor<MutableCapabilities> nativeCaptor = ArgumentCaptor.forClass(MutableCapabilities.class);
            Mockito.verify(constructedHelper).initializeDriver(Mockito.eq(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE), nativeCaptor.capture());
            Assert.assertEquals(nativeCaptor.getValue().getCapability("custom"), "new");
            HashMap<?, ?> options = (HashMap<?, ?>) nativeCaptor.getValue().getCapability("lt:options");
            Assert.assertEquals(options.get("app"), "lt://uploaded");
            Assert.assertEquals(options.get("platformName").toString(), "IOS");
            Assert.assertEquals(options.get("geoLocation"), "US");
        }
    }

    @Test
    public void failActionShouldThrowRuntimeException() throws Exception {
        Method method = LambdaTestHelper.class.getDeclaredMethod("failAction", String.class, Throwable[].class);
        method.setAccessible(true);

        try {
            method.invoke(null, "failure data", new Throwable[]{new RuntimeException("boom")});
            Assert.fail("Expected RuntimeException from failAction");
        } catch (InvocationTargetException exception) {
            Assert.assertTrue(exception.getCause() instanceof RuntimeException);
            Assert.assertTrue(exception.getCause().getMessage().contains("failed"));
        }
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
