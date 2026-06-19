package testPackage.unitTests;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.BrowserStackHelper;
import com.shaft.driver.internal.DriverFactory.BrowserStackSdkHelper;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.api.RequestBuilder;
import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import io.restassured.response.Response;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.MutableCapabilities;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.util.Map;

public class BrowserStackHelperUnitTest {
    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test(description = "BrowserStack execution address should use https and include /wd/hub path")
    public void constructExecutionAddress_usesHttpsAndWdHubPath() throws Exception {
        String executionAddress = (String) invokePrivate("constructExecutionAddress",
                new Class[]{String.class, String.class},
                "user123", "key456");

        SHAFT.Validations.assertThat().object(executionAddress)
                .isEqualTo("https://user123:key456@hub-cloud.browserstack.com/wd/hub").perform();
    }

    @Test
    public void constructorShouldInstantiate() {
        Assert.assertNotNull(new BrowserStackHelper());
    }

    @Test
    public void setupDesktopWebExecutionShouldCoverOptionalAndOsBranches() throws Exception {
        SHAFT.Properties.browserStack.set().userName("user").accessKey("key").osVersion("Sonoma")
                .browserVersion("17.0").geoLocation("EG").local(true).seleniumVersion("4.25.0");
        SHAFT.Properties.platform.set().targetPlatform("mac");
        SHAFT.Properties.web.set().targetBrowserName("Safari");

        MutableCapabilities desktopCapabilities = (MutableCapabilities) invokePrivate("setupDesktopWebExecution", new Class[]{});
        Assert.assertEquals(SHAFT.Properties.mobile.browserName(), "Safari");
        Assert.assertTrue(SHAFT.Properties.platform.executionAddress().contains("https://user:key@hub-cloud.browserstack.com/wd/hub"));
        Assert.assertEquals(desktopCapabilities.getCapability("browserVersion"), "17.0");

        @SuppressWarnings("unchecked")
        Map<String, Object> options = (Map<String, Object>) desktopCapabilities.getCapability("bstack:options");
        Assert.assertEquals(options.get("os"), "OS X");
        Assert.assertEquals(options.get("osVersion"), "Sonoma");
        Assert.assertEquals(options.get("geoLocation"), "EG");
        Assert.assertTrue(options.containsKey("buildName"));
        Assert.assertTrue(options.containsKey("projectName"));

        SHAFT.Properties.browserStack.set().browserVersion("").geoLocation("");
        SHAFT.Properties.platform.set().targetPlatform("windows");
        MutableCapabilities windowsCapabilities = (MutableCapabilities) invokePrivate("setupDesktopWebExecution", new Class[]{});
        @SuppressWarnings("unchecked")
        Map<String, Object> windowsOptions = (Map<String, Object>) windowsCapabilities.getCapability("bstack:options");
        Assert.assertEquals(windowsOptions.get("os"), "Windows");
        Assert.assertNull(windowsCapabilities.getCapability("browserVersion"));
        Assert.assertFalse(windowsOptions.containsKey("geoLocation"));
    }

    @Test
    public void setupMobileWebExecutionShouldSetMobileCapabilities() throws Exception {
        SHAFT.Properties.browserStack.set().userName("user").accessKey("key").osVersion("13.0")
                .deviceName("Google Pixel 7").local(true).appiumVersion("2.0.0").seleniumVersion("4.25.0");
        SHAFT.Properties.platform.set().targetPlatform("android").enableBiDi(true);

        MutableCapabilities capabilities = (MutableCapabilities) invokePrivate("setupMobileWebExecution", new Class[]{});
        Assert.assertTrue(SHAFT.Properties.platform.executionAddress().contains("https://user:key@hub-cloud.browserstack.com/wd/hub"));
        Assert.assertEquals(capabilities.getCapability("webSocketUrl"), true);

        @SuppressWarnings("unchecked")
        Map<String, Object> options = (Map<String, Object>) capabilities.getCapability("bstack:options");
        Assert.assertEquals(options.get("osVersion"), "13.0");
        Assert.assertEquals(options.get("deviceName"), "Google Pixel 7");
        Assert.assertEquals(options.get("appiumVersion"), "2.0.0");
        Assert.assertEquals(options.get("seleniumVersion"), "4.25.0");
    }

    @Test
    public void nativeSetupAndReportActionResultShouldCoverExistingAppAndFailMessage() throws Exception {
        SHAFT.Properties.browserStack.set().appiumVersion("2.0.0").acceptInsecureCerts(true).debug(true).networkLogs(true);
        ThreadLocalPropertiesManager.setProperty("browserStack.sessionName", "native-session");
        ThreadLocalPropertiesManager.setProperty("browserStack.enableBiometric", "true");
        MutableCapabilities capabilities = (MutableCapabilities) invokePrivate("setupNativeAppExecution",
                new Class[]{String.class, String.class, String.class, String.class, String.class},
                "user", "key", "device", "13.0", "bs://app");
        Assert.assertEquals(SHAFT.Properties.mobile.app(), "bs://app");
        Assert.assertTrue(SHAFT.Properties.platform.executionAddress().contains("https://user:key@hub-cloud.browserstack.com/wd/hub"));
        @SuppressWarnings("unchecked")
        Map<String, Object> options = (Map<String, Object>) capabilities.getCapability("bstack:options");
        Assert.assertEquals(options.get("interactiveDebugging"), true);
        Assert.assertEquals(options.get("networkLogs"), true);
        Assert.assertEquals(options.get("sessionName"), "native-session");
        Assert.assertEquals(options.get("enableBiometric"), "true");

        String passMessage = (String) invokePrivate("reportActionResult",
                new Class[]{String.class, String.class, Boolean.class, Throwable[].class},
                "setup", "data", true, new Throwable[]{});
        Assert.assertTrue(passMessage.contains("completed"));
        String failMessage = (String) invokePrivate("reportActionResult",
                new Class[]{String.class, String.class, Boolean.class, Throwable[].class},
                "setup", "data", false, new Throwable[]{new RuntimeException("boom")});
        Assert.assertTrue(failMessage.contains("failed"));
    }

    @Test
    public void getBrowserStackDriverShouldCoverDesktopMobileAndNativeExistingBranches() {
        SHAFT.Properties.browserStack.set().userName("user").accessKey("key").osVersion("13.0")
                .platformVersion("13.0").deviceName("Google Pixel 7").appiumVersion("2.0.0")
                .seleniumVersion("4.25.0").appUrl("").appRelativeFilePath("").browserVersion("17.0");

        try (MockedConstruction<DriverFactoryHelper> helperConstruction = Mockito.mockConstruction(DriverFactoryHelper.class);
             MockedStatic<BrowserStackSdkHelper> sdkHelperMock = Mockito.mockStatic(BrowserStackSdkHelper.class)) {
            sdkHelperMock.when(BrowserStackSdkHelper::generateBrowserStackYml).thenReturn("browserstack.yml");

            SHAFT.Properties.platform.set().targetPlatform("windows");
            SHAFT.Properties.mobile.set().browserName("");
            MutableCapabilities desktopInput = new MutableCapabilities();
            desktopInput.setCapability("custom", "desktop");
            DriverFactoryHelper desktopHelper = BrowserStackHelper.getBrowserStackDriver(desktopInput);

            DriverFactoryHelper firstConstructed = helperConstruction.constructed().getFirst();
            Assert.assertSame(desktopHelper, firstConstructed);
            ArgumentCaptor<MutableCapabilities> desktopCaptor = ArgumentCaptor.forClass(MutableCapabilities.class);
            Mockito.verify(firstConstructed).initializeDriver(desktopCaptor.capture());
            Assert.assertEquals(desktopCaptor.getValue().getCapability("custom"), "desktop");
            sdkHelperMock.verify(BrowserStackSdkHelper::generateBrowserStackYml);

            SHAFT.Properties.platform.set().targetPlatform("android");
            SHAFT.Properties.mobile.set().browserName("chrome");
            MutableCapabilities mobileInput = new MutableCapabilities();
            mobileInput.setCapability("custom", "mobile");
            BrowserStackHelper.getBrowserStackDriver(mobileInput);
            DriverFactoryHelper secondConstructed = helperConstruction.constructed().get(1);
            ArgumentCaptor<MutableCapabilities> mobileCaptor = ArgumentCaptor.forClass(MutableCapabilities.class);
            Mockito.verify(secondConstructed).initializeDriver(mobileCaptor.capture());
            Assert.assertEquals(mobileCaptor.getValue().getCapability("custom"), "mobile");

            SHAFT.Properties.browserStack.set().appUrl("bs://existing-app");
            MutableCapabilities nativeInput = new MutableCapabilities();
            nativeInput.setCapability("custom", "native");
            BrowserStackHelper.getBrowserStackDriver(nativeInput);
            DriverFactoryHelper thirdConstructed = helperConstruction.constructed().get(2);
            ArgumentCaptor<MutableCapabilities> nativeCaptor = ArgumentCaptor.forClass(MutableCapabilities.class);
            Mockito.verify(thirdConstructed).initializeDriver(Mockito.eq(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE), nativeCaptor.capture());
            Assert.assertEquals(nativeCaptor.getValue().getCapability("custom"), "native");
        }
    }

    @Test
    public void getBrowserStackDriverShouldRouteRemoteMobileAppAsNativeExecution() {
        SHAFT.Properties.browserStack.set().userName("user").accessKey("key").platformVersion("11.0")
                .deviceName("Samsung Galaxy S21").appUrl("").appRelativeFilePath("").appiumVersion("2.0.0");
        SHAFT.Properties.platform.set().targetPlatform("ANDROID");
        SHAFT.Properties.mobile.set().app("bs://existing-mobile-app").browserName("");

        try (MockedConstruction<DriverFactoryHelper> helperConstruction = Mockito.mockConstruction(DriverFactoryHelper.class);
             MockedStatic<BrowserStackSdkHelper> sdkHelperMock = Mockito.mockStatic(BrowserStackSdkHelper.class)) {
            sdkHelperMock.when(BrowserStackSdkHelper::generateBrowserStackYml).thenReturn("browserstack.yml");

            MutableCapabilities input = new MutableCapabilities();
            input.setCapability("custom", "native-mobile-app");
            DriverFactoryHelper helper = BrowserStackHelper.getBrowserStackDriver(input);

            DriverFactoryHelper constructed = helperConstruction.constructed().getFirst();
            Assert.assertSame(helper, constructed);
            ArgumentCaptor<MutableCapabilities> nativeCaptor = ArgumentCaptor.forClass(MutableCapabilities.class);
            Mockito.verify(constructed).initializeDriver(Mockito.eq(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE), nativeCaptor.capture());
            Assert.assertEquals(nativeCaptor.getValue().getCapability("custom"), "native-mobile-app");
            Assert.assertEquals(SHAFT.Properties.mobile.app(), "bs://existing-mobile-app");
        }
    }

    @Test
    public void getBrowserStackDriverShouldCoverNewNativeAppUploadBranch() {
        SHAFT.Properties.browserStack.set().userName("user").accessKey("key").platformVersion("13.0")
                .deviceName("Google Pixel 7").appName("ApiDemos").appRelativeFilePath("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk")
                .appUrl("").customID("").appiumVersion("2.0.0").acceptInsecureCerts(true).debug(true).networkLogs(true);

        FileActions mockedFileActions = Mockito.mock(FileActions.class);
        Mockito.when(mockedFileActions.getAbsolutePath(Mockito.anyString())).thenReturn("/tmp/app.apk");

        RequestBuilder mockedRequestBuilder = Mockito.mock(RequestBuilder.class, Mockito.RETURNS_SELF);
        SHAFT.API mockedApi = Mockito.mock(SHAFT.API.class);
        Response mockedResponse = Mockito.mock(Response.class);

        try (MockedStatic<FileActions> fileActionsMock = Mockito.mockStatic(FileActions.class);
             MockedStatic<RestActions> restActionsMock = Mockito.mockStatic(RestActions.class);
             MockedStatic<BrowserStackSdkHelper> sdkHelperMock = Mockito.mockStatic(BrowserStackSdkHelper.class);
             MockedConstruction<DriverFactoryHelper> helperConstruction = Mockito.mockConstruction(DriverFactoryHelper.class);
             MockedConstruction<com.shaft.tools.io.internal.ProgressBarLogger> ignoredProgressBar = Mockito.mockConstruction(com.shaft.tools.io.internal.ProgressBarLogger.class);
             MockedConstruction<RestActions> restActionsConstruction = Mockito.mockConstruction(RestActions.class, (mock, context) -> {
                 Mockito.when(mock.buildNewRequest(Mockito.anyString(), Mockito.eq(RestActions.RequestType.POST))).thenReturn(mockedRequestBuilder);
                 Mockito.when(mockedRequestBuilder.performRequest()).thenReturn(mockedApi);
                 Mockito.when(mock.getResponse()).thenReturn(mockedResponse);
             })) {
            fileActionsMock.when(() -> FileActions.getInstance(true)).thenReturn(mockedFileActions);
            restActionsMock.when(() -> RestActions.getResponseJSONValue(mockedResponse, "app_url")).thenReturn("bs://uploaded-app");
            sdkHelperMock.when(BrowserStackSdkHelper::generateBrowserStackYml).thenReturn("browserstack.yml");

            MutableCapabilities input = new MutableCapabilities();
            input.setCapability("custom", "native-new");
            DriverFactoryHelper helper = BrowserStackHelper.getBrowserStackDriver(input);

            DriverFactoryHelper constructed = helperConstruction.constructed().getFirst();
            Assert.assertSame(helper, constructed);
            ArgumentCaptor<MutableCapabilities> nativeCaptor = ArgumentCaptor.forClass(MutableCapabilities.class);
            Mockito.verify(constructed).initializeDriver(Mockito.eq(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE), nativeCaptor.capture());
            Assert.assertEquals(nativeCaptor.getValue().getCapability("custom"), "native-new");
            Assert.assertEquals(SHAFT.Properties.browserStack.appUrl(), "bs://uploaded-app");
        }
    }

    @Test
    public void failActionShouldThrowFailureReporterException() {
        Assert.assertThrows(Exception.class, () -> invokePrivate("failAction",
                new Class[]{String.class, Throwable[].class},
                "test-data", new Throwable[]{new RuntimeException("boom")}));
    }

    private Object invokePrivate(String methodName, Class<?>[] parameterTypes, Object... args) throws Exception {
        Method method = BrowserStackHelper.class.getDeclaredMethod(methodName, parameterTypes);
        method.setAccessible(true);
        return method.invoke(null, args);
    }
}
