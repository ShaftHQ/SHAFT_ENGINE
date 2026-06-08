package testPackage.unitTests;

import com.shaft.db.DatabaseActions;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.BrowserStackHelper;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.LambdaTestHelper;
import com.shaft.listeners.internal.TestNGListenerHelper;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertiesHelper;
import com.shaft.tools.io.internal.ProjectStructureManager;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import org.testng.xml.XmlTest;

import java.lang.reflect.Field;
import java.util.Map;

public class DriverFactoryCoverageUnitTest {
    private final String savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
    private final String savedTargetPlatform = SHAFT.Properties.platform.targetPlatform();
    private final String savedTargetBrowserName = SHAFT.Properties.web.targetBrowserName();

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        SHAFT.Properties.platform.set()
                .executionAddress(savedExecutionAddress)
                .targetPlatform(savedTargetPlatform);
        SHAFT.Properties.web.set().targetBrowserName(savedTargetBrowserName);
        Properties.clearForCurrentThread();
    }

    @Test
    public void basicFactoryMethodsShouldCreateDriverObjects() {
        DriverFactory factory = new DriverFactory();

        SHAFT.Validations.assertThat().object(factory).isNotNull().perform();
        SHAFT.Validations.assertThat().object(DriverFactory.getAPIDriver("https://example.com")).isNotNull().perform();
        SHAFT.Validations.assertThat().object(DriverFactory.getTerminalDriver()).isNotNull().perform();
        SHAFT.Validations.assertThat()
                .object(DriverFactory.getDatabaseDriver(DatabaseActions.DatabaseType.POSTGRES_SQL, "127.0.0.1", "5432", "db", "user", "pass"))
                .isNotNull().perform();
    }

    @Test
    public void reloadPropertiesShouldTriggerSetupWhenPropertiesAreNotInitialized() throws Exception {
        Field initializedField = Properties.class.getDeclaredField("initialized");
        initializedField.setAccessible(true);
        boolean initialValue = initializedField.getBoolean(null);

        try (MockedStatic<ProjectStructureManager> projectStructureManager = org.mockito.Mockito.mockStatic(ProjectStructureManager.class);
             MockedStatic<PropertiesHelper> propertiesHelper = org.mockito.Mockito.mockStatic(PropertiesHelper.class)) {
            initializedField.setBoolean(null, false);
            projectStructureManager.when(ProjectStructureManager::identifyRunType).thenReturn(ProjectStructureManager.RunType.TESTNG);

            SHAFT.Validations.assertThat().object(DriverFactory.reloadProperties()).isEqualTo(true).perform();
            projectStructureManager.verify(ProjectStructureManager::identifyRunType);
            propertiesHelper.verify(() -> PropertiesHelper.bootstrapEngine(ProjectStructureManager.RunType.TESTNG));
        } finally {
            initializedField.setBoolean(null, initialValue);
        }
    }

    @Test
    public void reloadPropertiesShouldSkipSetupWhenAlreadyInitialized() throws Exception {
        Field initializedField = Properties.class.getDeclaredField("initialized");
        initializedField.setAccessible(true);
        boolean initialValue = initializedField.getBoolean(null);

        try (MockedStatic<ProjectStructureManager> projectStructureManager = org.mockito.Mockito.mockStatic(ProjectStructureManager.class);
             MockedStatic<PropertiesHelper> propertiesHelper = org.mockito.Mockito.mockStatic(PropertiesHelper.class)) {
            initializedField.setBoolean(null, true);
            SHAFT.Validations.assertThat().object(DriverFactory.reloadProperties()).isEqualTo(true).perform();
            projectStructureManager.verify(ProjectStructureManager::identifyRunType, org.mockito.Mockito.never());
            propertiesHelper.verify(() -> PropertiesHelper.bootstrapEngine(org.mockito.ArgumentMatchers.any()), org.mockito.Mockito.never());
        } finally {
            initializedField.setBoolean(null, initialValue);
        }
    }

    @Test
    public void getHelperShouldCacheLocalHelperInstance() {
        SHAFT.Properties.platform.set().executionAddress("local");
        DriverFactory factory = new DriverFactory();

        try (MockedConstruction<DriverFactoryHelper> helperConstruction = org.mockito.Mockito.mockConstruction(DriverFactoryHelper.class,
                (mock, context) -> {
                    WebDriver driver = org.mockito.Mockito.mock(WebDriver.class);
                    org.mockito.Mockito.when(mock.getDriver()).thenReturn(driver);
                })) {
            DriverFactoryHelper helperOne = factory.getHelper();
            DriverFactoryHelper helperTwo = factory.getHelper();

            SHAFT.Validations.assertThat().object(helperOne).isEqualTo(helperTwo).perform();
            SHAFT.Validations.assertThat().object(helperConstruction.constructed().size()).isEqualTo(1).perform();
            org.mockito.Mockito.verify(helperOne).initializeDriver();
        }
    }

    @Test
    public void getHelperShouldRouteToBrowserStackAndLambdaTest() {
        DriverFactoryHelper browserStackHelper = org.mockito.Mockito.mock(DriverFactoryHelper.class);
        DriverFactoryHelper lambdaTestHelper = org.mockito.Mockito.mock(DriverFactoryHelper.class);

        try (MockedStatic<BrowserStackHelper> browserStackStatic = org.mockito.Mockito.mockStatic(BrowserStackHelper.class);
             MockedStatic<LambdaTestHelper> lambdaStatic = org.mockito.Mockito.mockStatic(LambdaTestHelper.class)) {
            browserStackStatic.when(() -> BrowserStackHelper.getBrowserStackDriver(org.mockito.ArgumentMatchers.any(MutableCapabilities.class)))
                    .thenReturn(browserStackHelper);
            lambdaStatic.when(() -> LambdaTestHelper.getLambdaTestDriver(org.mockito.ArgumentMatchers.any(MutableCapabilities.class)))
                    .thenReturn(lambdaTestHelper);

            SHAFT.Properties.platform.set().executionAddress("browserstack");
            DriverFactory browserStackFactory = new DriverFactory();
            SHAFT.Validations.assertThat().object(browserStackFactory.getHelper()).isEqualTo(browserStackHelper).perform();

            SHAFT.Properties.platform.set().executionAddress("lambdatest");
            DriverFactory lambdaFactory = new DriverFactory();
            SHAFT.Validations.assertThat().object(lambdaFactory.getHelper()).isEqualTo(lambdaTestHelper).perform();
        }
    }

    @Test
    public void getHelperByTypeAndDriverAndGetDriverShouldDelegateCorrectly() {
        DriverFactory factory = new DriverFactory();
        DriverFactoryHelper browserStackHelper = org.mockito.Mockito.mock(DriverFactoryHelper.class);
        DriverFactoryHelper lambdaTestHelper = org.mockito.Mockito.mock(DriverFactoryHelper.class);
        WebDriver attachedDriver = org.mockito.Mockito.mock(WebDriver.class);
        WebDriver activeDriver = org.mockito.Mockito.mock(WebDriver.class);
        DriverFactoryHelper storedHelper = org.mockito.Mockito.mock(DriverFactoryHelper.class);
        org.mockito.Mockito.when(storedHelper.getDriver()).thenReturn(activeDriver);

        try (MockedStatic<BrowserStackHelper> browserStackStatic = org.mockito.Mockito.mockStatic(BrowserStackHelper.class);
             MockedStatic<LambdaTestHelper> lambdaStatic = org.mockito.Mockito.mockStatic(LambdaTestHelper.class);
             MockedConstruction<DriverFactoryHelper> helperConstruction = org.mockito.Mockito.mockConstruction(DriverFactoryHelper.class)) {
            browserStackStatic.when(() -> BrowserStackHelper.getBrowserStackDriver(org.mockito.ArgumentMatchers.any(MutableCapabilities.class)))
                    .thenReturn(browserStackHelper);
            lambdaStatic.when(() -> LambdaTestHelper.getLambdaTestDriver(org.mockito.ArgumentMatchers.any(MutableCapabilities.class)))
                    .thenReturn(lambdaTestHelper);

            MutableCapabilities customDriverOptions = new MutableCapabilities();
            SHAFT.Validations.assertThat().object(factory.getHelper(DriverFactory.DriverType.BROWSERSTACK, customDriverOptions))
                    .isEqualTo(browserStackHelper).perform();
            SHAFT.Validations.assertThat().object(factory.getHelper(DriverFactory.DriverType.LAMBDATEST, customDriverOptions))
                    .isEqualTo(lambdaTestHelper).perform();

            DriverFactoryHelper localHelper = factory.getHelper(DriverFactory.DriverType.CHROME, customDriverOptions);
            org.mockito.Mockito.verify(localHelper).initializeDriver(DriverFactory.DriverType.CHROME, customDriverOptions);

            DriverFactoryHelper attachedHelper = factory.getHelper(attachedDriver);
            org.mockito.Mockito.verify(attachedHelper).initializeDriver(attachedDriver);

            factory.setHelper(storedHelper);
            SHAFT.Validations.assertThat().object(factory.getDriver()).isEqualTo(activeDriver).perform();
            SHAFT.Validations.assertThat().object(helperConstruction.constructed().size()).isEqualTo(2).perform();
        }
    }

    @Test
    public void getHelperShouldApplyTestNGXmlParametersAndInferPlatformFromTestName() {
        SHAFT.Properties.platform.set().targetPlatform("WINDOWS");
        DriverFactory factory = new DriverFactory();
        XmlTest xmlTest = org.mockito.Mockito.mock(XmlTest.class);
        MutableCapabilities customDriverOptions = new MutableCapabilities();

        try (MockedStatic<TestNGListenerHelper> listenerHelper = org.mockito.Mockito.mockStatic(TestNGListenerHelper.class);
             MockedConstruction<DriverFactoryHelper> helperConstruction = org.mockito.Mockito.mockConstruction(DriverFactoryHelper.class)) {
            listenerHelper.when(TestNGListenerHelper::getXmlTest).thenReturn(xmlTest);
            org.mockito.Mockito.when(xmlTest.getAllParameters()).thenReturn(Map.of("targetBrowserName", "firefox"));
            listenerHelper.when(TestNGListenerHelper::getTestName).thenReturn("Smoke_Chrome");

            DriverFactoryHelper helper = factory.getHelper(DriverFactory.DriverType.CHROME, customDriverOptions);
            org.mockito.Mockito.verify(helper).initializeDriver(DriverFactory.DriverType.CHROME, customDriverOptions);

            SHAFT.Validations.assertThat().object(SHAFT.Properties.platform.targetPlatform()).isEqualTo("LINUX").perform();
            SHAFT.Validations.assertThat().object(helperConstruction.constructed().size()).isEqualTo(1).perform();
        }
    }

    @Test
    public void driverTypeValuesShouldExposeExpectedMappings() {
        SHAFT.Validations.assertThat().object(DriverFactory.DriverType.CHROME.getValue()).isEqualTo("chrome").perform();
        SHAFT.Validations.assertThat().object(DriverFactory.DriverType.BROWSERSTACK.getValue()).isEqualTo("BrowserStack").perform();
        SHAFT.Validations.assertThat().object(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE.getValue()).isEqualTo("NativeMobileApp").perform();
    }
}
