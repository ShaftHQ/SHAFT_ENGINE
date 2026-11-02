package testPackage.appium;

import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.nio.file.Path;

public class MobileTestConfigurationTest {
    private static final String COMPATIBLE_PROVIDER = "shaft.mobile.compatibleProvider";
    private static final String EXECUTION_ADDRESS = "executionAddress";
    private static final String MOBILE_APP = "mobile_app";

    private boolean facadeCreated;
    private String originalCompatibleProvider;
    private String originalExecutionAddress;
    private String originalMobileApp;
    private final MobileTest fixture = new MobileTest() {
        @Override
        void createDriver() {
            facadeCreated = true;
        }
    };

    @BeforeMethod
    public void captureProperties() {
        originalCompatibleProvider = System.getProperty(COMPATIBLE_PROVIDER);
        originalExecutionAddress = System.getProperty(EXECUTION_ADDRESS);
        originalMobileApp = System.getProperty(MOBILE_APP);
    }

    @AfterMethod(alwaysRun = true)
    public void restoreProperties() {
        fixture.teardown();
        facadeCreated = false;
        restoreProperty(COMPATIBLE_PROVIDER, originalCompatibleProvider);
        restoreProperty(EXECUTION_ADDRESS, originalExecutionAddress);
        restoreProperty(MOBILE_APP, originalMobileApp);
    }

    @Test
    public void compatibleProviderShouldHonorExplicitEndpointAndApplication() {
        String app = Path.of("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk")
                .toAbsolutePath().normalize().toString();
        System.setProperty(COMPATIBLE_PROVIDER, "true");
        System.setProperty(EXECUTION_ADDRESS, "http://127.0.0.1:4723");
        System.setProperty(MOBILE_APP, app);

        fixture.setupApiDemosDebug();

        Assert.assertTrue("http://127.0.0.1:4723".equals(SHAFT.Properties.platform.executionAddress()),
                "Compatible provider endpoint was not retained.");
        Assert.assertEquals(SHAFT.Properties.mobile.app(), app);
        Assert.assertTrue(facadeCreated);
    }

    @Test
    public void defaultConfigurationShouldRemainOnBrowserStack() {
        System.clearProperty(COMPATIBLE_PROVIDER);

        fixture.setupApiDemosDebug();

        String executionAddress = SHAFT.Properties.platform.executionAddress();
        Assert.assertTrue(executionAddress != null && executionAddress.contains("browserstack"),
                "Default mobile provider was changed.");
        Assert.assertTrue(facadeCreated);
    }

    @Test
    public void compatibleProviderShouldRejectMissingEndpointBeforeCreatingFacade() {
        System.setProperty(COMPATIBLE_PROVIDER, "true");
        System.clearProperty(EXECUTION_ADDRESS);
        System.setProperty(MOBILE_APP, Path.of("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk")
                .toAbsolutePath().normalize().toString());

        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                fixture::setupApiDemosDebug);

        Assert.assertEquals(failure.getMessage(), "Compatible mobile provider requires executionAddress.");
        Assert.assertNull(MobileTest.driver.get());
        Assert.assertFalse(facadeCreated);
    }

    @Test
    public void compatibleProviderShouldRejectMissingApplicationBeforeCreatingFacade() {
        System.setProperty(COMPATIBLE_PROVIDER, "true");
        System.setProperty(EXECUTION_ADDRESS, "http://127.0.0.1:4723");
        System.clearProperty(MOBILE_APP);

        IllegalArgumentException failure = Assert.expectThrows(IllegalArgumentException.class,
                fixture::setupApiDemosDebug);

        Assert.assertEquals(failure.getMessage(), "Compatible mobile provider requires an existing mobile_app APK.");
        Assert.assertNull(MobileTest.driver.get());
        Assert.assertFalse(facadeCreated);
    }

    private static void restoreProperty(String name, String value) {
        if (value == null) {
            System.clearProperty(name);
        } else {
            System.setProperty(name, value);
        }
    }
}
