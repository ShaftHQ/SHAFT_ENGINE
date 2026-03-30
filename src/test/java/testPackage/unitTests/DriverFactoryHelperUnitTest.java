package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.BrowserStackHelper;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

public class DriverFactoryHelperUnitTest {
    private String savedExecutionAddress;

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        if (savedExecutionAddress != null) {
            SHAFT.Properties.platform.set().executionAddress(savedExecutionAddress);
            DriverFactoryHelper.initializeSystemProperties();
        }
    }

    @Test(description = "initializeSystemProperties should not append a trailing slash for browserstack keyword execution address")
    public void initializeSystemProperties_doesNotAppendTrailingSlashForBrowserStackKeyword() throws Exception {
        savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
        SHAFT.Properties.platform.set().executionAddress("browserstack");

        DriverFactoryHelper.initializeSystemProperties();

        SHAFT.Validations.assertThat().object(getTargetHubUrl()).isEqualTo("http://browserstack").perform();
    }

    @Test(description = "BrowserStack helper should construct final execution address with explicit /wd/hub path")
    public void browserStackHelper_constructsFinalExecutionAddressWithWdHubPath() throws Exception {
        Method constructExecutionAddressMethod = BrowserStackHelper.class.getDeclaredMethod("constructExecutionAddress", String.class, String.class);
        constructExecutionAddressMethod.setAccessible(true);

        String executionAddress = (String) constructExecutionAddressMethod.invoke(null, "user123", "key456");

        SHAFT.Validations.assertThat().object(executionAddress)
                .isEqualTo("https://user123:key456@hub-cloud.browserstack.com/wd/hub").perform();
    }

    @Test(description = "initializeSystemProperties should keep explicit BrowserStack /wd/hub execution URL unchanged")
    public void initializeSystemProperties_keepsExplicitBrowserStackWdHubUrlUnchanged() throws Exception {
        savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
        String explicitBrowserStackUrl = "http://user:key@hub-cloud.browserstack.com/wd/hub";
        SHAFT.Properties.platform.set().executionAddress(explicitBrowserStackUrl);

        DriverFactoryHelper.initializeSystemProperties();

        SHAFT.Validations.assertThat().object(getTargetHubUrl()).isEqualTo(explicitBrowserStackUrl).perform();
    }

    private static String getTargetHubUrl() throws NoSuchFieldException, IllegalAccessException {
        Field targetHubUrlField = DriverFactoryHelper.class.getDeclaredField("TARGET_HUB_URL");
        targetHubUrlField.setAccessible(true);
        return (String) targetHubUrlField.get(null);
    }
}
