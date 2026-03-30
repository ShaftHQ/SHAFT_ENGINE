package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.DataProvider;
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

    @Test(description = "initializeSystemProperties should keep explicit BrowserStack /wd/hub execution URL unchanged")
    public void initializeSystemProperties_keepsExplicitBrowserStackWdHubUrlUnchanged() throws Exception {
        savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
        String explicitBrowserStackUrl = "http://user:key@hub-cloud.browserstack.com/wd/hub";
        SHAFT.Properties.platform.set().executionAddress(explicitBrowserStackUrl);

        DriverFactoryHelper.initializeSystemProperties();

        SHAFT.Validations.assertThat().object(getTargetHubUrl()).isEqualTo(explicitBrowserStackUrl).perform();
    }

    @DataProvider(name = "remoteServerPingBaseUrls")
    public Object[][] remoteServerPingBaseUrls() {
        return new Object[][]{
                {"127.0.0.1:4723", "http://127.0.0.1:4723/"},
                {"http://127.0.0.1:4723", "http://127.0.0.1:4723/"},
                {"http://127.0.0.1:4723/wd/hub", "http://127.0.0.1:4723/wd/hub/"}
        };
    }

    @Test(description = "normalizeRemoteServerPingBaseUrl should normalize supported execution address formats for ping", dataProvider = "remoteServerPingBaseUrls")
    public void normalizeRemoteServerPingBaseUrl_handlesVariousFormats(String executionAddress, String expectedPingBaseUrl) throws Exception {
        Method normalizeRemoteServerPingBaseUrlMethod = DriverFactoryHelper.class.getDeclaredMethod("normalizeRemoteServerPingBaseUrl", String.class);
        normalizeRemoteServerPingBaseUrlMethod.setAccessible(true);

        String actualPingBaseUrl = (String) normalizeRemoteServerPingBaseUrlMethod.invoke(null, executionAddress);

        SHAFT.Validations.assertThat().object(actualPingBaseUrl).isEqualTo(expectedPingBaseUrl).perform();
    }

    @Test(description = "initializeSystemProperties should trim whitespace from execution address")
    public void initializeSystemProperties_trimsWhitespace() throws Exception {
        savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
        SHAFT.Properties.platform.set().executionAddress("  http://localhost:4444  ");

        DriverFactoryHelper.initializeSystemProperties();

        SHAFT.Validations.assertThat().object(getTargetHubUrl()).isEqualTo("http://localhost:4444").perform();
    }

    @Test(description = "initializeSystemProperties should preserve https scheme without corruption")
    public void initializeSystemProperties_preservesHttpsScheme() throws Exception {
        savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
        SHAFT.Properties.platform.set().executionAddress("https://user:key@hub-cloud.browserstack.com/wd/hub");

        DriverFactoryHelper.initializeSystemProperties();

        SHAFT.Validations.assertThat().object(getTargetHubUrl())
                .isEqualTo("https://user:key@hub-cloud.browserstack.com/wd/hub").perform();
    }

    @Test(description = "redactUriCredentials should mask user-info in URL")
    public void redactUriCredentials_masksCredentials() throws Exception {
        Method redactMethod = DriverFactoryHelper.class.getDeclaredMethod("redactUriCredentials", String.class);
        redactMethod.setAccessible(true);

        String redacted = (String) redactMethod.invoke(null, "https://myuser:mypass@hub-cloud.browserstack.com/wd/hub");

        SHAFT.Validations.assertThat().object(redacted)
                .isEqualTo("https://***:***@hub-cloud.browserstack.com/wd/hub").perform();
    }

    @Test(description = "redactUriCredentials should return URL unchanged when no credentials present")
    public void redactUriCredentials_noCredentials() throws Exception {
        Method redactMethod = DriverFactoryHelper.class.getDeclaredMethod("redactUriCredentials", String.class);
        redactMethod.setAccessible(true);

        String result = (String) redactMethod.invoke(null, "http://localhost:4723/wd/hub");

        SHAFT.Validations.assertThat().object(result).isEqualTo("http://localhost:4723/wd/hub").perform();
    }

    private static String getTargetHubUrl() throws NoSuchFieldException, IllegalAccessException {
        Field targetHubUrlField = DriverFactoryHelper.class.getDeclaredField("TARGET_HUB_URL");
        targetHubUrlField.setAccessible(true);
        return (String) targetHubUrlField.get(null);
    }
}
