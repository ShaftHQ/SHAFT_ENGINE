package testPackage.unitTests;

import com.shaft.driver.internal.DriverFactory.BrowserStackHelper;
import org.testng.annotations.Test;

import java.lang.reflect.Method;

public class BrowserStackHelperUnitTest {
    @Test(description = "BrowserStack execution address should use https and include /wd/hub path")
    public void constructExecutionAddress_usesHttpsAndWdHubPath() throws Exception {
        Method constructExecutionAddressMethod = BrowserStackHelper.class.getDeclaredMethod("constructExecutionAddress", String.class, String.class);
        constructExecutionAddressMethod.setAccessible(true);

        String executionAddress = (String) constructExecutionAddressMethod.invoke(null, "user123", "key456");

        com.shaft.driver.SHAFT.Validations.assertThat().object(executionAddress)
                .isEqualTo("https://user123:key456@hub-cloud.browserstack.com/wd/hub").perform();
    }
}
