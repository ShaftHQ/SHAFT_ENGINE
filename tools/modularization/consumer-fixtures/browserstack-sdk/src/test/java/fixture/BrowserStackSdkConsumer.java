package fixture;

import com.browserstack.config.Constants;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;

public class BrowserStackSdkConsumer {
    public SHAFT.GUI.WebDriver createBrowserStackDriver() {
        Constants.class.getName();
        SHAFT.Properties.browserStack.set().browserstackAutomation(true);
        return new SHAFT.GUI.WebDriver(DriverFactory.DriverType.BROWSERSTACK);
    }
}
