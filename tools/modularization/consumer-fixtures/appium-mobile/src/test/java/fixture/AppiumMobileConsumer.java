package fixture;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.MutableCapabilities;

public class AppiumMobileConsumer {
    public SHAFT.GUI.WebDriver createDriver() {
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("appium:automationName", "UiAutomator2");
        AppiumBy.accessibilityId("login");
        return new SHAFT.GUI.WebDriver(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE, capabilities);
    }
}
