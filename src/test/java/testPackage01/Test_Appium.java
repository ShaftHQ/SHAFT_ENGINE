package testPackage01;

import org.openqa.selenium.By;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionType;

import io.appium.java_client.MobileElement;
import io.appium.java_client.android.AndroidDriver;

public class Test_Appium {
    private AndroidDriver<MobileElement> driver;

    private By accessibility_button = By.xpath("//android.widget.TextView[@content-desc='Accessibility']");

    @Test
    public void firstAppiumTest() {
	Assertions.assertElementExists(driver, accessibility_button, AssertionType.POSITIVE);
	ElementActions.click(driver, accessibility_button);
    }

    @SuppressWarnings("unchecked")
    @BeforeMethod
    public void setup() {
	driver = (AndroidDriver<MobileElement>) BrowserFactory.getBrowser();
    }
}
