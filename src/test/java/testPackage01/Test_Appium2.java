package testPackage01;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions.SwipeDirection;
import com.shaft.validation.Verifications;
import com.shaft.validation.Verifications.VerificationType;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_Appium2 {
    private final By accessibilityButton = By.xpath("//android.widget.TextView[@content-desc='Accessibility']");
    private final By viewsButton = By.xpath("//android.widget.TextView[@content-desc='Views']");
    private final By webView3 = By.xpath("//android.widget.TextView[@content-desc='WebView3']");
    private WebDriver driver;

    @Test
    public void verifyLandingPageContent() {
        Verifications.verifyElementExists(driver, accessibilityButton, VerificationType.POSITIVE);
        ElementActions.performTouchAction(driver).swipeElementIntoView(viewsButton, SwipeDirection.DOWN);
        Verifications.verifyElementExists(driver, viewsButton, VerificationType.POSITIVE);
        ElementActions.performTouchAction(driver).tap(viewsButton);
        ElementActions.performTouchAction(driver).swipeElementIntoView(webView3, SwipeDirection.DOWN).tap(webView3);
    }

    @BeforeClass
    public void setup() {
        driver = BrowserFactory.getBrowser();
    }

    @AfterClass
    public void teardown() {
        BrowserActions.closeCurrentWindow(driver);
    }
}
