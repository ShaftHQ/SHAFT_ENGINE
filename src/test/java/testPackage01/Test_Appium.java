package testPackage01;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionComparisonType;
import com.shaft.validation.Assertions.AssertionType;
import com.shaft.validation.Verifications;
import com.shaft.validation.Verifications.VerificationComparisonType;
import com.shaft.validation.Verifications.VerificationType;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_Appium {
    private WebDriver driver;

    private final By applicationLogo = By.xpath("//android.widget.ImageView");
    private final By applicationText = By.xpath("//android.widget.TextView");
    private final By email = By.id("textInputEditTextEmail");
    private final By password = By.id("textInputEditTextPassword");
    private final By loginButton = By.id("appCompatButtonLogin");
    private final By popup = By.id("snackbar_text");

    @Test
    public void verifyLandingPageContent() {
        Verifications.verifyElementExists(driver, applicationLogo, VerificationType.POSITIVE);
        Verifications.verifyElementAttribute(driver, applicationText, "Text", "VERSION - V4",
                VerificationComparisonType.EQUALS, VerificationType.POSITIVE);
        Verifications.verifyElementExists(driver, email, VerificationType.POSITIVE);
        Verifications.verifyElementExists(driver, password, VerificationType.POSITIVE);
        Verifications.verifyElementExists(driver, loginButton, VerificationType.POSITIVE);
    }

    @Test
    public void login() {
        ElementActions.type(driver, email, "Mohab.MohieElDeen@outlook.com");
        ElementActions.typeSecure(driver, password, "DummyPassword");
        ElementActions.performTouchAction(driver).tap(loginButton);
        Assertions.assertElementAttribute(driver, popup, "Text", "Wrong Email or Password",
                AssertionComparisonType.EQUALS, AssertionType.POSITIVE);
    }

    @BeforeClass
    public void setup() {
        driver = BrowserFactory.getBrowser();
    }

    @AfterClass
    public void teardown(){
        BrowserActions.closeCurrentWindow(driver);
    }
}
