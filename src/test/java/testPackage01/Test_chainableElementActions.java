package testPackage01;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Verifications;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;

public class Test_chainableElementActions {
    @Test
    public void chainElementActions() {
        WebDriver driver = BrowserFactory.getBrowser(BrowserFactory.BrowserType.GOOGLE_CHROME);
        BrowserActions.navigateToURL(driver, "https://jqueryui.com/droppable/");
        new ElementActions(driver).type(By.name("s"), "chained type 1").type(By.name("s"), "chained type 2");
        Assertions.assertElementAttribute(driver, By.name("s"), "text", "chained type 2");
    }

    @Test
    public void chainElementActions_2() {
        WebDriver driver = BrowserFactory.getBrowser(BrowserFactory.BrowserType.GOOGLE_CHROME);
        driver.findElement(By.xpath("")).getAttribute("");
        BrowserActions.navigateToURL(driver, "https://www.phptravels.net/home");
        ElementActions.click(driver, By.xpath("//div[contains(@class,'hotelsearch')]/a"));
        Verifications.verifyElementAttribute(driver, By.xpath("//input[@required][@name='dest']"), "validationMessage", "Please fill out this field.", Verifications.VerificationComparisonType.EQUALS, Verifications.VerificationType.NEGATIVE);
        new ElementActions(driver).click(By.xpath("//form[@name='HOTELS']//button[@type='submit']"));
        Verifications.verifyElementAttribute(driver, By.xpath("//input[@required][@name='dest']"), "validationMessage", "Please fill out this field.");

        Assertions.assertElementMatches(driver, By.xpath(""), Assertions.VisualValidationEngine.EXACT_OPENCV, Assertions.AssertionType.POSITIVE);

    }
}
