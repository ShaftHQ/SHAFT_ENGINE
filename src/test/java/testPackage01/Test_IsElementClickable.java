package testPackage01;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionType;

public class Test_IsElementClickable {

	private WebDriver driver;
	
	private By firstName = By.id("firstname");
	private By lastName = By.id("lastname");
	private By emailAddress = By.id("email");
	private By password = By.id("password");
	private By registerButton = By.xpath("//button[contains(@class, 'register')]");
	private By checkBox1 = By.xpath("//label[@for='checkbox1']");
	
	@Test
	public void testIsButtonClickable() {
		driver = BrowserFactory.getBrowser();
		BrowserActions.navigateToURL(driver, "https://www.gocardi.com/register");
	
		ElementActions.type(driver, firstName, "nameOne");
		ElementActions.type(driver, lastName, "nameTwo");
		ElementActions.type(driver, emailAddress, "test@gmail.com");
		ElementActions.type(driver, password, "test123456");
		ElementActions.click(driver, checkBox1);
		Assertions.assertTrue(ElementActions.isElementClickable(driver, registerButton), AssertionType.POSITIVE);
	}
	
	@Test
	public void testIsButtonNotClickable() {
		driver = BrowserFactory.getBrowser();
		BrowserActions.navigateToURL(driver, "https://www.gocardi.com/register");		
	
		ElementActions.type(driver, firstName, "nameOne");
		ElementActions.type(driver, lastName, "nameTwo");
		ElementActions.type(driver, emailAddress, "test@gmail.com");
		ElementActions.type(driver, password, "test123456");
		Assertions.assertTrue(ElementActions.isElementClickable(driver, registerButton), AssertionType.NEGATIVE);
	}
	
	@Test
    public void testIsElementClickable() {
		driver = BrowserFactory.getBrowser();
		BrowserActions.navigateToURL(driver, "https://the-internet.herokuapp.com/");
		Assertions.assertTrue(ElementActions.isElementClickable(driver, By.linkText("File Upload")), AssertionType.POSITIVE);
    }
	
	@AfterMethod
	public void afterMathod() {
		BrowserActions.closeCurrentWindow(driver);
	}
}
