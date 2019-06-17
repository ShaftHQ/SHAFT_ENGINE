package testPackage01;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.browser.BrowserActions;
import com.shaft.browser.BrowserFactory;
import com.shaft.element.ElementActions;

public class Test_reactDnD {
	// Declaring webdriver instance
	WebDriver driver;

	@Test
	public void testReactDnD() {
		BrowserActions.navigateToURL(driver, "http://react-dnd.github.io/react-dnd/examples-chessboard-tutorial-app.html");
		ElementActions.dragAndDrop(driver, By.xpath("//div[@draggable='true']"), By.xpath("/html/body/div/div/div/div[2]/div/div/div[43]/div/div"));
	}

	@BeforeClass // Set-up method, to be run once before the first test
	public void beforeClass() {
		driver = BrowserFactory.getBrowser();
	}
}
