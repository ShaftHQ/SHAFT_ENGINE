package testPackage.SHAFTWizard;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import org.testng.asserts.SoftAssert;

public class Test_Wizard_GUI {
    SHAFT.GUI.WebDriver driver;
    SHAFT.TestData.JSON testData;

    By searchBox = By.name("q");
    By resultStats = By.id("result-stats");
    
    //locators of test_ClickUsingJavaScript
    By emailField = By.xpath("//input[@name='user-name']");
    By passwordField = By.xpath("//input[@name='password']");
    By loginButton = By.xpath("//input[@name='login-button']");
	By product1 = By.xpath("//button[@name='add-to-cart-sauce-labs-backpack']");
	By shoppingCartButton = By.xpath("//a[@class='shopping_cart_link']");
	By productName = By.xpath("//div[@class='inventory_item_name']");

    @Test
    public void test() {
        driver.browser().navigateToURL("https://www.google.com/");
        driver.verifyThat().browser().title().isEqualTo("Google").perform();
        driver.element().type(searchBox, testData.getTestData("searchQuery"))
                .keyPress(searchBox, Keys.ENTER);
        driver.assertThat().element(resultStats).text().doesNotEqual("").withCustomReportMessage("Check that result stats is not empty").perform();
    }

    //@Test
    public void test_nativeDriver() {
        WebDriver nativeWebDriver = driver.getDriver();
        nativeWebDriver.navigate().to("https://www.google.com/");
        new SoftAssert().assertEquals(nativeWebDriver.getTitle(), "Google");
        nativeWebDriver.findElement(searchBox).sendKeys(testData.getTestData("searchQuery") + Keys.ENTER);
        Assert.assertNotEquals(nativeWebDriver.findElement(resultStats).getText(), "");
    }
    
    @Test
    public void test_ClickUsingJavaScript() {
    	driver.browser().navigateToURL("https://www.saucedemo.com");
		driver.element().type(emailField, "standard_user");
		driver.element().type(passwordField, "secret_sauce");
		driver.element().clickUsingJavascript(driver.getDriver(), loginButton);
		driver.element().clickUsingJavascript(driver.getDriver(), product1);
		driver.element().clickUsingJavascript(driver.getDriver(), shoppingCartButton);
		driver.verifyThat().element(productName).text().equals("Sauce Labs Backpack");
		  }


    @BeforeClass
    public void beforeClass() {
        driver = new SHAFT.GUI.WebDriver();
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @AfterClass
    public void afterClass() {
        driver.quit();
    }
}
