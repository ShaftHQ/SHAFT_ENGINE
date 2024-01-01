package testPackage.SHAFTWizard;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.testng.asserts.SoftAssert;
import poms.GoogleSearch;

public class GUIWizardTests {
    SHAFT.GUI.WebDriver driver;
    SHAFT.TestData.JSON testData;

    By searchBox = GoogleSearch.getSearchBox_textField();
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
        driver.browser().navigateToURL("https://www.saucedemo.com")
                .element().type(emailField, "standard_user")
                .typeSecure(passwordField, "secret_sauce")
                .clickUsingJavascript(loginButton)
                .clickUsingJavascript(product1)
                .clickUsingJavascript(shoppingCartButton)
                .verifyThat(productName).text().isEqualTo("Sauce Labs Backpack").perform();
    }

    @Test
    public void test_selectFromDropdownList(){
        driver.browser().navigateToURL("http://the-internet.herokuapp.com/dropdown");
       //"1" is attribute value string value
        driver.element().select(By.id("dropdown"), "1");
        driver.element().captureScreenshot(By.id("dropdown"));
        //"Option 2" is the displayed text of "option 2"
        driver.element().select(By.id("dropdown"), "Option 2");
        driver.element().captureScreenshot(By.id("dropdown"));

    }

    @Test (expectedExceptions =  {AssertionError.class})
    public void test_selectOptionNotExistFromDropdownList(){
        driver.browser().navigateToURL("http://the-internet.herokuapp.com/dropdown");
        //"1" is attribute value string value
        driver.element().select(By.id("dropdown"), "1");
        driver.element().captureScreenshot(By.id("dropdown"));
        //"Option 2" is the displayed text of "option 2"
        driver.element().select(By.id("dropdown"), "Option 4");
        driver.element().captureScreenshot(By.id("dropdown"));

    }


    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.quit();
    }
}
