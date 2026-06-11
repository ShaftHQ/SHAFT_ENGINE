package testPackage.SHAFTWizard;

import org.openqa.selenium.By;
import org.testng.annotations.Test;
import testPackage.Tests;

public class GUIWizardTests extends Tests {
    //locators of test_ClickUsingJavaScript
    By emailField = By.xpath("//input[@name='user-name']");
    By passwordField = By.xpath("//input[@name='password']");
    By loginButton = By.xpath("//input[@name='login-button']");
	By product1 = By.xpath("//button[@name='add-to-cart-sauce-labs-backpack']");
	By shoppingCartButton = By.xpath("//a[@class='shopping_cart_link']");
	By productName = By.xpath("//div[@class='inventory_item_name']");

    @Test
    public void test_ClickUsingJavaScript() {
        driver.get().browser().navigateToURL("https://www.saucedemo.com")
                .element().type(emailField, "standard_user")
                .typeSecure(passwordField, "secret_sauce")
                .clickUsingJavascript(loginButton)
                .clickUsingJavascript(product1)
                .clickUsingJavascript(shoppingCartButton)
                .verifyThat(productName).text().isEqualTo("Sauce Labs Backpack").perform();
    }

    @Test
    public void test_selectFromDropdownList(){
        driver.get().browser().navigateToURL("http://the-internet.herokuapp.com/dropdown");
       //"1" is attribute value string value
        driver.get().element().select(By.id("dropdown"), "1");
        driver.get().element().captureScreenshot(By.id("dropdown"));
        //"Option 2" is the displayed text of "option 2"
        driver.get().element().select(By.id("dropdown"), "Option 2");
        driver.get().element().captureScreenshot(By.id("dropdown"));

    }

    @Test (expectedExceptions =  {AssertionError.class})
    public void test_selectOptionNotExistFromDropdownList(){
        driver.get().browser().navigateToURL("http://the-internet.herokuapp.com/dropdown");
        //"1" is attribute value string value
        driver.get().element().select(By.id("dropdown"), "1");
        driver.get().element().captureScreenshot(By.id("dropdown"));
        //"Option 2" is the displayed text of "option 2"
        driver.get().element().select(By.id("dropdown"), "Option 4");
        driver.get().element().captureScreenshot(By.id("dropdown"));

    }
}
