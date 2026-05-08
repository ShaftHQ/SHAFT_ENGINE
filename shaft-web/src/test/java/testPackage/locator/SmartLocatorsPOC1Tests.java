package testPackage.locator;

import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.support.locators.RelativeLocator;
import org.openqa.selenium.support.pagefactory.ByAll;
import org.testng.annotations.Ignore;
import testPackage.Tests;

@Ignore
public class SmartLocatorsPOC1Tests extends Tests {

    public void testSmartLocators1(){
        driver.get().browser().navigateToURL("https://www.saucedemo.com/v1/index.html");
        type("Username", "standard_user");
        type("Password", "secret_sauce");
        click("LOGIN");
    }

    public void testSmartLocators2(){
        driver.get().browser().navigateToURL("https://practicetestautomation.com/practice-test-login/");
        type("Username", "student");
        type("Password", "Password123");
        click("Submit");
    }

    public void testSmartLocators3(){
        driver.get().browser().navigateToURL("http://testphp.vulnweb.com/login.php");
        type("Username", "test");
        type("Password", "test");
        click("login");
    }

    public void testSmartLocators4(){
        driver.get().browser().navigateToURL("http://vbsca.ca/login/login.asp");
        type("Username", "test");
        type("Password", "test");
        click("Login");
    }

    public void testSmartLocators5(){
        driver.get().browser().navigateToURL("https://www.facebook.com/login.php/");
        type("Email", "test");
        type("Password", "test");
        click("Log in");
    }

    public void testSmartLocators6(){
        driver.get().browser().navigateToURL("https://www.selenium.dev/selenium/web/web-form.html");
        type("Text input", "text");
        type("Password", "password");
        type("Textarea", "a lot\nof text");
        type("Dropdown (datalist)", "New York");
        type("Date picker", "02/26/2025");
        click("Submit");
    }

    public void testSmartLocators7(){
        driver.get().browser().navigateToURL("https://duckduckgo.com/");
        type("Search", "Selenium" + Keys.ENTER);
    }

    public void testSmartLocators8(){
        driver.get().browser().navigateToURL("https://www.selenium.dev/selenium/web/login.html");
        type("Username", "test");
        type("Password", "test");
        click("Login");
    }

    public void testSmartLocators9(){
        driver.get().browser().navigateToURL("https://moatazeldebsy.github.io/test-automation-practices/#/forms");
        type("Username", "USERNAME");
        type("Email", "EMAIL@DOMAIN.COM");
        type("Password", "PASSWORD");
        click("Sign In");
    }

    private void type(String element, CharSequence text){
        By smartLocator = new ByAll(
                // input
                By.xpath("//input[contains(@placeholder,'"+element+"')]|//input[contains(@placeholder,'"+element.toLowerCase()+"')]|//input[contains(@placeholder,'"+element.toUpperCase()+"')]")
                ,By.xpath("//input[contains(@id,'"+element+"')]|//input[contains(@id,'"+element.toLowerCase()+"')]|//input[contains(@id,'"+element.toUpperCase()+"')]")
                ,By.xpath("(//*[contains(text(),'"+element+"')]/input)[1]|(//*[contains(text(),'"+element.toLowerCase()+"')]/input)[1]|(//*[contains(text(),'"+element.toUpperCase()+"')]/input)[1]")
                // text area
                ,By.xpath("//textarea[contains(@placeholder,'"+element+"')]|//textarea[contains(@placeholder,'"+element.toLowerCase()+"')]|//textarea[contains(@placeholder,'"+element.toUpperCase()+"')]")
                ,By.xpath("//textarea[contains(@id,'"+element+"')]|//textarea[contains(@id,'"+element.toLowerCase()+"')]|//textarea[contains(@id,'"+element.toUpperCase()+"')]")
                ,By.xpath("(//*[contains(text(),'"+element+"')]/textarea)[1]|(//*[contains(text(),'"+element.toLowerCase()+"')]/textarea)[1]|(//*[contains(text(),'"+element.toUpperCase()+"')]/textarea)[1]")
                // relative input, straight right of
                ,RelativeLocator.with(By.tagName("input")).straightRightOf(By.xpath("//*[contains(text(),'"+element+"')]|//*[contains(text(),'"+element.toLowerCase()+"')]|//*[contains(text(),'"+element.toUpperCase()+"')]"))
                // following input
                ,By.xpath("(//*[contains(text(),'"+element+"')]/following::input)[1]|(//*[contains(text(),'"+element.toLowerCase()+"')]/following::input)[1]|(//*[contains(text(),'"+element.toUpperCase()+"')]/following::input)[1]")
        );
        var elements = driver.get().getDriver().findElements(smartLocator);
        elements.getFirst().sendKeys(text);
    }

    private void click(String element){
        By smartLocator = new ByAll(
                By.xpath("//input[contains(@value,'"+element+"')]|//input[contains(@value,'"+element.toLowerCase()+"')]|//input[contains(@value,'"+element.toUpperCase()+"')]")
                ,By.xpath("//input[contains(@id,'"+element+"')]|//input[contains(@id,'"+element.toLowerCase()+"')]|//input[contains(@id,'"+element.toUpperCase()+"')]")
                ,By.xpath("//button[contains(text(),'"+element+"')]|//button[contains(text(),'"+element.toLowerCase()+"')]|//button[contains(text(),'"+element.toUpperCase()+"')]")
                ,By.xpath("//button[@id='"+element+"']|//button[@id='"+element.toLowerCase()+"']|//button[@id='"+element.toUpperCase()+"']")
        );
        var elements = driver.get().getDriver().findElements(smartLocator);
        elements.getFirst().click();
    }

}
