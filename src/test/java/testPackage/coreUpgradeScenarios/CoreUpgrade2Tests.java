package testPackage.coreUpgradeScenarios;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class CoreUpgrade2Tests {
    SHAFT.GUI.WebDriver driver;

    By firstNameInput = By.xpath("//input[@ng-model='FirstName']");
    By lastNameInput = By.xpath("//input[@ng-model='LastName']");
    By addressInput = By.xpath("//textarea[@ng-model='Adress']");
    By emailInput = By.xpath("//input[@ng-model='EmailAdress']");
    By phoneInput = By.xpath("//input[@ng-model='Phone']");
    By maleRadioButton = By.xpath("//input[@value='Male']");
    By moviesCheckbox = By.xpath("//input[@value='Movies']");
    By countryDropdown = By.xpath("//span[@aria-labelledby='select2-country-container']");
    By countrySearchInput = By.xpath("//input[@type='search']");
    By passwordInput = By.id("firstpassword");
    By confirmPasswordInput = By.id("secondpassword");
    By submitButton = By.id("submitbtn");

//    @Test
//    public void newCore(){
//        driver.newActions().type(firstNameInput, "John")
//                        .type(lastNameInput, "Doe")
//                        .type(addressInput, "Address")
//                        .type(emailInput, "john.doe@gmail.com")
//                        .type(phoneInput, "12345")
//                        .click(maleRadioButton)
//                        .click(moviesCheckbox)
//                        .click(countryDropdown)
//                        .type(countrySearchInput, "Japan" + Keys.ENTER)
//                        .type(passwordInput, "12345")
//                        .type(confirmPasswordInput, "12345")
//                        .click(submitButton);
//    }

    @Test
    public void oldCore(){
        driver.element().type(firstNameInput, "John")
                .type(lastNameInput, "Doe")
                .type(addressInput, "Address")
                .type(emailInput, "john.doe@gmail.com")
                .type(phoneInput, "12345")
                .click(maleRadioButton)
                .click(moviesCheckbox)
                .click(countryDropdown)
                .type(countrySearchInput, "Japan" + Keys.ENTER)
                .type(passwordInput, "12345")
                .type(confirmPasswordInput, "12345")
                .click(submitButton);
    }

    @BeforeMethod
    public void setUp() {
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://demo.automationtesting.in/Register.html");
    }

    @AfterMethod
    public void tearDown() {
        driver.quit();
    }
}
