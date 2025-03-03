package testPackage.locator;

import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.testng.annotations.Test;
import testPackage.TestScenario;
import testPackage.Tests;

public class SmartLocatorsRealisticTests extends TestScenario {
    // Phase 3: Realistic Testing

    // Realistic test scenario for an e-commerce website
    // Fluent design, mixing regular locators with smart locators, performing assertions, reporting.
    // Two more algorithms were added for smart element identification, bringing the total to 34.

    @Test
    public void login() {
        driver.get().browser().navigateToURL("https://qa-practice.netlify.app/auth_ecommerce")
                .and().element().type("Email", "admin@admin.com")
                .and().type("Password", "admin123")
                .and().click("Submit")
                .and().assertThat(By.cssSelector("#prooood h2")).text().isEqualTo("SHOPPING CART");
    }

    @Test(dependsOnMethods = "login")
    public void addProductToCart() {
        driver.get().element()
                .click("Huawei Mate 20 Lite, 64GB, Black")
                .and().assertThat(By.cssSelector(".cart-total-price")).text().isEqualTo("$236.12");
    }

    @Test(dependsOnMethods = {"login", "addProductToCart"})
    public void proceedToCheckout() {
        driver.get().element().click("PROCEED TO CHECKOUT")
                .and().assertThat(By.cssSelector("#shipping-address>h2")).text().isEqualTo("Shipping Details");
    }

    @Test(dependsOnMethods = {"login", "addProductToCart", "proceedToCheckout"})
    public void fillShippingDetails() {
        driver.get().element().type("Phone number", "00201000000000")
                .and().type("Street", "101 dummy street")
                .and().type("City", "Cairo")
                .and().select(By.id("countries_dropdown_menu"), "Egypt")
                .and().element().click("Submit Order")
                .and().assertThat(By.id("message")).text().isEqualTo("Congrats! Your order of $236.12 has been registered and will be shipped to 101 dummy street, Cairo - Egypt.");
    }
}