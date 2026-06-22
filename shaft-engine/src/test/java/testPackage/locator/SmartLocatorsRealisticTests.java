package testPackage.locator;

import org.openqa.selenium.By;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.testng.annotations.Test;
import testPackage.TestScenario;

import java.time.Duration;

public class SmartLocatorsRealisticTests extends TestScenario {
    // Phase 3: Realistic Testing
    private static final By HUAWEI_ADD_TO_CART_BUTTON = By.xpath(
            "(//div[@id='prooood']//span[normalize-space()='Huawei Mate 20 Lite, 64GB, Black']" +
                    "/ancestor::div[contains(concat(' ', normalize-space(@class), ' '), ' shop-item ')][1]" +
                    "//button[contains(concat(' ', normalize-space(@class), ' '), ' shop-item-button ')])[1]");
    private static final By CART_TOTAL_PRICE = By.cssSelector(".cart-total-price");
    // Realistic test scenario for an e-commerce website
    // Fluent design, mixing regular locators with smart locators, performing assertions, reporting.
    // Two more algorithms were added for smart element identification, bringing the total to 34.

    @Test
    public void login() {
        driver.browser().navigateToURL("https://qa-practice.netlify.app/auth_ecommerce")
                .and().element().type("Email", "admin@admin.com")
                .and().type("Password", "admin123")
                .and().click("Submit")
                .and().assertThat(By.cssSelector("#prooood h2")).text().isEqualTo("SHOPPING CART");
    }

    @Test(dependsOnMethods = "login")
    public void addProductToCart() {
        driver.element()
                .waitUntil(ExpectedConditions.elementToBeClickable(HUAWEI_ADD_TO_CART_BUTTON), Duration.ofSeconds(10))
                .and().click(HUAWEI_ADD_TO_CART_BUTTON)
                .and().waitUntil(ExpectedConditions.textToBe(CART_TOTAL_PRICE, "$236.12"), Duration.ofSeconds(10))
                .and().assertThat(CART_TOTAL_PRICE).text().isEqualTo("$236.12");
    }

    @Test(dependsOnMethods = {"login", "addProductToCart"})
    public void fillShippingDetails() {
        driver.element().type("Phone number", "00201000000000")
                .and().type("Street", "101 dummy street")
                .and().type("City", "Cairo")
                .and().select(By.id("countries_dropdown_menu"), "Egypt")
                .and().element().click("Submit Order")
                .and().assertThat(By.id("message")).text().isEqualTo("Congrats! Your order of $236.12 has been registered and will be shipped to 101 dummy street, Cairo - Egypt.");
    }
}
