package testPackage.locator;

import org.openqa.selenium.By;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.testng.annotations.Test;
import testPackage.TestScenario;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Duration;

public class SmartLocatorsRealisticTests extends TestScenario {
    // Phase 3: Realistic Testing
    private static final By HUAWEI_ADD_TO_CART_BUTTON = By.xpath(
            "(//div[@id='prooood']//span[normalize-space()='Huawei Mate 20 Lite, 64GB, Black']" +
                    "/ancestor::div[contains(concat(' ', normalize-space(@class), ' '), ' shop-item ')][1]" +
                    "//button[contains(concat(' ', normalize-space(@class), ' '), ' shop-item-button ')])[1]");
    private static final By CART_TOTAL_PRICE = By.cssSelector(".cart-total-price");
    private static final By PROCEED_TO_CHECKOUT_BUTTON = By.xpath("//button[normalize-space()='PROCEED TO CHECKOUT']");
    private static final By PHONE_INPUT = By.id("phone");
    // Realistic test scenario for an e-commerce website
    // Fluent design, mixing regular locators with smart locators, performing assertions, reporting.
    // Two more algorithms were added for smart element identification, bringing the total to 34.

    @Test
    public void login() {
        driver.browser().navigateToURL(localStorefrontUrl())
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
        driver.element()
                .waitUntil(ExpectedConditions.elementToBeClickable(PROCEED_TO_CHECKOUT_BUTTON), Duration.ofSeconds(10))
                .and().click(PROCEED_TO_CHECKOUT_BUTTON)
                .and().waitUntil(ExpectedConditions.elementToBeClickable(PHONE_INPUT), Duration.ofSeconds(10))
                .and().type("Phone number", "00201000000000")
                .and().type("Street", "101 dummy street")
                .and().type("City", "Cairo")
                .and().select(By.id("countries_dropdown_menu"), "Egypt")
                .and().element().click("Submit Order")
                .and().assertThat(By.id("message")).text().isEqualTo("Congrats! Your order of $236.12 has been registered and will be shipped to 101 dummy street, Cairo - Egypt.");
    }

    private static String localStorefrontUrl() {
        String html = """
                <!doctype html>
                <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>Local storefront</title>
                </head>
                <body>
                    <main id="app">
                        <form id="login-form">
                            <label for="email">Email</label>
                            <input id="email" name="email" type="email">
                            <label for="password">Password</label>
                            <input id="password" name="password" type="password">
                            <button type="submit">Submit</button>
                        </form>
                    </main>
                    <script>
                        const app = document.getElementById('app');
                        const orderMessage = 'Congrats! Your order of $236.12 has been registered and will be shipped to 101 dummy street, Cairo - Egypt.';

                        function showShop() {
                            app.innerHTML = `
                                <div id="prooood">
                                    <h2>SHOPPING CART</h2>
                                    <div class="shop-item">
                                        <span>Huawei Mate 20 Lite, 64GB, Black</span>
                                        <button type="button" class="shop-item-button">Add to cart</button>
                                    </div>
                                    <div class="cart-total-price">$0.00</div>
                                    <button type="button" id="checkout">PROCEED TO CHECKOUT</button>
                                </div>`;
                            document.querySelector('.shop-item-button').addEventListener('click', () => {
                                document.querySelector('.cart-total-price').textContent = '$236.12';
                            });
                            document.getElementById('checkout').addEventListener('click', showCheckout);
                        }

                        function showCheckout() {
                            app.insertAdjacentHTML('beforeend', `
                                <section id="shipping">
                                    <label for="phone">Phone number</label>
                                    <input id="phone" name="phone">
                                    <label for="street">Street</label>
                                    <input id="street" name="street">
                                    <label for="city">City</label>
                                    <input id="city" name="city">
                                    <select id="countries_dropdown_menu">
                                        <option>Egypt</option>
                                    </select>
                                    <button type="button" id="submit-order">Submit Order</button>
                                    <div id="message"></div>
                                </section>`);
                            document.getElementById('submit-order').addEventListener('click', () => {
                                document.getElementById('message').textContent = orderMessage;
                            });
                        }

                        document.getElementById('login-form').addEventListener('submit', event => {
                            event.preventDefault();
                            showShop();
                        });
                    </script>
                </body>
                </html>
                """;
        return "data:text/html;charset=utf-8," + URLEncoder.encode(html, StandardCharsets.UTF_8).replace("+", "%20");
    }
}
