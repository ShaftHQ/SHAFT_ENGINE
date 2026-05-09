package testPackage.realtime;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.By;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class RealtimeReporterHeadlessCheckoutDemoTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private SHAFT.TestData.JSON testData;

    private final By usernameField = Locator.hasTagName("input").hasAttribute("data-test", "username").build();
    private final By passwordField = Locator.hasTagName("input").hasAttribute("data-test", "password").build();
    private final By loginButton = Locator.hasTagName("input").hasAttribute("data-test", "login-button").build();
    private final By backpackAddToCartButton = By.id("add-to-cart-sauce-labs-backpack");
    private final By bikeLightAddToCartButton = By.id("add-to-cart-sauce-labs-bike-light");
    private final By cartBadge = By.className("shopping_cart_badge");
    private final By cartLink = By.className("shopping_cart_link");
    private final By checkoutButton = By.id("checkout");
    private final By firstNameField = By.id("first-name");
    private final By lastNameField = By.id("last-name");
    private final By postalCodeField = By.id("postal-code");
    private final By continueButton = By.id("continue");
    private final By checkoutSummary = By.id("checkout_summary_container");
    private final By finishButton = By.id("finish");
    private final By completeHeader = By.className("complete-header");
    private final By backHomeButton = By.id("back-to-products");

    @BeforeClass(alwaysRun = true)
    public void loadTestData() {
        testData = new SHAFT.TestData.JSON("realtimeReporterCheckoutDemo.json");
    }

    @BeforeMethod(alwaysRun = true)
    public void setup() {
        SHAFT.Properties.reporting.set().realtimeReport(true);
        SHAFT.Properties.web.set()
                .targetBrowserName(Browser.CHROME.browserName())
                .headlessExecution(true);
        SHAFT.Properties.flags.set().automaticallyAddRecommendedChromeOptions(true);
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        if (driver.get() != null) {
            driver.get().quit();
            driver.remove();
        }
        Properties.clearForCurrentThread();
    }

    @Test(groups = "realtime-reporter-demo", description = "Runs a long headless Sauce Demo checkout journey for realtime dashboard recording")
    public void completeFullShoppingCheckoutCycleInHeadlessMode() {
        driver.get().browser().navigateToURL(testData.getTestData("baseUrl"));
        pauseForDashboardRecording();

        driver.get().assertThat().browser().title().contains(testData.getTestData("expectedTitle")).perform();
        driver.get().element()
                .type(usernameField, testData.getTestData("username"))
                .typeSecure(passwordField, testData.getTestData("password"))
                .click(loginButton);
        pauseForDashboardRecording();

        driver.get().assertThat().element(By.xpath("//div[text()='Sauce Labs Backpack']")).exists().perform();
        driver.get().element()
                .click(backpackAddToCartButton)
                .click(bikeLightAddToCartButton);
        pauseForDashboardRecording();

        driver.get().assertThat().element(cartBadge).text().isEqualTo(testData.getTestData("expectedCartCount")).perform();
        driver.get().element().click(cartLink);
        pauseForDashboardRecording();

        driver.get().assertThat().element(By.xpath("//div[text()='Sauce Labs Backpack']")).exists().perform();
        driver.get().assertThat().element(By.xpath("//div[text()='Sauce Labs Bike Light']")).exists().perform();
        driver.get().element().click(checkoutButton);
        pauseForDashboardRecording();

        driver.get().element()
                .type(firstNameField, testData.getTestData("firstName"))
                .type(lastNameField, testData.getTestData("lastName"))
                .type(postalCodeField, testData.getTestData("postalCode"))
                .click(continueButton);
        pauseForDashboardRecording();

        driver.get().assertThat().element(checkoutSummary).exists().perform();
        driver.get().assertThat().element(By.xpath("//div[contains(text(),'Payment Information')]")).exists().perform();
        driver.get().element().click(finishButton);
        pauseForDashboardRecording();

        driver.get().assertThat().element(completeHeader).text().isEqualTo(testData.getTestData("completionMessage")).perform();
        driver.get().element().click(backHomeButton);
        driver.get().assertThat().element(By.xpath("//div[text()='Sauce Labs Backpack']")).exists().perform();
        pauseForDashboardRecording();
    }

    private void pauseForDashboardRecording() {
        try {
            Thread.sleep(1500);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("Interrupted while pacing realtime dashboard demo", e);
        }
    }
}
