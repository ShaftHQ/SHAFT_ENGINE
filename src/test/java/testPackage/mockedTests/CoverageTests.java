package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchSessionException;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.*;
import testPackage.legacy.SearchOptimizationTest;

public class CoverageTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    boolean initialValue = SHAFT.Properties.visuals.createAnimatedGif();

    @Test
    public void getElementsCount() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            int numberOfOptions = driver.get().browser().navigateToURL(SHAFT.Properties.paths.testData() + "hoverDemo.html")
                    .and().element().getElementsCount(SHAFT.GUI.Locator.hasTagName("a").build());
            SHAFT.Validations.assertThat().number(numberOfOptions).isGreaterThan(1).perform();
        }
    }

    @Test(expectedExceptions = {RuntimeException.class})
    public void invalidLocator() {
        String testElement = "data:text/html,<input type=\"text\"><br><br>";
        driver.get().browser().navigateToURL(testElement);
        driver.get().element().click(By.xpath("....."))
                .and().alert();
    }

    @Test
    public void getWindowsHeight() {
        String testElement = "data:text/html,<input type=\"text\"><br><br>";
        driver.get().browser().navigateToURL(testElement);
        driver.get().browser().getWindowHeight();
        driver.get().browser().getWindowWidth();
        if (!SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName()))
            driver.get().browser().navigateToURLWithBasicAuthentication("https://authenticationtest.com/HTTPAuth/", "user", "pass", "https://authenticationtest.com/loginSuccess/");
    }

    @Test(enabled = false)
    public void alerts_getText_and_accept() {
        driver.get().browser().navigateToURL("https://kitchen.applitools.com/ingredients/alert");
        var alert = driver.get().element().click(By.id("alert-button"))
                .and().alert();
        alert.getAlertText();
        alert.acceptAlert();
    }

    @Test(expectedExceptions = {RuntimeException.class})
    public void clickFakeElement_expectedToFail() {
        String testElement = "data:text/html,<input type=\"text\"><br><br>";
        driver.get().browser().navigateToURL(testElement);
        driver.get().element().click(By.id("fakeElement"));
    }

    @Test(expectedExceptions = {RuntimeException.class})
    public void typeInFakeElement_expectedToFail() {
        String testElement = "data:text/html,<input type=\"text\"><br><br>";
        driver.get().browser().navigateToURL(testElement);
        driver.get().element().type(By.id("fakeElement"), "anyText");
    }

    @Test
    public void nativeWebDriverListenerTests() {
        String testElement = "data:text/html,<form><input type=\"text\"><br><br></form>";
        By locator = SHAFT.GUI.Locator.hasTagName("input").build();
        driver.get().getDriver().navigate().to("https://www.automatest.org");
        driver.get().getDriver().get(testElement);
        driver.get().getDriver().navigate().back();
        driver.get().getDriver().navigate().forward();
        driver.get().getDriver().navigate().refresh();
        driver.get().getDriver().manage().window().minimize();
        driver.get().getDriver().manage().window().maximize();
        driver.get().getDriver().getCurrentUrl();
        driver.get().getDriver().getTitle();
        driver.get().getDriver().findElement(locator).click();
        driver.get().getDriver().findElement(locator).clear();
        driver.get().getDriver().findElement(locator).sendKeys("test");
        driver.get().getDriver().findElement(locator).getAttribute("value");
        driver.get().getDriver().findElement(locator).getText();
        driver.get().getDriver().findElement(locator).submit();
        driver.get().element().assertThat(locator).text().isEqualTo("test").perform();
        driver.get().getDriver().close();
        try {
            driver.get().getDriver().quit();
        } catch (NoSuchSessionException noSuchSessionException) {
            // do nothing
        }
    }

    @Test(enabled = false)
    public void alerts_dismiss() {
        driver.get().browser().navigateToURL("https://kitchen.applitools.com/ingredients/alert");
        driver.get().element().click(By.id("confirm-button"))
                .and().alert().dismissAlert();
    }

    @Test(enabled = false)
    public void alerts_type() {
        driver.get().browser().navigateToURL("https://kitchen.applitools.com/ingredients/alert");
        driver.get().element().click(By.id("prompt-button"))
                .and().alert().typeIntoPromptAlert("nachos").acceptAlert();
    }

    @Test(enabled = false)
    public void submitFormUsingJavaScript() {
        By searchBox = SearchOptimizationTest.searchBox;
        By searchResult = SHAFT.GUI.Locator.hasTagName("a").containsAttribute("href", "SHAFT_ENGINE").isFirst().build();
        boolean isElementDisplayed = driver.get().browser().navigateToURL("https://www.google.com/ncr")
                .and().element().type(searchBox, "SHAFT_Engine")
                .and().clear(searchBox)
                .and().element().type(searchBox, "SHAFT_Engine")
                .and().submitFormUsingJavaScript(searchBox)
                .and().isElementDisplayed(searchResult);
        SHAFT.Validations.assertThat().object(isElementDisplayed).isEqualTo(true).withCustomReportMessage("target search result is displayed").perform();
    }

    @BeforeClass
    public void beforeClass() {
        SHAFT.Properties.visuals.set().createAnimatedGif(true);
    }

    @AfterClass(alwaysRun = true)
    public void afterClass() {
        SHAFT.Properties.visuals.set().createAnimatedGif(initialValue);
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
