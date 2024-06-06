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

    @Test(expectedExceptions = {java.lang.AssertionError.class})
    public void invalidLocator() {
        driver.get().browser().navigateToURL("https://kitchen.applitools.com/ingredients/alert");
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
        if (SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local")
           && (!SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName()))) {
            driver.get().browser().getLocalStorage();
            driver.get().browser().getSessionStorage();
        }
    }

    @Test
    public void alerts_getText_and_accept() {
        driver.get().browser().navigateToURL("https://kitchen.applitools.com/ingredients/alert");
        var alert = driver.get().element().click(By.id("alert-button"))
                .and().alert();
        alert.getAlertText();
        alert.acceptAlert();
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void clickFakeElement_expectedToFail() {
        String testElement = "data:text/html,<input type=\"text\"><br><br>";
        driver.get().browser().navigateToURL(testElement);
        driver.get().element().click(By.id("fakeElement"));
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void typeInFakeElement_expectedToFail() {
        String testElement = "data:text/html,<input type=\"text\"><br><br>";
        driver.get().browser().navigateToURL(testElement);
        driver.get().element().type(By.id("fakeElement"), "anyText");
    }

    @Test
    public void nativeWebDriverListenerTests() {
        String testElement = "data:text/html,<form><input type=\"text\"><br><br></form>";
        By locator = SHAFT.GUI.Locator.hasTagName("input").build();
        var nativeDriver = driver.get().getDriver();
        nativeDriver.navigate().to("https://www.google.com/ncr");
        nativeDriver.get(testElement);
        nativeDriver.navigate().back();
        nativeDriver.navigate().forward();
        nativeDriver.navigate().refresh();
        nativeDriver.manage().window().minimize();
        nativeDriver.manage().window().maximize();
        nativeDriver.getCurrentUrl();
        nativeDriver.getTitle();
        nativeDriver.findElement(locator).click();
        nativeDriver.findElement(locator).clear();
        nativeDriver.findElement(locator).sendKeys("test");
        nativeDriver.findElement(locator).getAttribute("value");
        nativeDriver.findElement(locator).getText();
        nativeDriver.findElement(locator).submit();
        driver.get().element().assertThat(locator).text().isEqualTo("test").perform();
        nativeDriver.close();
        try {
            nativeDriver.quit();
        } catch (NoSuchSessionException noSuchSessionException) {
            // do nothing
        }
    }

    @Test
    public void alerts_dismiss() {
        driver.get().browser().navigateToURL("https://kitchen.applitools.com/ingredients/alert");
        driver.get().element().click(By.id("confirm-button"))
                .and().alert().dismissAlert();
    }

    @Test
    public void alerts_type() {
        driver.get().browser().navigateToURL("https://kitchen.applitools.com/ingredients/alert");
        driver.get().element().click(By.id("prompt-button"))
                .and().alert().typeIntoPromptAlert("nachos").acceptAlert();
    }

    @Test
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
