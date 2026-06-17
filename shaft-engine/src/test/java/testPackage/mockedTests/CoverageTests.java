package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchSessionException;
import org.openqa.selenium.remote.Browser;
import org.openqa.selenium.support.ui.WebDriverWait;
import testPackage.TestPageServer;
import org.testng.annotations.*;

import java.time.Duration;

public class CoverageTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    boolean initialValue = SHAFT.Properties.visuals.createAnimatedGif();

    @Test
    public void getElementsCount() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            int numberOfOptions = driver.get().browser().navigateToURL(TestPageServer.url("hoverDemo.html"))
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

    @Test
    public void alerts_getText_and_accept() {
        driver.get().browser().navigateToURL(TestPageServer.url("alertFixture.html"));
        var alert = driver.get().element().click(By.id("alert-button"))
                .and().alert();
        alert.getAlertText();
        alert.acceptAlert();
        driver.get().element().assertThat(By.id("alert-result")).text().isEqualTo("accepted").perform();
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
        driver.get().getDriver().navigate().to(TestPageServer.url("coverageTestPage.html"));
        waitForDocumentReady();
        driver.get().getDriver().get(testElement);
        waitForDocumentReady();
        driver.get().getDriver().navigate().back();
        waitForDocumentReady();
        driver.get().getDriver().navigate().forward();
        waitForDocumentReady();
        driver.get().getDriver().navigate().refresh();
        waitForDocumentReady();
        driver.get().getDriver().manage().window().minimize();
        driver.get().getDriver().manage().window().maximize();
        driver.get().getDriver().getCurrentUrl();
        driver.get().getDriver().getTitle();
        driver.get().getDriver().get(testElement);
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

    private void waitForDocumentReady() {
        new WebDriverWait(driver.get().getDriver(), Duration.ofSeconds(5))
                .until(webDriver -> "complete".equals(((JavascriptExecutor) webDriver)
                        .executeScript("return document.readyState")));
    }

    @Test
    public void alerts_dismiss() {
        driver.get().browser().navigateToURL(TestPageServer.url("alertFixture.html"));
        driver.get().element().click(By.id("confirm-button"))
                .and().alert().dismissAlert();
        driver.get().element().assertThat(By.id("confirm-result")).text().isEqualTo("false").perform();
    }

    @Test
    public void alerts_type() {
        driver.get().browser().navigateToURL(TestPageServer.url("alertFixture.html"));
        driver.get().element().click(By.id("prompt-button"))
                .and().alert().typeIntoPromptAlert("nachos").acceptAlert();
        driver.get().element().assertThat(By.id("prompt-result")).text().isEqualTo("nachos").perform();
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
        if (driver.get() != null) {
            driver.get().quit();
        }
        driver.remove();
    }
}
