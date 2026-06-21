package testPackage.playwright;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

public abstract class PlaywrightActionsE2ETestBase {
    private static final ThreadLocal<SHAFT.GUI.Playwright> DRIVER = new ThreadLocal<>();

    @BeforeMethod(alwaysRun = true)
    public void setUpPlaywright() {
        Properties.clearForCurrentThread();
        SHAFT.Properties.web.set()
                .headlessExecution(true)
                .browserWindowWidth(1280)
                .browserWindowHeight(720);
        SHAFT.Properties.playwright.set()
                .connectionMode("local")
                .browserName(browserName())
                .channel(channel())
                .deviceName(deviceName())
                .defaultTimeoutMilliseconds(10000)
                .navigationTimeoutMilliseconds(10000);
        DRIVER.set(new SHAFT.GUI.Playwright());
    }

    @AfterMethod(alwaysRun = true)
    public void tearDownPlaywright() {
        try {
            if (DRIVER.get() != null) {
                DRIVER.get().quit();
            }
        } finally {
            DRIVER.remove();
            Properties.clearForCurrentThread();
        }
    }

    @Test
    public void shouldNavigateAndValidateBrowserState() {
        driver().browser().navigateToURL(TestPageServer.url("coverageTestPage.html"));

        driver().assertThat().browser().title().contains("SHAFT Coverage").perform();
        driver().assertThat().browser().url().contains("coverageTestPage.html").perform();
        driver().assertThat().browser().text().contains("Coverage Test Page").perform();
    }

    @Test
    public void shouldPerformElementActions() {
        driver().browser().navigateToURL(TestPageServer.url("coverageTestPage.html"));

        driver().element()
                .clear(By.id("textInput"))
                .type(By.id("textInput"), "Playwright")
                .typeAppend(By.id("textInput"), " backend")
                .select(By.id("selectEl"), "opt2")
                .click(By.id("checkboxUnchecked"));

        driver().assertThat().element(By.id("textInput")).domProperty("value").isEqualTo("Playwright backend").perform();
        driver().assertThat().element(By.id("selectEl")).domProperty("value").isEqualTo("opt2").perform();
        driver().assertThat().element(By.id("checkboxUnchecked")).isChecked().perform();
    }

    @Test
    public void shouldHandleAlerts() {
        driver().browser().navigateToURL(TestPageServer.url("alertFixture.html"));

        driver().alert().acceptAlert();
        driver().element().click(By.id("alert-button"));

        driver().assertThat().object(driver().alert().getAlertText()).isEqualTo("Hello from fixture").perform();
        driver().assertThat().element(By.id("alert-result")).text().isEqualTo("accepted").perform();
    }

    protected SHAFT.GUI.Playwright driver() {
        return DRIVER.get();
    }

    protected String browserName() {
        return "";
    }

    protected String channel() {
        return "";
    }

    protected String deviceName() {
        return "";
    }
}
