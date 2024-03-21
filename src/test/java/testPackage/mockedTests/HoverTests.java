package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.List;

public class HoverTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void basicHoverTest() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            driver.get().browser().navigateToURL(SHAFT.Properties.paths.testData() + "hoverDemo.html")
                    .and().element().hover(By.className("dropbtn"))
                    .and().assertThat(SHAFT.GUI.Locator.hasTagName("a").and().hasText("Link 3").build()).isVisible().perform();
        }
    }

    @Test
    public void basicHoverAndClickTest() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            driver.get().browser().navigateToURL(SHAFT.Properties.paths.testData() + "hoverDemo.html")
                    .and().element().hoverAndClick(List.of(By.className("dropbtn")), SHAFT.GUI.Locator.hasTagName("a").and().hasText("Link 3").build())
                    .and().assertThat(SHAFT.GUI.Locator.hasTagName("a").and().hasText("Link 3").build()).isVisible().perform();
        }
    }

    @Test
    public void hoverOutsideViewPortTest() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            driver.get().browser().navigateToURL(SHAFT.Properties.paths.testData() + "hoverDemo_outsideViewPort.html")
                    .and().element().hover(By.className("dropbtn"))
                    .and().assertThat(SHAFT.GUI.Locator.hasTagName("a").and().hasText("Link 3").build()).isVisible().perform();
        }
    }

    @Test
    public void hoverOutsideViewPortHorizontallyTest() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            driver.get().browser().navigateToURL(SHAFT.Properties.paths.testData() + "hoverDemo_outsideViewPort_Horizontal.html")
                    .and().element().hover(By.className("dropbtn"))
                    .and().assertThat(SHAFT.GUI.Locator.hasTagName("a").and().hasText("Link 3").build()).isVisible().perform();
        }
    }

    @BeforeMethod
    public void beforeClass() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterClass() {
        driver.get().quit();
    }
}
