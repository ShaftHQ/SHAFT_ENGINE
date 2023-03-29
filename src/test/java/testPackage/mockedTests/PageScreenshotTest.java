package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import com.shaft.enums.internal.Screenshots;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class PageScreenshotTest {
    ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    String testPage = "data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>";

    @Test
    public void captureScreenshot() {
        driver.get().browser().captureScreenshot();
    }

    @Test
    public void captureFullScreenshot() {
        driver.get().browser().captureScreenshot(Screenshots.FULL);
    }

    @Test
    public void captureViewportScreenshot() {
        driver.get().browser().captureScreenshot(Screenshots.VIEWPORT);
    }

    @Test
    public void captureSnapshot() {
        driver.get().browser().captureSnapshot();
    }

    @Test
    public void captureElementScreenshot() {
        driver.get().element().captureScreenshot(By.tagName("button"));
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL(testPage);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
        driver.remove();
    }
}
