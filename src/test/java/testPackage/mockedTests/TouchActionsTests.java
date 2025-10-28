package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.TouchActions;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class TouchActionsTests {
    ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    String testElement = "data:text/html,<input type=\"text\"><br><br>";
    By locator = SHAFT.GUI.Locator.hasTagName("input").build();

    @Test
    public void randomActions() {
        driver.get().touch()
                .doubleTap(locator)
                .longTap(locator)
                .pinchToZoom(TouchActions.ZoomDirection.IN)
                .pinchToZoom(TouchActions.ZoomDirection.OUT);
    }

    @BeforeMethod
    void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL(testElement);
    }

    @AfterMethod(alwaysRun = true)
    void afterMethod() {
        driver.get().quit();
    }
}
