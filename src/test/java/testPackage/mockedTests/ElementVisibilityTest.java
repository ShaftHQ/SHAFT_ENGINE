package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class ElementVisibilityTest {
    ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    String testElement = "data:text/html,<script>var result;</script><button ${HIDDEN} alt='Google' onclick='result=\"Clicked\"'>Go</button>";
    By locator = SHAFT.GUI.Locator.hasTagName("button").hasAttribute("alt", "Google").build();

    @Test
    public void elementHiddenAndExpectedToBeHidden() {
        driver.get().browser().navigateToURL(testElement.replace("${HIDDEN}", "hidden"));
        driver.get().element().waitToBeInvisible(locator);
        driver.get().assertThat().element(locator).isHidden().perform();
    }

    @Test
    public void elementVisibleAndExpectedToBeVisible() {
        driver.get().browser().navigateToURL(testElement.replace("${HIDDEN}", ""));
        driver.get().element().waitToBeReady(locator);
        driver.get().assertThat().element(locator).isVisible().perform();
    }

    @Test(expectedExceptions = {AssertionError.class}, expectedExceptionsMessageRegExp = ".*ailed.*")
    public void elementHiddenAndExpectedToBeVisible() {
        driver.get().browser().navigateToURL(testElement.replace("${HIDDEN}", "hidden"));
        driver.get().element().waitToBeInvisible(locator);
        driver.get().assertThat().element(locator).isVisible().perform();
    }

    @Test(expectedExceptions = {AssertionError.class}, expectedExceptionsMessageRegExp = ".*ailed.*")
    public void elementVisibleAndExpectedToBeHidden() {
        driver.get().browser().navigateToURL(testElement.replace("${HIDDEN}", ""));
        driver.get().element().waitToBeReady(locator);
        driver.get().assertThat().element(locator).isHidden().perform();
    }

    @Test(expectedExceptions = {AssertionError.class}, expectedExceptionsMessageRegExp = ".*ailed.*")
    public void elementDoesntExistAndExpectedToBeVisible() {
        driver.get().browser().navigateToURL(testElement.replace("${HIDDEN}", "hidden"));
        driver.get().element().waitToBeInvisible(By.id("bla"));
        driver.get().assertThat().element(locator).isVisible().perform();
    }

    @Test(expectedExceptions = {AssertionError.class}, expectedExceptionsMessageRegExp = ".*ailed.*")
    public void elementDoesntExistAndExpectedToBeHidden() {
        driver.get().browser().navigateToURL(testElement.replace("${HIDDEN}", ""));
        driver.get().element().waitToBeReady(By.id("bla"));
        driver.get().assertThat().element(locator).isHidden().perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
        driver.remove();
    }
}
