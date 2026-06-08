package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.TestNGListener;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Listeners;
import org.testng.annotations.Test;

@Listeners({TestNGListener.class})
public class GroupsTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test(groups = {"regression"})
    public void test() {
        driver.get().browser().navigateToURL("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>");
        driver.get().assertThat().element(By.cssSelector("button")).exists().perform();
    }

    @Test(groups = {"regression"})
    public void test1() {
        driver.get().browser().navigateToURL("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>");
        driver.get().assertThat().element(By.cssSelector("button")).exists().perform();
    }

    @Test(groups = {"NOTregression"})
    public void test2() {
        driver.get().browser().navigateToURL("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>");
        driver.get().assertThat().element(By.cssSelector("button")).exists().perform();
    }

    @Test(groups = {"regression"})
    public void test3() {
        driver.get().browser().navigateToURL("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>");
        driver.get().assertThat().element(By.cssSelector("button")).exists().perform();
    }

    @BeforeMethod(description = "setup browser", alwaysRun = true)
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(description = "quit browser", alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
