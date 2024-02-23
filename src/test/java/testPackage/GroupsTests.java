package testPackage;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.TestNGListener;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Listeners;
import org.testng.annotations.Test;

@Listeners({TestNGListener.class})
public class GroupsTests {
    SHAFT.GUI.WebDriver driver;

    @Test(groups = {"regression"})
    public void test() {
        driver.browser().navigateToURL("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>");
        driver.assertThat().element(By.cssSelector("button")).exists().perform();
    }

    @Test(groups = {"regression"})
    public void test1() {
        driver.browser().navigateToURL("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>");
        driver.assertThat().element(By.cssSelector("button")).exists().perform();
    }

    @Test(groups = {"NOTregression"})
    public void test2() {
        driver.browser().navigateToURL("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>");
        driver.assertThat().element(By.cssSelector("button")).exists().perform();
    }

    @Test(groups = {"regression"})
    public void test3() {
        driver.browser().navigateToURL("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>");
        driver.assertThat().element(By.cssSelector("button")).exists().perform();
    }

    @BeforeMethod(description = "setup browser", alwaysRun = true)
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod(description = "quit browser", alwaysRun = true)
    public void afterMethod() {
        driver.quit();
    }
}
