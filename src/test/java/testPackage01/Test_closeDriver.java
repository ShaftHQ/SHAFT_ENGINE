package testPackage01;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import org.testng.annotations.Test;

public class Test_closeDriver {
    //    @BeforeClass
    public void beforeClass() {
        // remote browserstack server
        System.setProperty("executionAddress", "browserstack");
        System.setProperty("targetOperatingSystem", "Mac-64");
        System.setProperty("targetBrowserName", "Safari");
        System.setProperty("browserStack.os", "OS X");
        System.setProperty("browserStack.osVersion", "Monterey");
        System.setProperty("browserStack.local", "false");
        System.setProperty("browserStack.seleniumVersion", "4.1.2");
        SHAFT.GUI.WebDriver driver;
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        BrowserActions.closeCurrentWindow(driver.getDriver());
    }

    @Test
    public void test1() {
        SHAFT.GUI.WebDriver driver;
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        BrowserActions.closeCurrentWindow(driver.getDriver());
    }

    @Test
    public void test2() {
        SHAFT.GUI.WebDriver driver;
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.quit();
    }

    @Test
    public void test3() {
        SHAFT.GUI.WebDriver driver;
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.quit();
    }

    @Test
    public void test4() {
        SHAFT.GUI.WebDriver driver;
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.quit();
    }

    @Test
    public void test5() {
        SHAFT.GUI.WebDriver driver;
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.quit();
    }

    @Test
    public void test6() {
        SHAFT.GUI.WebDriver driver;
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.quit();
    }

    @Test
    public void test7() {
        SHAFT.GUI.WebDriver driver;
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.quit();
    }

    @Test
    public void test8() {
        SHAFT.GUI.WebDriver driver;
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.quit();
    }

    @Test
    public void test9() {
        SHAFT.GUI.WebDriver driver;
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.quit();
    }

    @Test
    public void test10() {
        SHAFT.GUI.WebDriver driver;
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("https://www.google.com/");
        driver.quit();
    }
}
