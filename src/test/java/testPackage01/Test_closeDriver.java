package testPackage01;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import org.testng.annotations.Test;

public class Test_closeDriver {
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
