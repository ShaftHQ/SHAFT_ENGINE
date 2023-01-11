package testPackage01;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class Test_closeDriver {
    ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void test1() {
        basicNavigation();
    }

    @Test
    public void test2() {
        basicNavigation();
    }

    @Test
    public void test3() {
        basicNavigation();
    }

    @Test
    public void test4() {
        basicNavigation();
    }

    @Test
    public void test5() {
        basicNavigation();
    }

    @Test
    public void test6() {
        basicNavigation();
    }

    @Test
    public void test7() {
        basicNavigation();
    }

    @Test
    public void test8() {
        basicNavigation();
    }

    @Test
    public void test9() {
        basicNavigation();
    }

    @Test
    public void test10() {
        basicNavigation();
    }

    private void basicNavigation() {
        driver.get().browser().navigateToURL(SHAFT.Properties.paths.testData() + "test.html");
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod
    public void afterMethod() {
        driver.get().quit();
        driver.remove();
    }
}
