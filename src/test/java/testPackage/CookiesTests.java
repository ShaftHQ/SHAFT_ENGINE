package testPackage;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class CookiesTests {
    private SHAFT.GUI.WebDriver driver;

    @Test
    public void test_addCookie() {
        driver.browser().addCookie("foo", "bar");
        SHAFT.Validations.assertThat().object(driver.browser().getCookie("foo")).isNotNull().perform();
    }

    @Test
    public void test_getCookieAttributes() {
        driver.browser().addCookie("foo", "bar");
        SHAFT.Validations.verifyThat().object(driver.browser().getCookieDomain("foo")).isEqualTo("www.example.com").perform();
        SHAFT.Validations.verifyThat().object(driver.browser().getCookiePath("foo")).isEqualTo("/").perform();
        SHAFT.Validations.verifyThat().object(driver.browser().getCookieValue("foo")).isEqualTo("bar").perform();
    }

    @Test
    public void test_getAllCookies() {
        driver.browser()
                .addCookie("cookie1", "val1")
                .addCookie("cookie2", "val2");

        var cookies = driver.browser().getAllCookies();
        SHAFT.Validations.assertThat().object(cookies).contains("cookie1=val1").perform();
        SHAFT.Validations.assertThat().object(cookies).contains("cookie2=val2").perform();

    }

    @Test
    public void test_deleteCookie() {
        driver.browser()
                .addCookie("foo", "bar")
                .deleteCookie("foo");

        var cookie = "";
        try {
            cookie = driver.browser().getCookie("foo").toString();
        } catch (AssertionError ae){}
        SHAFT.Validations.assertThat().object(cookie).isEqualTo("").perform();
    }

    @Test
    public void test_deleteAllCookies() {
        driver.browser()
                .addCookie("cookie1", "val1")
                .addCookie("cookie2", "cal2")
                .deleteAllCookies();

        SHAFT.Validations.assertThat().object(driver.browser().getAllCookies().toString()).isEqualTo("[]").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL("http://www.example.com");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.quit();
    }

}
