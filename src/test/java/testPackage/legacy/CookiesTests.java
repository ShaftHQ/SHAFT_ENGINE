package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class CookiesTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void test_addCookie() {
        driver.get().browser().addCookie("foo", "bar");
        SHAFT.Validations.assertThat().object(driver.get().browser().getCookie("foo")).isNotNull().perform();
    }

    @Test
    public void test_getCookieAttributes() {
        driver.get().browser().addCookie("foo", "bar");
        SHAFT.Validations.verifyThat().object(driver.get().browser().getCookieDomain("foo")).isEqualTo("www.example.com").perform();
        SHAFT.Validations.verifyThat().object(driver.get().browser().getCookiePath("foo")).isEqualTo("/").perform();
        SHAFT.Validations.verifyThat().object(driver.get().browser().getCookieValue("foo")).isEqualTo("bar").perform();
    }

    @Test
    public void test_getAllCookies() {
        driver.get().browser()
                .addCookie("cookie1", "val1")
                .addCookie("cookie2", "val2");

        var cookies = driver.get().browser().getAllCookies();
        SHAFT.Validations.assertThat().object(cookies).contains("cookie1=val1").perform();
        SHAFT.Validations.assertThat().object(cookies).contains("cookie2=val2").perform();

    }

    @Test
    public void test_deleteCookie() {
        driver.get().browser()
                .addCookie("foo", "bar")
                .deleteCookie("foo");

        var cookie = "";
        try {
            cookie = driver.get().browser().getCookie("foo").toString();
        } catch (AssertionError ae){}
        SHAFT.Validations.assertThat().object(cookie).isEqualTo("").perform();
    }

    @Test
    public void test_deleteAllCookies() {
        driver.get().browser()
                .addCookie("cookie1", "val1")
                .addCookie("cookie2", "cal2")
                .deleteAllCookies();

        SHAFT.Validations.assertThat().object(driver.get().browser().getAllCookies().toString()).isEqualTo("[]").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("http://www.example.com");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }

}
