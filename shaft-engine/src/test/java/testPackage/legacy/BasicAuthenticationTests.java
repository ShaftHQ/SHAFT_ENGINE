package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.Test;
import testPackage.TestPageServer;
import testPackage.Tests;

public class BasicAuthenticationTests extends Tests {

    @Test
    public void basicAuthenticationTraditional() {
        if (isChromium()) {
            driver.get().browser().navigateToURL(TestPageServer.basicAuthUrl(true), TestPageServer.basicAuthUrl(false));
            driver.get().assertThat().element(By.tagName("h1")).text().isEqualTo("Login Success").perform();
        }
    }

    @Test
    public void basicAuthenticationWebdriverBidi() {
        if (isChromium()) {
            driver.get().browser().navigateToURLWithBasicAuthentication(TestPageServer.basicAuthUrl(false), "user", "pass", TestPageServer.basicAuthUrl(false));
            driver.get().assertThat().element(By.tagName("h1")).text().isEqualTo("Login Success").perform();
        }
    }

    private static boolean isChromium() {
        String browser = SHAFT.Properties.web.targetBrowserName();
        return browser.equalsIgnoreCase(Browser.CHROME.browserName()) || browser.equalsIgnoreCase(Browser.EDGE.browserName());
    }
}
