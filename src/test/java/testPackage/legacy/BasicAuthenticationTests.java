package testPackage.legacy;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.Test;
import testPackage.Tests;

public class BasicAuthenticationTests extends Tests {

    @Test
    public void basicAuthenticationTraditional(){
        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.CHROME.browserName())
                || SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.EDGE.browserName())) {
            driver.get().browser().navigateToURL("https://user:pass@authenticationtest.com/HTTPAuth/", "https://authenticationtest.com/loginSuccess/");
            driver.get().assertThat().element(By.tagName("h1")).text().equals("Login Success");
        }
    }

    @Test
    public void basicAuthenticationWebdriverBidi() {
        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.CHROME.browserName())
                || SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.EDGE.browserName())) {
            driver.get().browser().navigateToURLWithBasicAuthentication("https://authenticationtest.com/HTTPAuth/", "user", "pass", "https://authenticationtest.com/loginSuccess/");
            driver.get().assertThat().element(By.tagName("h1")).text().equals("Login Success");
        }
    }

}
