package testPackage;

import com.shaft.driver.SHAFT;
import io.github.bonigarcia.wdm.WebDriverManager;
import org.openqa.selenium.By;
import org.openqa.selenium.HasAuthentication;
import org.openqa.selenium.UsernameAndPassword;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.Augmenter;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.concurrent.atomic.AtomicReference;

public class BasicAuthenticationTests {
    SHAFT.GUI.WebDriver driver;

    @Test
    public void basicAuthentication_traditional(){
            driver.browser().navigateToURL("https://user:pass@authenticationtest.com/HTTPAuth/", "https://authenticationtest.com/loginSuccess/");
            driver.assertThat().element(By.tagName("h1")).text().equals("Login Success");
    }

    @Test
    public void basicAuthentication_webdriverBiDi() {
            driver.browser().navigateToURLWithBasicAuthentication("https://authenticationtest.com/HTTPAuth/", "user", "pass", "https://authenticationtest.com/loginSuccess/");
            driver.assertThat().element(By.tagName("h1")).text().equals("Login Success");
    }

    //@Test
    public void testBasicAuth() throws MalformedURLException {
        //nattive selenium test with bidi/oauth
        //this test is currently broken, I think there's an issue with this feature from Selenium's side
        // https://www.selenium.dev/documentation/webdriver/bidirectional/bidi_api_remotewebdriver/
        WebDriverManager.chromedriver().setup();

        var gridUrl = new URL("http://localhost:4444/");
        ChromeOptions options = new ChromeOptions();
        WebDriver driver = new RemoteWebDriver(gridUrl, options);
        AtomicReference<DevTools> devToolsAtomicReference = new AtomicReference<>();

        driver = new Augmenter().addDriverAugmentation("chrome",
                HasAuthentication.class,
                (caps, exec) -> (whenThisMatches, useTheseCredentials) -> {
                    devToolsAtomicReference.get()
                            .createSessionIfThereIsNotOne();
                    devToolsAtomicReference.get().getDomains()
                            .network()
                            .addAuthHandler(whenThisMatches,
                                    useTheseCredentials);

                }).augment(driver);

        DevTools devTools = ((HasDevTools) driver).getDevTools();
        devTools.createSession();
        devToolsAtomicReference.set(devTools);
        ((HasAuthentication) driver).register(UsernameAndPassword.of("admin", "admin"));
        driver.get("https://the-internet.herokuapp.com/basic_auth");
        var text = driver.findElement(By.tagName("p")).getText();
        SHAFT.Validations.assertThat().object(text).equals("Congratulations! You must have the proper credentials.");
    }

    @BeforeMethod
    public void beforeMethod(){
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod(){
        driver.quit();
    }
}
