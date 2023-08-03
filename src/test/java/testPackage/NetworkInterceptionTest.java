package testPackage;

import com.google.common.net.MediaType;
import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.function.Predicate;

public class NetworkInterceptionTest {
    SHAFT.GUI.WebDriver driver;

    @BeforeMethod
    public void setup() {
        driver = SHAFT.GUI.WebDriver.getInstance();
    }

    @AfterMethod
    public void tearDown() {
        driver.quit();
    }

    @Test(expectedExceptions = {AssertionError.class})
    public void interceptShaftLogoAndReplaceItWithYoutubeLogo() {
        //more samples here: https://www.selenium.dev/selenium/docs/api/java/org/openqa/selenium/devtools/NetworkInterceptor.html
        // https://www.selenium.dev/documentation/webdriver/bidirectional/bidi_api/#network-interception
        var mockedResponse = new HttpResponse()
                .setStatus(200)
                .addHeader("Content-Type", MediaType.ANY_IMAGE_TYPE.toString())
                .setContent(Contents.bytes(SHAFT.CLI.file().readFileAsByteArray("sikulixElements/youtube.png")));
        Predicate<HttpRequest> requestPredicate = httpRequest -> httpRequest.getMethod() == HttpMethod.GET && httpRequest.getUri().endsWith("/img/shaft.svg");

        driver.browser().mock(requestPredicate, mockedResponse)
                .navigateToURL("https://shafthq.github.io/")
                .element().assertThat(By.xpath("//img[@alt='SHAFT_Engine']")).matchesReferenceImage().perform();
    }


}
