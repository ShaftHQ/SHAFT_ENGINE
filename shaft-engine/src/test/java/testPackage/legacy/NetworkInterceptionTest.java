package testPackage.legacy;

import com.google.common.net.MediaType;
import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.remote.Browser;
import org.openqa.selenium.remote.http.Contents;
import org.openqa.selenium.remote.http.HttpMethod;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;
import org.testng.annotations.Test;
import testPackage.Tests;

import java.util.Locale;
import java.util.function.Predicate;

public class NetworkInterceptionTest extends Tests {
    private static final String SHAFT_DOCS_URL = "https://shaftengine.netlify.app/";
    private static final By SHAFT_LOGO = By.xpath("(//img[@alt='SHAFT_Engine'])[1]");

    @Test
    public void interceptShaftLogoAndReplaceItWithYoutubeLogo() {
        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.CHROME.browserName())
                || SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.EDGE.browserName())) {
            // prepare the expected result => should always pass
            driver.get().browser().navigateToURL(SHAFT_DOCS_URL)
                    .element().assertThat(SHAFT_LOGO).matchesReferenceImage().perform();

            //more samples here: https://www.selenium.dev/selenium/docs/api/java/org/openqa/selenium/devtools/NetworkInterceptor.html
            // https://www.selenium.dev/documentation/webdriver/bidirectional/bidi_api/#network-interception
            var mockedResponse = new HttpResponse()
                    .setStatus(200)
                    .addHeader("Content-Type", MediaType.ANY_IMAGE_TYPE.toString())
                    .setContent(Contents.bytes(SHAFT.CLI.file().readFileAsByteArray("youtube.png")));
            Predicate<HttpRequest> requestPredicate = httpRequest -> {
                String uri = httpRequest.getUri().toLowerCase(Locale.ROOT);
                return httpRequest.getMethod() == HttpMethod.GET
                        && uri.contains("shaft")
                        && (uri.contains(".svg") || uri.contains(".png"));
            };

            // mock and compare actual to expected => should always fail
            driver.get().browser().mock(requestPredicate, mockedResponse)
                    .navigateToURL(SHAFT_DOCS_URL)
                    .element().assertThat(SHAFT_LOGO).doesNotMatchReferenceImage().perform();
        }
    }
}
