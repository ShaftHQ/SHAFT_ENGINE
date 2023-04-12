package testPackage;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class iFrameTest {
    SHAFT.GUI.WebDriver driver;
    By iframe_1 = By.name("iframe1");
    By iframe_2 = By.name("iframe2");
    String testPage = "data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>\n" +
            "<html>\n" +
            "  <head>\n" +
            "    <title>Nested iFrames Example</title>\n" +
            "  </head>\n" +
            "  <body>\n" +
            "    <h1>Parent Frame</h1>\n" +
            "    <iframe src=\"data:text/html;base64,PCFET0NUWVBFIGh0bWw+DQo8aHRtbD4NCiAgPGhlYWQ+DQogICAgPHRpdGxlPkNoaWxkIGlGcmFtZSAoRnJhbWUgQSk8L3RpdGxlPg0KICA8L2hlYWQ+DQogIDxib2R5Pg0KICAgIDxoMj5DaGlsZCBpRnJhbWUgKEZyYW1lIEEpPC9oMj4NCiAgICAgIDxpZnJhbWUgc3JjPSJQQ0ZFVDBOVVdWQkZJR2gwYld3K0RRbzhhSFJ0YkQ0TkNpQWdQR2hsWVdRK0RRb2dJQ0FnUEhScGRHeGxQa05vYVd4a0lHbEdjbUZ0WlNBb1JuSmhiV1VnUWlrOEwzUnBkR3hsUGcwS0lDQThMMmhsWVdRK0RRb2dJRHhpYjJSNVBnMEtJQ0FnSUR4b016NURhR2xzWkNCcFJuSmhiV1VnS0VaeVlXMWxJRUlwUEM5b016NE5DaUFnSUNBOGNENVVhR2x6SUdseklIUm9aU0JwYm01bGNtMXZjM1FnYVdaeVlXMWxMand2Y0Q0TkNpQWdQQzlpYjJSNVBnMEtQQzlvZEcxc1BnPT0iIG5hbWU9ImlmcmFtZTIiPjwvaWZyYW1lPg0KICA8L2JvZHk+DQo8L2h0bWw+\" name=\"iframe1\"></iframe>\n" +
            "  </body>\n" +
            "</html>";

    @Test()
    public void switchToIframeAndParentFrame() {
        driver.browser().navigateToURL(testPage);
        driver.browser().switchToIframe(iframe_1);
        var currentFrame = driver.browser().getCurrentFrame();
        driver.browser().switchToDefaultContent();
        var currentFrameAfterSwitchingBackToParentFrame = driver.browser().getCurrentFrame();
        SHAFT.Validations.assertThat().object(currentFrameAfterSwitchingBackToParentFrame).doesNotEqual(currentFrame);
    }

    @Test()
    public void switchToIframeAndParentFrame2() {
        driver.browser().navigateToURL(testPage);
        driver.browser().switchToIframe(iframe_1);
        driver.browser().switchToIframe(iframe_2);
        var currentFrame = driver.browser().getCurrentFrame();
        driver.browser().switchToParentFrame();
        var currentFrameAfterSwitchingBackToParentFrame = driver.browser().getCurrentFrame();
        SHAFT.Validations.assertThat().object(currentFrameAfterSwitchingBackToParentFrame).doesNotEqual(currentFrame);
    }

    @BeforeClass // Set-up method, to be run once before the first test
    public void beforeClass() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterClass(alwaysRun = true)
    public void afterClass() {
        driver.quit();
    }
}