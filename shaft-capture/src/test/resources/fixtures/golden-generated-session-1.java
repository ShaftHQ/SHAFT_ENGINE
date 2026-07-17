package generated.capture;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

public class FormCompletedTest {
    // Capture review: readiness=READY, events=14, fallback locators=1.
    private SHAFT.GUI.WebDriver driver;
    private SHAFT.TestData.JSON testData;
    private final Map<String, String> windows = new HashMap<>();

    @BeforeMethod
    public void setUp() {
        driver = new SHAFT.GUI.WebDriver(DriverFactory.DriverType.CHROME);
        testData = new SHAFT.TestData.JSON("form-completed-test.json");
        windows.put("window-1", driver.browser().getWindowHandle());
    }

    @Test
    public void replayFormCompleted() throws Exception {
        driver.browser().navigateToURL("https://example.test/form");
        driver.element().click(SHAFT.GUI.Locator.hasTagName("input").containsText("Username").build());
        driver.element().type(SHAFT.GUI.Locator.hasTagName("input").containsText("Username").build(), requiredData("username"));
        driver.element().clear(SHAFT.GUI.Locator.hasTagName("input").containsText("Username").build());
        driver.element().select(SHAFT.GUI.Locator.hasTagName("input").containsText("Username").build(), requiredData("username"));
        driver.element().click(SHAFT.GUI.Locator.hasTagName("input").containsText("Username").build());
        driver.element().click(SHAFT.GUI.Locator.hasTagName("input").containsText("Username").build());
        driver.element().typeFileLocationForUpload(SHAFT.GUI.Locator.hasTagName("input").containsText("Username").build(), Path.of("src/test/resources/test-data/uploads/avatar.png").toString());
        // Targeted keyboard shortcut omitted by SHAFT-only codegen.
        driver.browser().openNewTab("about:blank");
        windows.put("window-2", driver.browser().getWindowHandle());
        driver.element().switchToIframe(SHAFT.GUI.Locator.hasTagName("input").containsText("Username").build());
        driver.browser().typeIntoPromptAlert(requiredData("username"));
        driver.element().assertThat(SHAFT.GUI.Locator.hasTagName("input").containsText("Username").build()).isVisible();
        driver.element().assertThat(SHAFT.GUI.Locator.hasTagName("input").containsText("Username").build()).text().isEqualTo(requiredData("username"));
        // Recorded checkpoint checkpoint-1 (ASSERTION). Form completed
    }

    private String requiredData(String key) {
        String value = testData.getTestData("values." + key);
        if (value == null) {
            throw new IllegalStateException("Missing generated test data: " + key);
        }
        return value;
    }

    private String requiredEnvironment(String name) {
        String value = System.getenv(name);
        if (value == null || value.isBlank()) {
            throw new IllegalStateException("Missing required environment variable: " + name);
        }
        return value;
    }

    @AfterMethod(alwaysRun = true)
    public void tearDown() {
        if (driver != null) {
            driver.quit();
        }
    }
}
