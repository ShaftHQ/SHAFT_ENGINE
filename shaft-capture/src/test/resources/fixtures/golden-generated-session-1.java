package generated.capture;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Role;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WindowType;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

public class Session1Test {
    private static final By USERNAME_INPUT_LOCATOR = SHAFT.GUI.Locator.inputField("Username");

    private SHAFT.GUI.WebDriver driver;
    private SHAFT.TestData.JSON testData;
    private final Map<String, String> windows = new HashMap<>();

    @BeforeMethod
    public void setUp() {
        driver = new SHAFT.GUI.WebDriver(DriverFactory.DriverType.CHROME);
        testData = new SHAFT.TestData.JSON("session1-test.json");
        windows.put("window-1", driver.getDriver().getWindowHandle());
    }

    @Test
    public void replaySession1() throws Exception {
        driver.browser().navigateToURL("https://example.test/form");
        driver.element().click(USERNAME_INPUT_LOCATOR);
        driver.element().type(USERNAME_INPUT_LOCATOR, requiredData("username"));
        driver.element().clear(USERNAME_INPUT_LOCATOR);
        driver.element().select(USERNAME_INPUT_LOCATOR, requiredData("username"));
        if (driver.getDriver().findElement(USERNAME_INPUT_LOCATOR).isSelected() != true) {
            driver.element().click(USERNAME_INPUT_LOCATOR);
        }
        if (driver.getDriver().findElement(USERNAME_INPUT_LOCATOR).isSelected() != false) {
            driver.element().click(USERNAME_INPUT_LOCATOR);
        }
        driver.element().typeFileLocationForUpload(USERNAME_INPUT_LOCATOR, Path.of("src/test/resources/test-data/uploads/avatar.png").toString());
        driver.element().typeAppend(USERNAME_INPUT_LOCATOR, Keys.chord(Keys.CONTROL, "A"));
        driver.getDriver().switchTo().newWindow(WindowType.TAB);
        windows.put("window-2", driver.getDriver().getWindowHandle());
        driver.element().switchToIframe(USERNAME_INPUT_LOCATOR);
        driver.alert().typeIntoPromptAlert(requiredData("username"));
        driver.element().waitUntil(ExpectedConditions.visibilityOfElementLocated(USERNAME_INPUT_LOCATOR), java.time.Duration.ofSeconds(10));
        driver.assertThat().element(USERNAME_INPUT_LOCATOR).text().isEqualTo(requiredData("username")).perform();
        // Recorded checkpoint checkpoint-1 (ASSERTION).
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
