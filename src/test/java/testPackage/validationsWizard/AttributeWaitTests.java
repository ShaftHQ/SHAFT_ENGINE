package testPackage.validationsWizard;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.*;

public class AttributeWaitTests {
    private static final double defaultElementIdentificationTimeout = SHAFT.Properties.timeouts.defaultElementIdentificationTimeout();

    private final String URL = "https://the-internet.herokuapp.com/dynamic_loading/1";
    private final By buttonStart = By.cssSelector("#start button");
    private final By divFinish = By.id("finish");
    private SHAFT.GUI.WebDriver driver;

    @BeforeMethod
    void setup() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @Test
    void testWaitToAttribute() {
        driver.browser().navigateToURL(URL);
        driver.element().click(buttonStart);
        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase("safari")) {
            driver.element().waitToAttribute(divFinish, "style", "display: none;");
        } else {
            driver.element().waitToAttribute(divFinish, "style", "");
        }
        driver.assertThat().element(divFinish).isVisible().perform();
    }

    @Test(expectedExceptions = AssertionError.class)
    void testWaitToAttributeWithWrongAttribute() {
        driver.browser().navigateToURL(URL);
        driver.element().click(buttonStart).waitToAttribute(divFinish, "name", "test");
    }

    @Test(expectedExceptions = AssertionError.class)
    void testWaitToAttributeWithWrongAttributeValue() {
        driver.browser().navigateToURL(URL);
        driver.element().click(buttonStart).waitToAttribute(divFinish, "style", "test");
    }

    @AfterMethod
    void tearDown() {
        driver.quit();
    }

    @BeforeClass
    public void beforeClass() {
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(2);
    }

    @AfterClass(alwaysRun = true)
    public void afterClass() {
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(defaultElementIdentificationTimeout);
    }
}
