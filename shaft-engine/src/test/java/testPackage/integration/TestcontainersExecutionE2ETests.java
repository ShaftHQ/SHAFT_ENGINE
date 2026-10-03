package testPackage.integration;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.TestcontainersGrid;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

/**
 * {@code executionAddress=testcontainers} (#6405): runs a TestPageServer page in a container when Docker is available
 * and fails fast with a clear message otherwise.
 */
public class TestcontainersExecutionE2ETests {
    private String previousAddress;
    private SHAFT.GUI.WebDriver driver;

    /**
     * Opens a local fixture page through a Selenium standalone container.
     */
    @Test
    public void runsTestPageServerPageInContainer() {
        if (!TestcontainersGrid.isAvailable()) {
            throw new SkipException("Docker is not available.");
        }
        useTestcontainers();
        driver = new SHAFT.GUI.WebDriver();
        driver.browser().navigateToURL(TestPageServer.url("clickableFixture.html"))
                .assertThat().title().contains("SHAFT Clickable Fixture").perform();
    }

    /**
     * Without Docker the mode stops before any driver work with an actionable message.
     */
    @Test
    public void failsFastWithoutDocker() {
        if (TestcontainersGrid.isAvailable()) {
            throw new SkipException("Docker is available.");
        }
        useTestcontainers();
        IllegalStateException error = Assert.expectThrows(IllegalStateException.class, DriverFactoryHelper::initializeSystemProperties);
        Assert.assertTrue(error.getMessage().contains("Docker"), error.getMessage());
        Assert.expectThrows(IllegalStateException.class, () -> TestcontainersGrid.hubUrl("chrome"));
    }

    /**
     * Maps browsers to Selenium standalone images.
     */
    @Test
    public void mapsBrowserToStandaloneImage() {
        Assert.assertEquals(TestcontainersGrid.image("Firefox"), "selenium/standalone-firefox:latest");
        Assert.assertEquals(TestcontainersGrid.image("MicrosoftEdge"), "selenium/standalone-edge:latest");
        Assert.assertEquals(TestcontainersGrid.image(null), "selenium/standalone-chrome:latest");
    }

    private void useTestcontainers() {
        previousAddress = SHAFT.Properties.platform.executionAddress();
        SHAFT.Properties.platform.set().executionAddress(TestcontainersGrid.EXECUTION_ADDRESS);
    }

    /**
     * Quits the driver and restores the execution address.
     */
    @AfterMethod(alwaysRun = true)
    public void restore() {
        if (driver != null) {
            driver.quit();
            driver = null;
        }
        if (previousAddress != null) {
            SHAFT.Properties.platform.set().executionAddress(previousAddress);
            previousAddress = null;
        }
    }
}
