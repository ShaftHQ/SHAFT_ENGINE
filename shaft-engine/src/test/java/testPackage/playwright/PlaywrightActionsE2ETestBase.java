package testPackage.playwright;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.CheckpointCounter;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.TestPageServer;

import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

@SuppressWarnings("PMD.AvoidAccessibilityAlteration")
public abstract class PlaywrightActionsE2ETestBase {
    private static final ThreadLocal<SHAFT.GUI.Playwright> DRIVER = new ThreadLocal<>();
    private static final Field CHECKPOINTS_FIELD;

    static {
        try {
            CHECKPOINTS_FIELD = CheckpointCounter.class.getDeclaredField("checkpoints");
            CHECKPOINTS_FIELD.setAccessible(true);
        } catch (NoSuchFieldException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    @BeforeMethod(alwaysRun = true)
    public void setUpPlaywright() {
        Properties.clearForCurrentThread();
        SHAFT.Properties.web.set()
                .headlessExecution(true)
                .browserWindowWidth(1280)
                .browserWindowHeight(720);
        SHAFT.Properties.visuals.set()
                .screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly")
                .screenshotParamsWatermark(false);
        SHAFT.Properties.playwright.set()
                .connectionMode("local")
                .browserName(browserName())
                .channel(channel())
                .deviceName(deviceName())
                .defaultTimeoutMilliseconds(10000)
                .navigationTimeoutMilliseconds(10000);
        DRIVER.set(new SHAFT.GUI.Playwright());
    }

    @AfterMethod(alwaysRun = true)
    public void tearDownPlaywright() {
        try {
            if (DRIVER.get() != null) {
                DRIVER.get().quit();
            }
        } finally {
            DRIVER.remove();
            Properties.clearForCurrentThread();
        }
    }

    @Test
    public void shouldNavigateAndValidateBrowserState() {
        driver().browser().navigateToURL(TestPageServer.url("coverageTestPage.html"));

        driver().assertThat().browser().title().contains("SHAFT Coverage").perform();
        driver().assertThat().browser().url().contains("coverageTestPage.html").perform();
        driver().assertThat().browser().text().contains("Coverage Test Page").perform();
    }

    @Test
    public void shouldPerformElementActions() {
        driver().browser().navigateToURL(TestPageServer.url("coverageTestPage.html"));

        driver().element()
                .clear(By.id("textInput"))
                .type(By.id("textInput"), "Playwright")
                .typeAppend(By.id("textInput"), " backend")
                .select(By.id("selectEl"), "opt2")
                .click(By.id("checkboxUnchecked"));

        driver().assertThat().element(By.id("textInput")).domProperty("value").isEqualTo("Playwright backend").perform();
        driver().assertThat().element(By.id("selectEl")).domProperty("value").isEqualTo("opt2").perform();
        driver().assertThat().element(By.id("checkboxUnchecked")).isChecked().perform();
    }

    @Test
    public void shouldHandleAlerts() {
        driver().browser().navigateToURL(TestPageServer.url("alertFixture.html"));

        driver().alert().acceptAlert();
        driver().element().click(By.id("alert-button"));

        driver().assertThat().object(driver().alert().getAlertText()).isEqualTo("Hello from fixture").perform();
        driver().assertThat().element(By.id("alert-result")).text().isEqualTo("accepted").perform();
    }

    @Test
    public void shouldAttachScreenshotsAndRecordCheckpointsForPlaywrightAssertions() throws Exception {
        driver().browser().navigateToURL(TestPageServer.url("coverageTestPage.html"));
        int baselineCheckpoints = checkpointsSize();
        long baselineScreenshots = screenshotAttachments();

        driver().assertThat().browser().title().contains("SHAFT Coverage").perform();
        driver().assertThat().element(By.id("pageTitle")).text().contains("Coverage Test Page").perform();

        Assert.assertTrue(checkpointsSize() >= baselineCheckpoints + 2,
                "Playwright assertions should increment SHAFT checkpoints.");
        Assert.assertTrue(screenshotAttachments() >= baselineScreenshots + 2,
                "Playwright assertions should attach screenshots.");
    }

    protected SHAFT.GUI.Playwright driver() {
        return DRIVER.get();
    }

    protected String browserName() {
        return "";
    }

    protected String channel() {
        return "";
    }

    protected String deviceName() {
        return "";
    }

    private int checkpointsSize() throws IllegalAccessException {
        return ((ConcurrentHashMap<?, ?>) CHECKPOINTS_FIELD.get(null)).size();
    }

    private long screenshotAttachments() throws IOException {
        long count = 0;
        for (Path directory : List.of(Path.of("allure-results"), Path.of("shaft-engine", "allure-results"))) {
            if (Files.isDirectory(directory)) {
                try (Stream<Path> files = Files.walk(directory)) {
                    count += files.filter(Files::isRegularFile)
                            .filter(path -> path.getFileName().toString().endsWith(".png"))
                            .count();
                }
            }
        }
        return count;
    }
}
