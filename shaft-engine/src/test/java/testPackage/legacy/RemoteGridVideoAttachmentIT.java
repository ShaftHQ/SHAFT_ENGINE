package testPackage.legacy;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.stream.Stream;

/**
 * Integration test for verifying video attachment behavior with remote Selenium Grid sessions.
 */
public class RemoteGridVideoAttachmentIT {
    private static final Path ALLURE_RESULTS = Path.of("allure-results");
    private final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() {
        if (!Boolean.parseBoolean(System.getProperty("runGridVideoIT", "false"))) {
            throw new SkipException("Enable with -DrunGridVideoIT=true after starting local selenium grid with video enabled.");
        }
        SHAFT.Properties.platform.set().executionAddress(System.getProperty("gridUrl", "http://localhost:4444/wd/hub"));
        SHAFT.Properties.web.set().headlessExecution(false);
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(true).videoParamsScope("DriverSession");
        driver.set(new SHAFT.GUI.WebDriver(DriverFactory.DriverType.CHROME));
    }

    @Test(description = "Remote Chrome session on local Selenium Grid should produce an Allure video attachment")
    public void remoteGridSessionShouldAttachVideoRecording() throws IOException {
        var d = driver.get();
        d.browser().navigateToURL("https://example.com");
        d.assertThat().browser().title().contains("Example").perform();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws IOException {
        if (driver.get() != null) {
            driver.get().quit();
        }
        if (!Boolean.parseBoolean(System.getProperty("runGridVideoIT", "false"))) {
            return;
        }

        List<Path> resultFiles;
        try (Stream<Path> paths = Files.list(ALLURE_RESULTS)) {
            resultFiles = paths
                    .filter(path -> path.getFileName().toString().endsWith("-result.json"))
                    .sorted(Comparator.comparingLong((Path path) -> path.toFile().lastModified()).reversed())
                    .limit(5)
                    .toList();
        }

        boolean hasVideoAttachment = false;
        for (Path resultFile : resultFiles) {
            String json = Files.readString(resultFile);
            if (json.contains("\"name\":\"Video Recording\"")
                    && json.toLowerCase(Locale.ROOT).contains("video/mp4")) {
                hasVideoAttachment = true;
                break;
            }
        }

        SHAFT.Validations.assertThat().object(hasVideoAttachment)
                .isEqualTo(true).perform();
    }
}
