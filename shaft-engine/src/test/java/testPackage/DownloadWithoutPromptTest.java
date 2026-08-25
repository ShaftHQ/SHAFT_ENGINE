package testPackage;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;

public class DownloadWithoutPromptTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeMethod(alwaysRun = true)
    public void init() {
        SHAFT.Properties.web.set().headlessExecution(true).targetBrowserName("chrome").incognitoMode(false);
        SHAFT.Properties.flags.set().automaticallyAddRecommendedChromeOptions(false);
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void tear() {
        try {
            if (driver.get() != null) {
                driver.get().quit();
            }
        } finally {
            driver.remove();
            Properties.clearForCurrentThread();
        }
    }

    @Test
    public void chromeShouldSaveAttachmentToConfiguredDownloadsFolderWithoutPrompt() throws Exception {
        String fileName = "shaft-silent-" + UUID.randomUUID() + ".bin";
        Path downloadDir = Path.of(System.getProperty("user.dir"),
                SHAFT.Properties.paths.downloads().replace("/", File.separator));
        Files.createDirectories(downloadDir);
        Path downloaded = downloadDir.resolve(fileName);

        driver.get().browser()
                .navigateToURL(TestPageServer.downloadPageUrl(fileName))
                .element()
                .click(By.id("download-link"));

        long deadline = System.currentTimeMillis() + 20_000L;
        while (System.currentTimeMillis() < deadline
                && !(Files.isRegularFile(downloaded) && Files.size(downloaded) > 0)) {
            Thread.sleep(200L);
        }

        Assert.assertTrue(Files.isRegularFile(downloaded), "Expected download at " + downloaded);
        Assert.assertEquals(Files.readString(downloaded), "SHAFT silent download payload\n");
        Files.deleteIfExists(downloaded);
    }
}
