package testPackage.appium;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.driver.MobileRecordingOptions;
import com.shaft.gui.image.ImageTarget;
import com.shaft.gui.ocr.OcrTarget;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.Validations;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.Platform;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.WebDriverException;
import org.testng.SkipException;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.concurrent.TimeUnit;
import java.util.zip.ZipFile;

public class IOSBasicInteractionsTest {
    private static final String ENABLE_NATIVE_IOS_E2E_PROPERTY = "shaft.enableNativeIosE2E";
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void test() {
        new ElementActions(driver.get().getDriver()).performTouchAction().tap(AppiumBy.accessibilityId("Text Button"));
        new ElementActions(driver.get().getDriver()).type(AppiumBy.accessibilityId("Text Input"), "hello@browserstack.com" + "\n");
        Validations.assertThat()
                .element(driver.get().getDriver(), AppiumBy.accessibilityId("Text Output"))
                .text()
                .isEqualTo("hello@browserstack.com")
                .perform();
    }

    /** Opt-in real-device proof that iOS can interact through device screenshots and OCR text. */
    @Test(groups = {"visual-ocr-mobile-acceptance"})
    public void visualAndOcrTargetsShouldInteractWithNativeControls() {
        byte[] inputScreenshot = driver.get().getDriver().findElement(AppiumBy.accessibilityId("Text Input"))
                .getScreenshotAs(OutputType.BYTES);

        driver.get().touch()
                .tap(ImageTarget.fromBytes(inputScreenshot));
        Assert.assertEquals(driver.get().getDriver().switchTo().activeElement().getAttribute("name"), "Text Input");

        ((IOSDriver) driver.get().getDriver()).hideKeyboard();
        Assert.assertNotEquals(driver.get().getDriver().switchTo().activeElement().getAttribute("name"), "Text Input");
        driver.get().touch().tap(OcrTarget.exact("Text Input"));
        Assert.assertEquals(driver.get().getDriver().switchTo().activeElement().getAttribute("name"), "Text Input");
        driver.get().element().type(AppiumBy.accessibilityId("Text Input"), "visual ocr ios" + "\n");

        Validations.assertThat()
                .element(driver.get().getDriver(), AppiumBy.accessibilityId("Text Output"))
                .text()
                .isEqualTo("visual ocr ios")
                .perform();
    }

    /** Real-provider acceptance for bounded iOS screen recording and exact-target saving. */
    @Test(groups = {"mobile-recording-compatible-provider"})
    public void screenRecordingShouldReturnAndSaveBoundedMedia() throws Exception {
        long maxBytes = 32L * 1024 * 1024;
        var options = new MobileRecordingOptions(Duration.ofSeconds(30), maxBytes);
        var recording = driver.get().mobile().recording();
        Path directory = Files.createTempDirectory("shaft-ios-recording");
        Path target = directory.resolve("recording.mp4");
        try {
            recording.start(options);
            TimeUnit.SECONDS.sleep(2);
            byte[] inline = recording.stop();
            Assert.assertTrue(inline.length > 0 && inline.length <= maxBytes);

            recording.start(options);
            TimeUnit.SECONDS.sleep(2);
            Assert.assertEquals(recording.stopAndSave(target), target.toAbsolutePath().normalize());
            Assert.assertTrue(Files.size(target) > 0 && Files.size(target) <= maxBytes);
        } finally {
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    /** Real-provider proof that BrowserStack rejects Appium recording without corrupting SHAFT's lifecycle. */
    @Test
    public void screenRecordingShouldPreserveUnsupportedProviderFailureAndResetState() {
        var recording = driver.get().mobile().recording();

        WebDriverException firstFailure = Assert.expectThrows(WebDriverException.class, recording::start);
        Assert.assertTrue(firstFailure.getMessage().contains("Command is not supported"));
        Assert.expectThrows(IllegalStateException.class, () -> recording.stop());

        WebDriverException retryFailure = Assert.expectThrows(WebDriverException.class, recording::start);
        Assert.assertTrue(retryFailure.getMessage().contains("Command is not supported"));
    }

    /** Real-provider acceptance for bounded current-session Evidence archive publication. */
    @Test(groups = {"mobile-evidence-real-provider"})
    public void mobileEvidenceShouldPublishAResolvedBoundedArchive() throws Exception {
        Path directory = Files.createTempDirectory("shaft-ios-evidence");
        Path target = directory.resolve("mobile-evidence.zip");
        int maxArtifactMb = SHAFT.Properties.reporting.traceMaxArtifactMb();
        boolean screenshots = SHAFT.Properties.reporting.traceIncludeScreenshots();
        boolean nativeSource = SHAFT.Properties.reporting.traceIncludeNativePageSource();
        try {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(true)
                    .traceIncludeNativePageSource(true);
            var bundle = driver.get().mobile().evidence().capture(target);

            Assert.assertEquals(bundle.archive(), target.toAbsolutePath().normalize());
            Assert.assertTrue(Files.size(target) > 0);
            Assert.assertEquals(bundle.artifacts().size(), 3);
            var screenshot = bundle.artifacts().stream()
                    .filter(artifact -> artifact.id().equals("screenshot")).findFirst().orElseThrow();
            var source = bundle.artifacts().stream()
                    .filter(artifact -> artifact.id().equals("source")).findFirst().orElseThrow();
            Assert.assertFalse(screenshot.omitted());
            Assert.assertFalse(source.omitted());
            try (ZipFile archive = new ZipFile(target.toFile())) {
                Assert.assertNotNull(archive.getEntry("mobile-evidence.json"));
                bundle.artifacts().forEach(artifact -> Assert.assertNotNull(archive.getEntry(artifact.path())));
                byte[] screenshotBytes = archive.getInputStream(archive.getEntry(screenshot.path())).readAllBytes();
                Assert.assertTrue(screenshotBytes.length > 8);
                Assert.assertEquals(java.util.Arrays.copyOf(screenshotBytes, 8),
                        new byte[]{(byte) 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A});
                byte[] sourceBytes = archive.getInputStream(archive.getEntry(source.path())).readAllBytes();
                Assert.assertTrue(sourceBytes.length > 0);
                long uncompressedBytes = archive.stream().mapToLong(java.util.zip.ZipEntry::getSize).sum();
                Assert.assertTrue(uncompressedBytes <= (long) maxArtifactMb * 1024 * 1024);
            }
        } finally {
            SHAFT.Properties.reporting.set()
                    .traceIncludeScreenshots(screenshots)
                    .traceIncludeNativePageSource(nativeSource);
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    @SuppressWarnings("CommentedOutCode")
    @BeforeMethod
    public void setup() {
        if (!Boolean.getBoolean(ENABLE_NATIVE_IOS_E2E_PROPERTY)) {
            throw new SkipException("Native iOS BrowserStack E2E is disabled for the web Safari matrix. Set -D"
                    + ENABLE_NATIVE_IOS_E2E_PROPERTY + "=true in a native iOS job after validating the app upload.");
        }
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.IOS.toString());
        SHAFT.Properties.mobile.set().automationName("XCUITest");
        SHAFT.Properties.mobile.set().browserName("");
        System.setProperty("mobile_appWaitActivity", "*");

        // local self-managed instance routing to browserstack for ios
//        SHAFT.Properties.mobile.set().selfManaged(true);

//         local appium server (for local and GitHub actions execution)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
//        SHAFT.Properties.mobile.set().app(SHAFT.Properties.paths.testData() + "apps/BStackSampleApp.ipa");

        // remote browserstack server (new app version)
        SHAFT.Properties.platform.set().executionAddress("browserstack");
        SHAFT.Properties.browserStack.set().platformVersion("16");
        SHAFT.Properties.browserStack.set().deviceName("iPhone 14");
        SHAFT.Properties.browserStack.set().appName("BStackSampleApp.ipa");
        SHAFT.Properties.browserStack.set().appRelativeFilePath(SHAFT.Properties.paths.testData() +  "apps/BStackSampleApp.ipa");
        SHAFT.Properties.browserStack.set().appUrl("");

        // remote browserstack server (existing app version)
//        System.setProperty("browserStack.platformVersion", "14");
//        System.setProperty("browserStack.deviceName", "iPhone 12 Pro Max");
//        System.setProperty("browserStack.appName", "");
//        System.setProperty("browserStack.appRelativeFilePath", "");
//        System.setProperty("browserStack.appUrl", "bs://e2c374a22cf954e582b5c02e9a9f7cfd650a8325");
        driver.set(new SHAFT.GUI.WebDriver());

    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        if (driver.get() != null) {
            driver.get().quit();
            driver.remove();
        }
        Properties.clearForCurrentThread();
    }
}
