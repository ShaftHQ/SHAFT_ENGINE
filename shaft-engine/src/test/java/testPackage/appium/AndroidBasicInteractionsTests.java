package testPackage.appium;

import com.google.common.collect.ImmutableMap;
import com.shaft.driver.SHAFT;
import com.shaft.gui.driver.MobileRecordingOptions;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.image.ImageMatchingMode;
import com.shaft.gui.image.ImageTarget;
import com.shaft.gui.ocr.OcrTarget;
import io.appium.java_client.AppiumBy;
import io.appium.java_client.android.AndroidDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.WebDriverException;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.time.Duration;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipFile;

public class AndroidBasicInteractionsTests extends MobileTest {
    private final String PACKAGE = "io.appium.android.apis";

    /**
     * Diagnostic test: just launches the ApiDemos app and verifies the home screen is visible.
     * This test is used to confirm the app is actually starting and rendering on BrowserStack.
     */
    @Test(groups = {"ApiDemosDebug"})
    public void testAppLaunch() {
        driver.get().assertThat().element(By.xpath("//android.widget.TextView[@text='API Demos']")).exists().perform();
    }

    /** Real-provider acceptance for the bounded Android performance-data namespace. */
    @Test(groups = {"ApiDemosDebug"})
    public void performanceDataShouldExposeAProviderSampleAndBoundedHistory() {
        var performance = driver.get().mobile().performance();

        Assert.assertTrue(performance.supportedTypes().contains("memoryinfo"));
        var sample = performance.sample(PACKAGE, "memoryinfo");

        Assert.assertEquals(sample.applicationId(), PACKAGE);
        Assert.assertEquals(sample.dataType(), "memoryinfo");
        Assert.assertFalse(sample.columns().isEmpty());
        Assert.assertEquals(performance.history(), java.util.List.of(sample));
    }

    /** Real-provider acceptance for bounded Android screen recording and exact-target saving. */
    @Test(groups = {"ApiDemosDebug", "mobile-recording-compatible-provider"})
    public void screenRecordingShouldReturnAndSaveBoundedMedia() throws Exception {
        long maxBytes = 32L * 1024 * 1024;
        var options = new MobileRecordingOptions(Duration.ofSeconds(30), maxBytes);
        var recording = driver.get().mobile().recording();
        AndroidDriver nativeDriver = (AndroidDriver) driver.get().getDriver();
        Path directory = Files.createTempDirectory("shaft-android-recording");
        Path target = directory.resolve("recording.mp4");
        try {
            recording.start(options);
            nativeDriver.runAppInBackground(Duration.ofSeconds(2));
            byte[] inline = recording.stop();
            Assert.assertTrue(inline.length > 0 && inline.length <= maxBytes);

            recording.start(options);
            nativeDriver.runAppInBackground(Duration.ofSeconds(2));
            Assert.assertEquals(recording.stopAndSave(target), target.toAbsolutePath().normalize());
            Assert.assertTrue(Files.size(target) > 0 && Files.size(target) <= maxBytes);
        } finally {
            Files.deleteIfExists(target);
            Files.deleteIfExists(directory);
        }
    }

    /** Real-provider proof that BrowserStack rejects Appium recording without corrupting SHAFT's lifecycle. */
    @Test(groups = {"ApiDemosDebug"})
    public void screenRecordingShouldPreserveUnsupportedProviderFailureAndResetState() {
        var recording = driver.get().mobile().recording();

        WebDriverException firstFailure = Assert.expectThrows(WebDriverException.class, recording::start);
        Assert.assertTrue(firstFailure.getMessage().contains("Command is not supported"));
        Assert.expectThrows(IllegalStateException.class, () -> recording.stop());

        WebDriverException retryFailure = Assert.expectThrows(WebDriverException.class, recording::start);
        Assert.assertTrue(retryFailure.getMessage().contains("Command is not supported"));
    }

    /** Real-provider acceptance for bounded current-session Evidence archive publication. */
    @Test(groups = {"ApiDemosDebug", "mobile-evidence-real-provider"})
    public void mobileEvidenceShouldPublishAResolvedBoundedArchive() throws Exception {
        Path directory = Files.createTempDirectory("shaft-android-evidence");
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

    @Test(groups = {"ApiDemosDebug"})
    public void wizard_scrollInExpandableLists_verticalScrolling_insideScreen() {
        ((AndroidDriver) driver.get().getDriver()).runAppInBackground(Duration.ofSeconds(5));
        driver.get().element().performTouchAction()
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Expandable Lists"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Expandable Lists"))
                .swipeElementIntoView(AppiumBy.accessibilityId("3. Simple Adapter"), TouchActions.SwipeDirection.DOWN)
//                .swipeElementIntoView(AppiumBy.accessibilityId("3. Simple Adapter"), TouchActions.SwipeDirection.DOWN);
//        shaftDriver.verifyThat().element(AppiumBy.accessibilityId("3. Simple Adapter")).matchesReferenceImage().perform();
//        shaftDriver.element().performTouchAction()
                .tap(AppiumBy.accessibilityId("3. Simple Adapter"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 18']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.TextView[@text='Group 18']"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Child 13']"), TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 1']"), TouchActions.SwipeDirection.UP)
                .sendAppToBackground(1)
                .assertThat(By.xpath("//android.widget.TextView[@text='Group 1']")).exists();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void scrollInExpandableLists_verticalScrolling_insideScreen() {
        driver.get().touch()
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Expandable Lists"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Expandable Lists"))
                .swipeElementIntoView(AppiumBy.accessibilityId("3. Simple Adapter"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("3. Simple Adapter"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 18']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.TextView[@text='Group 18']"))
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Child 13']"), TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(By.xpath("//android.widget.TextView[@text='Group 1']"), TouchActions.SwipeDirection.UP)
                .sendAppToBackground();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void scrollInExpandableLists_verticalScrolling_insideElement(){
        driver.get().touch()
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Splitting Touches across Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Splitting Touches across Views"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list2"), By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Blue']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Blue']"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list2"), By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Abbaye de Belloc']"), TouchActions.SwipeDirection.UP)
                .tap(By.xpath("//android.widget.ListView[2]/android.widget.TextView[@text='Abbaye de Belloc']"))
                .assertThat(By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Abbaye de Belloc']")).exists();

    }

    @Test(groups = {"ApiDemosDebug"})
    public void scrollInExpandableLists_verticalScrolling_insideElement2(){
        driver.get().touch()
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Splitting Touches across Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Splitting Touches across Views"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list1"), By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Blue']"), TouchActions.SwipeDirection.DOWN)
                .tap(By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Blue']"))
                .swipeElementIntoView(By.id("io.appium.android.apis:id/list1"), By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Abbaye de Belloc']"), TouchActions.SwipeDirection.UP)
                .assertThat(By.xpath("//android.widget.ListView[1]/android.widget.TextView[@text='Abbaye de Belloc']")).exists();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void scrollInExpandableLists_horizontalScrolling_insideElement(){
        driver.get().touch()
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Tabs"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Tabs"))
                .swipeElementIntoView(AppiumBy.accessibilityId("5. Scrollable"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("5. Scrollable"))
                .swipeElementIntoView(By.xpath("//android.widget.HorizontalScrollView"), By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 12']"), TouchActions.SwipeDirection.RIGHT)
                .tap(By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 12']"))
                .swipeElementIntoView(By.xpath("//android.widget.HorizontalScrollView"), By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 1']"), TouchActions.SwipeDirection.LEFT)
                .assertThat(By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 1']")).exists();
    }

    /** Opt-in real-device proof for screenshot and OCR scrolling in both axes. */
    @Test(groups = {"ApiDemosDebug", "visual-ocr-mobile-acceptance"})
    public void visualAndOcrTargetsShouldScrollVerticallyThroughNativeControls() {
        By group1 = By.xpath("//android.widget.TextView[@text='Group 1']");
        By group18 = By.xpath("//android.widget.TextView[@text='Group 18']");
        driver.get().touch()
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Expandable Lists"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Expandable Lists"))
                .tap(AppiumBy.accessibilityId("3. Simple Adapter"));

        byte[] group1Screenshot = driver.get().getDriver().findElement(group1).getScreenshotAs(OutputType.BYTES);
        driver.get().touch()
                .swipeElementIntoView(group18, TouchActions.SwipeDirection.DOWN)
                .swipeElementIntoView(ImageTarget.fromBytes(group1Screenshot).matchingMode(ImageMatchingMode.AUTO),
                        TouchActions.SwipeDirection.UP);
        Assert.assertTrue(driver.get().getDriver().findElement(group1).isDisplayed());

        driver.get().touch().swipeElementIntoView(OcrTarget.exact("Group 18"), TouchActions.SwipeDirection.DOWN);
        Assert.assertTrue(driver.get().getDriver().findElement(group18).isDisplayed());
    }

    /** Opt-in real-device proof for screenshot and OCR scrolling inside a horizontal native control. */
    @Test(groups = {"ApiDemosDebug", "visual-ocr-mobile-acceptance"})
    public void visualAndOcrTargetsShouldScrollHorizontallyInsideNativeControl() {
        By tabs = By.xpath("//android.widget.HorizontalScrollView");
        By tab1 = By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 1']");
        By tab12 = By.xpath("//android.widget.HorizontalScrollView//android.widget.TextView[@text='TAB 12']");
        driver.get().touch()
                .swipeElementIntoView(AppiumBy.accessibilityId("Views"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Views"))
                .swipeElementIntoView(AppiumBy.accessibilityId("Tabs"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("Tabs"))
                .swipeElementIntoView(AppiumBy.accessibilityId("5. Scrollable"), TouchActions.SwipeDirection.DOWN)
                .tap(AppiumBy.accessibilityId("5. Scrollable"));

        byte[] tab1Screenshot = driver.get().getDriver().findElement(tab1).getScreenshotAs(OutputType.BYTES);
        driver.get().touch()
                .swipeElementIntoView(tabs, tab12, TouchActions.SwipeDirection.RIGHT)
                .swipeElementIntoView(tabs,
                        ImageTarget.fromBytes(tab1Screenshot).matchingMode(ImageMatchingMode.AUTO),
                        TouchActions.SwipeDirection.LEFT);
        Assert.assertTrue(driver.get().getDriver().findElement(tab1).isDisplayed());

        driver.get().touch().swipeElementIntoView(tabs, OcrTarget.exact("TAB 12"), TouchActions.SwipeDirection.RIGHT);
        Assert.assertTrue(driver.get().getDriver().findElement(tab12).isDisplayed());
    }

    @Test(groups = {"ApiDemosDebug"})
    public void visualElementIdentification_samedpi() {
        var referenceImageFile = "content.png";
        if (SHAFT.Properties.platform.executionAddress().toLowerCase().contains("browserstack")) {
            referenceImageFile = "content_local.png";
        }

        var elementReferenceFilePath = "src/main/resources/dynamicObjectRepository/Android/" + referenceImageFile;
        driver.get().touch()
                .swipeElementIntoView(elementReferenceFilePath, TouchActions.SwipeDirection.DOWN)
                .waitUntilElementIsVisible(elementReferenceFilePath)
                .tap(elementReferenceFilePath);

        driver.get().assertThat().element(AppiumBy.accessibilityId("Assets")).exists().perform();
    }

    //    @Test(groups = {"ApiDemosDebug"})
    public void visualElementIdentification_requiresProcessing() {
        driver.get().touch()
                .swipeElementIntoView("src/main/resources/dynamicObjectRepository/content2.png", TouchActions.SwipeDirection.DOWN)
                .tap("src/main/resources/dynamicObjectRepository/content2.png");

        driver.get().assertThat().element(AppiumBy.accessibilityId("Assets")).exists().perform();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void testSendKeys() {
        String SEARCH_ACTIVITY = ".app.SearchInvoke";

        ((AndroidDriver) driver.get().getDriver()).executeScript("mobile: startActivity", ImmutableMap.of("intent", PACKAGE + "/" + SEARCH_ACTIVITY));
//        ((AndroidDriver) driver.get().getDriver()).startActivity(new Activity(PACKAGE, SEARCH_ACTIVITY));

        driver.get().element().type(By.id("txt_query_prefill"), "Hello world!")
                .and().touch().tap(By.id("btn_start_search"))
                .and().assertThat(By.id("android:id/search_src_text")).text().isEqualTo("Hello world!").perform();
    }

    @Test(groups = {"ApiDemosDebug"})
    public void testOpensAlert() {
        // Open the "Alert Dialog" activity of the android app
        String ALERT_DIALOG_ACTIVITY = ".app.AlertDialogSamples";

        ((AndroidDriver) driver.get().getDriver()).executeScript("mobile: startActivity", ImmutableMap.of("intent", PACKAGE + "/" + ALERT_DIALOG_ACTIVITY));
//        ((AndroidDriver) driver.get().getDriver()).startActivity(new Activity(PACKAGE, ALERT_DIALOG_ACTIVITY));

        // Click button that opens a dialog
        driver.get().element().touch().tap(By.id("io.appium.android.apis:id/two_buttons"));

        // Check that the dialog is there
        driver.get().verifyThat()
                .element(By.id("android:id/alertTitle"))
                .text()
                .isEqualTo("Lorem ipsum dolor sit aie consectetur adipiscing\nPlloaso mako nuto siwuf cakso dodtos anr koop.")
                .perform();

        // Close the dialog
        driver.get().element().touch().tap(By.id("android:id/button1"));
    }
}
