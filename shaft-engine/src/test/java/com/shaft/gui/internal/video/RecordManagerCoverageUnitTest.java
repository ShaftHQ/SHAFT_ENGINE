package com.shaft.gui.internal.video;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.InputStream;

import static org.mockito.Mockito.mock;

public class RecordManagerCoverageUnitTest {
    private boolean videoRecording;
    private String executionAddress;
    private boolean headlessExecution;

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() {
        videoRecording = SHAFT.Properties.visuals.videoParamsRecordVideo();
        executionAddress = SHAFT.Properties.platform.executionAddress();
        headlessExecution = SHAFT.Properties.web.headlessExecution();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(videoRecording);
        SHAFT.Properties.platform.set().executionAddress(executionAddress);
        SHAFT.Properties.web.set().headlessExecution(headlessExecution);
    }

    @Test(description = "#6834: cloud providers record themselves, so Appium screen recording is never requested there")
    public void cloudProvidersRecordOnProviderSide() {
        Assert.assertTrue(RecordManager.recordsOnProviderSide("https://u:k@hub-cloud.browserstack.com/wd/hub"));
        Assert.assertTrue(RecordManager.recordsOnProviderSide("https://u:k@mobile-hub.lambdatest.com/wd/hub"));
        Assert.assertTrue(RecordManager.recordsOnProviderSide("https://ondemand.eu-central-1.saucelabs.com/wd/hub"));
        Assert.assertFalse(RecordManager.recordsOnProviderSide("http://127.0.0.1:4723"));
        Assert.assertFalse(RecordManager.recordsOnProviderSide("local"));
        Assert.assertFalse(RecordManager.recordsOnProviderSide(null));
    }

    @Test
    public void startVideoRecordingWithRemoteWebDriverShouldNotFallbackToDesktopRecorder() {
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(true);
        SHAFT.Properties.platform.set().executionAddress("local");
        SHAFT.Properties.web.set().headlessExecution(false);

        WebDriver webDriver = mock(RemoteWebDriver.class);
        RecordManager.startVideoRecording(webDriver);

        InputStream videoRecordingInputStream = RecordManager.getVideoRecording();
        Assert.assertNull(videoRecordingInputStream,
                "Desktop recorder fallback should not start when a WebDriver instance is already provided.");
    }
}
