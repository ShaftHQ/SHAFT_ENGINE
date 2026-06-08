package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class VisualsTests {
    double visualMatchingThreshold;
    double screenshotParamsScalingFactor;
    String screenshotParamsWhenToTakeAScreenshot;
    String screenshotParams_screenshotType;
    boolean screenshotParamsHighlightElements;
    String screenshotParamsHighlightMethod;
    String screenshotParamsSkippedElementsFromScreenshot;
    boolean screenshotParamsWatermark;
    float screenshotParamsWatermarkOpacity;
    boolean createAnimatedGif;
    int animatedGifFrameDelay;
    boolean videoParamsRecordVideo;
    String videoParamsScope;
    String whenToTakePageSourceSnapshot;


    @BeforeClass
    public void beforeClass() {
        visualMatchingThreshold = SHAFT.Properties.visuals.visualMatchingThreshold();
        screenshotParamsScalingFactor = SHAFT.Properties.visuals.screenshotParamsScalingFactor();
        screenshotParamsWhenToTakeAScreenshot = SHAFT.Properties.visuals.screenshotParamsWhenToTakeAScreenshot();
        screenshotParams_screenshotType = SHAFT.Properties.visuals.screenshotParamsScreenshotType();
        screenshotParamsHighlightElements = SHAFT.Properties.visuals.screenshotParamsHighlightElements();
        screenshotParamsHighlightMethod = SHAFT.Properties.visuals.screenshotParamsHighlightMethod();
        screenshotParamsSkippedElementsFromScreenshot = SHAFT.Properties.visuals.screenshotParamsSkippedElementsFromScreenshot();
        screenshotParamsWatermark = SHAFT.Properties.visuals.screenshotParamsWatermark();
        screenshotParamsWatermarkOpacity = SHAFT.Properties.visuals.screenshotParamsWatermarkOpacity();
        createAnimatedGif = SHAFT.Properties.visuals.createAnimatedGif();
        animatedGifFrameDelay = SHAFT.Properties.visuals.animatedGifFrameDelay();
        videoParamsRecordVideo = SHAFT.Properties.visuals.videoParamsRecordVideo();
        videoParamsScope = SHAFT.Properties.visuals.videoParamsScope();
        whenToTakePageSourceSnapshot = SHAFT.Properties.visuals.whenToTakePageSourceSnapshot();
    }

    @Test
    public void test() {
        SHAFT.Properties.visuals.set().visualMatchingThreshold(visualMatchingThreshold);
        SHAFT.Properties.visuals.set().screenshotParamsScalingFactor(screenshotParamsScalingFactor);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot(screenshotParamsWhenToTakeAScreenshot);
        SHAFT.Properties.visuals.set().screenshotParamsScreenshotType(screenshotParams_screenshotType);
        SHAFT.Properties.visuals.set().screenshotParamsHighlightElements(screenshotParamsHighlightElements);
        SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod(screenshotParamsHighlightMethod);
        SHAFT.Properties.visuals.set().screenshotParamsSkippedElementsFromScreenshot(screenshotParamsSkippedElementsFromScreenshot);
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(screenshotParamsWatermark);
        SHAFT.Properties.visuals.set().screenshotParamsWatermarkOpacity(screenshotParamsWatermarkOpacity);
        SHAFT.Properties.visuals.set().createAnimatedGif(createAnimatedGif);
        SHAFT.Properties.visuals.set().animatedGifFrameDelay(animatedGifFrameDelay);
        SHAFT.Properties.visuals.set().videoParamsRecordVideo(videoParamsRecordVideo);
        SHAFT.Properties.visuals.set().videoParamsScope(videoParamsScope);
        SHAFT.Properties.visuals.set().whenToTakePageSourceSnapshot(whenToTakePageSourceSnapshot);

    }

}
