package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/VisualValidations.properties", "file:src/main/resources/properties/default/VisualValidations.properties", "classpath:VisualValidations.properties"})
public interface Visuals extends EngineProperties {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.visuals = ConfigFactory.create(Visuals.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }


    @Key("screenshotParams_scalingFactor")
    @DefaultValue("1")
    String screenshotParamsScalingFactor();

    @Key("screenshotParams_whenToTakeAScreenshot")
    @DefaultValue("ValidationPointsOnly")
    String screenshotParamsWhenToTakeAScreenshot();

    @Key("screenshotParams_screenshotType")
    @DefaultValue("FullPage")
    String screenshotParamsScreenshotType();

    @Key("screenshotParams_highlightElements")
    @DefaultValue("true")
    boolean screenshotParamsHighlightElements();

    @Key("screenshotParams_highlightMethod")
    @DefaultValue("AI")
    String screenshotParamsHighlightMethod();

    @Key("screenshotParams_skippedElementsFromScreenshot")
    @DefaultValue("")
    String screenshotParamsSkippedElementsFromScreenshot();

    @Key("screenshotParams_watermark")
    @DefaultValue("true")
    boolean screenshotParamsWatermark();

    @Key("screenshotParams_watermarkOpacity")
    @DefaultValue("0.2")
    float screenshotParamsWatermarkOpacity();

    @Key("createAnimatedGif")
    @DefaultValue("false")
    boolean createAnimatedGif();

    @Key("animatedGif_frameDelay")
    @DefaultValue("500")
    int animatedGifFrameDelay();

    @Key("videoParams_recordVideo")
    @DefaultValue("false")
    boolean videoParamsRecordVideo();

    @Key("videoParams_scope")
    @DefaultValue("DriverSession")
    String videoParamsScope();

    @Key("whenToTakePageSourceSnapshot")
    @DefaultValue("FailuresOnly")
    String whenToTakePageSourceSnapshot();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void screenshotParamsScalingFactor(String value) {
            setProperty("screenshotParams_scalingFactor", value);
        }

        public void screenshotParamsWhenToTakeAScreenshot(String value) {
            setProperty("screenshotParams_whenToTakeAScreenshot", value);
        }

        public void screenshotParamsScreenshotType(String value) {
            setProperty("screenshotParams_screenshotType", value);
        }

        public void screenshotParamsHighlightElements(boolean value) {
            setProperty("screenshotParams_highlightElements", String.valueOf(value));
        }

        public void screenshotParamsHighlightMethod(String value) {
            setProperty("screenshotParams_highlightMethod", value);
        }

        public void screenshotParamsSkippedElementsFromScreenshot(String value) {
            setProperty("screenshotParams_skippedElementsFromScreenshot", value);
        }

        public void screenshotParamsWatermark(boolean value) {
            setProperty("screenshotParams_watermark", String.valueOf(value));
        }

        public void screenshotParamsWatermarkOpacity(float value) {
            setProperty("screenshotParams_watermarkOpacity", String.valueOf(value));
        }

        public void createAnimatedGif(boolean value) {
            setProperty("createAnimatedGif", String.valueOf(value));
        }

        public void animatedGifFrameDelay(int value) {
            setProperty("animatedGif_frameDelay", String.valueOf(value));
        }

        public void videoParamsRecordVideo(boolean value) {
            setProperty("videoParams_recordVideo", String.valueOf(value));
        }

        public void videoParamsScope(String value) {
            setProperty("videoParams_scope", value);
        }

        public void whenToTakePageSourceSnapshot(String value) {
            setProperty("whenToTakePageSourceSnapshot", value);
        }

    }

}
