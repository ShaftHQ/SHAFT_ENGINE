package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/VisualValidations.properties", "file:src/main/resources/properties/default/VisualValidations.properties", "classpath:VisualValidations.properties"})
public interface Visuals extends EngineProperties<Visuals> {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.visuals = ConfigFactory.create(Visuals.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("visualMatchingThreshold")
    @DefaultValue("0.90")
    double visualMatchingThreshold();

    @Key("screenshotParams_scalingFactor")
    @DefaultValue("1.0")
    double screenshotParamsScalingFactor();

    @Key("screenshotParams_whenToTakeAScreenshot")
    @DefaultValue("ValidationPointsOnly")
    String screenshotParamsWhenToTakeAScreenshot();

    @Key("screenshotParams_screenshotType")
    @DefaultValue("fullPage")
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
    @DefaultValue("Never")
    String whenToTakePageSourceSnapshot();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {

        public SetProperty visualMatchingThreshold(double value) {
            setProperty("visualMatchingThreshold", String.valueOf(value));
            return this;
        }

        public SetProperty screenshotParamsScalingFactor(double value) {
            setProperty("screenshotParams_scalingFactor", String.valueOf(value));
            return this;
        }

        public SetProperty screenshotParamsWhenToTakeAScreenshot(String value) {
            setProperty("screenshotParams_whenToTakeAScreenshot", value);
            return this;
        }

        public SetProperty screenshotParamsScreenshotType(String value) {
            setProperty("screenshotParams_screenshotType", value);
            return this;
        }

        public SetProperty screenshotParamsHighlightElements(boolean value) {
            setProperty("screenshotParams_highlightElements", String.valueOf(value));
            return this;
        }

        public SetProperty screenshotParamsHighlightMethod(String value) {
            setProperty("screenshotParams_highlightMethod", value);
            return this;
        }

        public SetProperty screenshotParamsSkippedElementsFromScreenshot(String value) {
            setProperty("screenshotParams_skippedElementsFromScreenshot", value);
            return this;
        }

        public SetProperty screenshotParamsWatermark(boolean value) {
            setProperty("screenshotParams_watermark", String.valueOf(value));
            return this;
        }

        public SetProperty screenshotParamsWatermarkOpacity(float value) {
            setProperty("screenshotParams_watermarkOpacity", String.valueOf(value));
            return this;
        }

        public SetProperty createAnimatedGif(boolean value) {
            setProperty("createAnimatedGif", String.valueOf(value));
            return this;
        }

        public SetProperty animatedGifFrameDelay(int value) {
            setProperty("animatedGif_frameDelay", String.valueOf(value));
            return this;
        }

        public SetProperty videoParamsRecordVideo(boolean value) {
            setProperty("videoParams_recordVideo", String.valueOf(value));
            return this;
        }

        public SetProperty videoParamsScope(String value) {
            setProperty("videoParams_scope", value);
            return this;
        }

        public SetProperty whenToTakePageSourceSnapshot(String value) {
            setProperty("whenToTakePageSourceSnapshot", value);
            return this;
        }

    }

}
