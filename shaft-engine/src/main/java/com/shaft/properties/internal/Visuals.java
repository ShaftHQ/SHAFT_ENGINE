package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for visual testing settings in the SHAFT framework.
 * Controls screenshot capture, GIF recording, video recording, and visual comparison thresholds.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("Always");
 * }</pre>
 *
 * <p>Set {@code -Dshaft.updateSnapshots=true} to regenerate visual and aria-snapshot baselines instead of
 * comparing against the existing ones.</p>
 */
@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/VisualValidations.properties", "file:src/main/resources/properties/default/VisualValidations.properties", "classpath:VisualValidations.properties"})
public interface Visuals extends EngineProperties<Visuals> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.visualsOverride.set(ConfigFactory.create(Visuals.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * Visual matching threshold for AI powered element identification.
     *
     * <p>Default: {@code 0.90}. Possible values: any decimal value between 0.00 and 1.00.
     *
     * @return the configured value of {@code visualMatchingThreshold}
     */
    @Key("visualMatchingThreshold")
    @DefaultValue("0.90")
    double visualMatchingThreshold();

    /**
     * Scaling factor for screenshots.
     *
     * <p>Default: {@code 1.0}. Possible values: example: 1.0, 0.5, 2.0.
     *
     * @return the configured value of {@code screenshotParams_scalingFactor}
     */
    @Key("screenshotParams_scalingFactor")
    @DefaultValue("1.0")
    double screenshotParamsScalingFactor();

    /**
     * Granular screenshot policy. Profile levels except CUSTOM override this after property loading.
     *
     * <p>Default: {@code ValidationPointsOnly}. Possible values: Always, ValidationPointsOnly,
     * FailuresOnly, Never.
     *
     * @return the configured value of {@code screenshotParams_whenToTakeAScreenshot}
     */
    @Key("screenshotParams_whenToTakeAScreenshot")
    @DefaultValue("ValidationPointsOnly")
    String screenshotParamsWhenToTakeAScreenshot();

    /**
     * Type of screenshot to capture.
     *
     * <p>Default: {@code fullPage}. Possible values: fullPage, regular, element.
     *
     * @return the configured value of {@code screenshotParams_screenshotType}
     */
    @Key("screenshotParams_screenshotType")
    @DefaultValue("fullPage")
    String screenshotParamsScreenshotType();

    /**
     * Highlight elements in screenshots.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code screenshotParams_highlightElements}
     */
    @Key("screenshotParams_highlightElements")
    @DefaultValue("true")
    boolean screenshotParamsHighlightElements();

    /**
     * Method for highlighting elements in screenshots.
     *
     * <p>Default: {@code AI}. Possible values: AI, NATIVE.
     *
     * @return the configured value of {@code screenshotParams_highlightMethod}
     */
    @Key("screenshotParams_highlightMethod")
    @DefaultValue("AI")
    String screenshotParamsHighlightMethod();

    /**
     * Elements to skip/hide when taking screenshots.
     *
     * <p>Possible values: CSS selectors separated by commas.
     *
     * @return the configured value of {@code screenshotParams_skippedElementsFromScreenshot}
     */
    @Key("screenshotParams_skippedElementsFromScreenshot")
    @DefaultValue("")
    String screenshotParamsSkippedElementsFromScreenshot();

    /**
     * Add watermark to screenshots.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code screenshotParams_watermark}
     */
    @Key("screenshotParams_watermark")
    @DefaultValue("true")
    boolean screenshotParamsWatermark();

    /**
     * Opacity level for screenshot watermark.
     *
     * <p>Default: {@code 0.2}. Possible values: 0.0 to 1.0.
     *
     * @return the configured value of {@code screenshotParams_watermarkOpacity}
     */
    @Key("screenshotParams_watermarkOpacity")
    @DefaultValue("0.2")
    float screenshotParamsWatermarkOpacity();

    /**
     * Granular GIF policy. Profile levels except CUSTOM override this after property loading; retry
     * attempts can enable GIFs automatically.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code createAnimatedGif}
     */
    @Key("createAnimatedGif")
    @DefaultValue("false")
    boolean createAnimatedGif();

    /**
     * Delay between frames in animated GIF.
     *
     * <p>Default: {@code 500}. Possible values: milliseconds.
     *
     * @return the configured value of {@code animatedGif_frameDelay}
     */
    @Key("animatedGif_frameDelay")
    @DefaultValue("500")
    int animatedGifFrameDelay();

    /**
     * Granular video policy. Profile levels except CUSTOM override this after property loading.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code videoParams_recordVideo}
     */
    @Key("videoParams_recordVideo")
    @DefaultValue("false")
    boolean videoParamsRecordVideo();

    /**
     * Scope for video recording.
     *
     * <p>Default: {@code DriverSession}. Possible values: DriverSession, TestMethod.
     *
     * @return the configured value of {@code videoParams_scope}
     */
    @Key("videoParams_scope")
    @DefaultValue("DriverSession")
    String videoParamsScope();

    // Default OFF (issue reported 2026-07-18): assertion/validation evidence should be a screenshot
    // and nothing else by default. Page-source/HTML snapshots are noisy and only opt-in via this
    // property (or a richer evidenceLevel profile such as BALANCED/FULL).
    /**
     * Granular page-source policy. Profile levels except CUSTOM override this after property loading.
     *
     * <p>Default: {@code Never}. Possible values: Never, Always, FailuresOnly.
     *
     * @return the configured value of {@code whenToTakePageSourceSnapshot}
     */
    @Key("whenToTakePageSourceSnapshot")
    @DefaultValue("Never")
    String whenToTakePageSourceSnapshot();

    /**
     * When true, regenerates visual and ARIA-snapshot baselines instead of comparing new captures
     * against the existing ones.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.updateSnapshots}
     */
    @Key("shaft.updateSnapshots")
    @DefaultValue("false")
    boolean updateSnapshots();

    /**
     * Starts a fluent, thread-local override of these properties for the current test thread.
     *
     * @return a new {@link SetProperty} builder
     */
    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {

        /**
         * Overrides the {@code visualMatchingThreshold} property at runtime. Visual matching threshold for
         * AI powered element identification.
         *
         * @param value the new value of {@code visualMatchingThreshold}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty visualMatchingThreshold(double value) {
            setProperty("visualMatchingThreshold", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code screenshotParams_scalingFactor} property at runtime. Scaling factor for
         * screenshots.
         *
         * @param value the new value of {@code screenshotParams_scalingFactor}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty screenshotParamsScalingFactor(double value) {
            setProperty("screenshotParams_scalingFactor", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code screenshotParams_whenToTakeAScreenshot} property at runtime. Granular
         * screenshot policy.
         *
         * @param value the new value of {@code screenshotParams_whenToTakeAScreenshot}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty screenshotParamsWhenToTakeAScreenshot(String value) {
            setProperty("screenshotParams_whenToTakeAScreenshot", value);
            return this;
        }

        /**
         * Overrides the {@code screenshotParams_screenshotType} property at runtime. Type of screenshot to
         * capture.
         *
         * @param value the new value of {@code screenshotParams_screenshotType}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty screenshotParamsScreenshotType(String value) {
            setProperty("screenshotParams_screenshotType", value);
            return this;
        }

        /**
         * Overrides the {@code screenshotParams_highlightElements} property at runtime. Highlight elements
         * in screenshots.
         *
         * @param value the new value of {@code screenshotParams_highlightElements}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty screenshotParamsHighlightElements(boolean value) {
            setProperty("screenshotParams_highlightElements", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code screenshotParams_highlightMethod} property at runtime. Method for
         * highlighting elements in screenshots.
         *
         * @param value the new value of {@code screenshotParams_highlightMethod}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty screenshotParamsHighlightMethod(String value) {
            setProperty("screenshotParams_highlightMethod", value);
            return this;
        }

        /**
         * Overrides the {@code screenshotParams_skippedElementsFromScreenshot} property at runtime.
         * Elements to skip/hide when taking screenshots.
         *
         * @param value the new value of {@code screenshotParams_skippedElementsFromScreenshot}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty screenshotParamsSkippedElementsFromScreenshot(String value) {
            setProperty("screenshotParams_skippedElementsFromScreenshot", value);
            return this;
        }

        /**
         * Overrides the {@code screenshotParams_watermark} property at runtime. Add watermark to
         * screenshots.
         *
         * @param value the new value of {@code screenshotParams_watermark}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty screenshotParamsWatermark(boolean value) {
            setProperty("screenshotParams_watermark", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code screenshotParams_watermarkOpacity} property at runtime. Opacity level for
         * screenshot watermark.
         *
         * @param value the new value of {@code screenshotParams_watermarkOpacity}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty screenshotParamsWatermarkOpacity(float value) {
            setProperty("screenshotParams_watermarkOpacity", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code createAnimatedGif} property at runtime. Granular GIF policy.
         *
         * @param value the new value of {@code createAnimatedGif}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty createAnimatedGif(boolean value) {
            setProperty("createAnimatedGif", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code animatedGif_frameDelay} property at runtime. Delay between frames in
         * animated GIF.
         *
         * @param value the new value of {@code animatedGif_frameDelay}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty animatedGifFrameDelay(int value) {
            setProperty("animatedGif_frameDelay", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code videoParams_recordVideo} property at runtime. Granular video policy.
         *
         * @param value the new value of {@code videoParams_recordVideo}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty videoParamsRecordVideo(boolean value) {
            setProperty("videoParams_recordVideo", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code videoParams_scope} property at runtime. Scope for video recording.
         *
         * @param value the new value of {@code videoParams_scope}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty videoParamsScope(String value) {
            setProperty("videoParams_scope", value);
            return this;
        }

        /**
         * Overrides the {@code whenToTakePageSourceSnapshot} property at runtime. Granular page-source
         * policy.
         *
         * @param value the new value of {@code whenToTakePageSourceSnapshot}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty whenToTakePageSourceSnapshot(String value) {
            setProperty("whenToTakePageSourceSnapshot", value);
            return this;
        }

        /**
         * Overrides the {@code shaft.updateSnapshots} property at runtime. When true, regenerates visual
         * and ARIA-snapshot baselines instead of comparing new captures against the existing ones.
         *
         * @param value the new value of {@code shaft.updateSnapshots}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty updateSnapshots(boolean value) {
            setProperty("shaft.updateSnapshots", String.valueOf(value));
            return this;
        }

    }

}
