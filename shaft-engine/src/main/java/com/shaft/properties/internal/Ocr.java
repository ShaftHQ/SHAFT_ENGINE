package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/** Configuration for local OCR model provisioning. */
@Sources({"system:properties", "file:src/main/resources/properties/custom.properties",
        "file:src/main/resources/properties/default/custom.properties", "classpath:custom.properties"})
public interface Ocr extends EngineProperties<Ocr> {
    /**
     * Optional OCR runtime cache override. Empty uses the platform default.
     *
     * <p>Possible values: absolute path or empty.
     *
     * @return the configured value of {@code shaft.ocr.cacheDirectory}
     */
    @Key("shaft.ocr.cacheDirectory")
    @DefaultValue("")
    String cacheDirectory();

    /**
     * Allows a missing verified OCR runtime to be downloaded.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code shaft.ocr.downloadEnabled}
     */
    @Key("shaft.ocr.downloadEnabled")
    @DefaultValue("true")
    boolean downloadEnabled();

    /**
     * Resolution used to rasterize document pages (for example PDF) before OCR.
     *
     * <p>Default: {@code 300}. Possible values: Integer DPI.
     *
     * @return the configured value of {@code shaft.ocr.document.renderDpi}
     */
    @Key("shaft.ocr.document.renderDpi")
    @DefaultValue("300")
    int documentRenderDpi();

    /**
     * Largest document file accepted for OCR (512 MB).
     *
     * <p>Default: {@code 536870912}. Possible values: Integer bytes.
     *
     * @return the configured value of {@code shaft.ocr.document.maximumInputBytes}
     */
    @Key("shaft.ocr.document.maximumInputBytes")
    @DefaultValue("536870912")
    long documentMaximumInputBytes();

    /**
     * Maximum number of pages processed per document.
     *
     * <p>Default: {@code 1000}. Possible values: Integer.
     *
     * @return the configured value of {@code shaft.ocr.document.maximumPages}
     */
    @Key("shaft.ocr.document.maximumPages")
    @DefaultValue("1000")
    int documentMaximumPages();

    /**
     * Maximum rasterized pixels per page; larger pages are rejected.
     *
     * <p>Default: {@code 40000000}. Possible values: Integer pixels.
     *
     * @return the configured value of {@code shaft.ocr.document.maximumPixelsPerPage}
     */
    @Key("shaft.ocr.document.maximumPixelsPerPage")
    @DefaultValue("40000000")
    long documentMaximumPixelsPerPage();

    /**
     * Time limit for OCR of a single page.
     *
     * <p>Default: {@code 120}. Possible values: Integer seconds.
     *
     * @return the configured value of {@code shaft.ocr.document.pageTimeoutSeconds}
     */
    @Key("shaft.ocr.document.pageTimeoutSeconds")
    @DefaultValue("120")
    long documentPageTimeoutSeconds();

    /**
     * Largest OCR artifact attached to Allure (25 MB).
     *
     * <p>Default: {@code 26214400}. Possible values: Integer bytes.
     *
     * @return the configured value of {@code shaft.ocr.document.maximumAllureArtifactBytes}
     */
    @Key("shaft.ocr.document.maximumAllureArtifactBytes")
    @DefaultValue("26214400")
    long documentMaximumAllureArtifactBytes();

    /**
     * Number of pages processed in parallel.
     *
     * <p>Default: {@code 4}. Possible values: Integer.
     *
     * @return the configured value of {@code shaft.ocr.document.batchParallelism}
     */
    @Key("shaft.ocr.document.batchParallelism")
    @DefaultValue("4")
    int documentBatchParallelism();

    /**
     * Memory cap for rasterized pages held at once (256 MB).
     *
     * <p>Default: {@code 268435456}. Possible values: Integer bytes.
     *
     * @return the configured value of {@code shaft.ocr.document.maximumInFlightRasterBytes}
     */
    @Key("shaft.ocr.document.maximumInFlightRasterBytes")
    @DefaultValue("268435456")
    long documentMaximumInFlightRasterBytes();

    @Override
    default OcrPropertyBuilder set() {
        return new OcrPropertyBuilder();
    }

    final class OcrPropertyBuilder implements EngineProperties.SetProperty {
        /**
         * Overrides the {@code shaft.ocr.cacheDirectory} property at runtime. Optional OCR runtime cache
         * override.
         *
         * @param value the new value of {@code shaft.ocr.cacheDirectory}
         * @return this {@link OcrPropertyBuilder} instance for chaining
         */
        public OcrPropertyBuilder cacheDirectory(String value) {
            setProperty("shaft.ocr.cacheDirectory", value);
            return this;
        }

        /**
         * Overrides the {@code shaft.ocr.downloadEnabled} property at runtime. Allows a missing verified
         * OCR runtime to be downloaded.
         *
         * @param value the new value of {@code shaft.ocr.downloadEnabled}
         * @return this {@link OcrPropertyBuilder} instance for chaining
         */
        public OcrPropertyBuilder downloadEnabled(boolean value) {
            setProperty("shaft.ocr.downloadEnabled", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code shaft.ocr.document.renderDpi} property at runtime. Resolution used to
         * rasterize document pages (for example PDF) before OCR.
         *
         * @param value the new value of {@code shaft.ocr.document.renderDpi}
         * @return this {@link OcrPropertyBuilder} instance for chaining
         */
        public OcrPropertyBuilder documentRenderDpi(int value) {
            return set("shaft.ocr.document.renderDpi", value);
        }

        /**
         * Overrides the {@code shaft.ocr.document.maximumInputBytes} property at runtime. Largest document
         * file accepted for OCR (512 MB).
         *
         * @param value the new value of {@code shaft.ocr.document.maximumInputBytes}
         * @return this {@link OcrPropertyBuilder} instance for chaining
         */
        public OcrPropertyBuilder documentMaximumInputBytes(long value) {
            return set("shaft.ocr.document.maximumInputBytes", value);
        }

        /**
         * Overrides the {@code shaft.ocr.document.maximumPages} property at runtime. Maximum number of
         * pages processed per document.
         *
         * @param value the new value of {@code shaft.ocr.document.maximumPages}
         * @return this {@link OcrPropertyBuilder} instance for chaining
         */
        public OcrPropertyBuilder documentMaximumPages(int value) {
            return set("shaft.ocr.document.maximumPages", value);
        }

        /**
         * Overrides the {@code shaft.ocr.document.maximumPixelsPerPage} property at runtime. Maximum
         * rasterized pixels per page; larger pages are rejected.
         *
         * @param value the new value of {@code shaft.ocr.document.maximumPixelsPerPage}
         * @return this {@link OcrPropertyBuilder} instance for chaining
         */
        public OcrPropertyBuilder documentMaximumPixelsPerPage(long value) {
            return set("shaft.ocr.document.maximumPixelsPerPage", value);
        }

        /**
         * Overrides the {@code shaft.ocr.document.pageTimeoutSeconds} property at runtime. Time limit for
         * OCR of a single page.
         *
         * @param value the new value of {@code shaft.ocr.document.pageTimeoutSeconds}
         * @return this {@link OcrPropertyBuilder} instance for chaining
         */
        public OcrPropertyBuilder documentPageTimeoutSeconds(long value) {
            return set("shaft.ocr.document.pageTimeoutSeconds", value);
        }

        /**
         * Overrides the {@code shaft.ocr.document.maximumAllureArtifactBytes} property at runtime. Largest
         * OCR artifact attached to Allure (25 MB).
         *
         * @param value the new value of {@code shaft.ocr.document.maximumAllureArtifactBytes}
         * @return this {@link OcrPropertyBuilder} instance for chaining
         */
        public OcrPropertyBuilder documentMaximumAllureArtifactBytes(long value) {
            return set("shaft.ocr.document.maximumAllureArtifactBytes", value);
        }

        /**
         * Overrides the {@code shaft.ocr.document.batchParallelism} property at runtime. Number of pages
         * processed in parallel.
         *
         * @param value the new value of {@code shaft.ocr.document.batchParallelism}
         * @return this {@link OcrPropertyBuilder} instance for chaining
         */
        public OcrPropertyBuilder documentBatchParallelism(int value) {
            return set("shaft.ocr.document.batchParallelism", value);
        }

        /**
         * Overrides the {@code shaft.ocr.document.maximumInFlightRasterBytes} property at runtime. Memory
         * cap for rasterized pages held at once (256 MB).
         *
         * @param value the new value of {@code shaft.ocr.document.maximumInFlightRasterBytes}
         * @return this {@link OcrPropertyBuilder} instance for chaining
         */
        public OcrPropertyBuilder documentMaximumInFlightRasterBytes(long value) {
            return set("shaft.ocr.document.maximumInFlightRasterBytes", value);
        }

        private OcrPropertyBuilder set(String key, Number value) {
            setProperty(key, String.valueOf(value));
            return this;
        }

        private static void setProperty(String key, String value) {
            ThreadLocalPropertiesManager.setProperty(key, value);
            Properties.ocrOverride.set(ConfigFactory.create(Ocr.class, ThreadLocalPropertiesManager.getOverrides()));
            EngineProperties.logPropertyUpdate(key, value);
        }
    }
}
