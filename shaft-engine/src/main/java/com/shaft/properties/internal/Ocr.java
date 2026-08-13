package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/** Configuration for local OCR model provisioning. */
@Sources({"system:properties", "file:src/main/resources/properties/custom.properties",
        "file:src/main/resources/properties/default/custom.properties", "classpath:custom.properties"})
public interface Ocr extends EngineProperties<Ocr> {
    @Key("shaft.ocr.cacheDirectory")
    @DefaultValue("")
    String cacheDirectory();

    @Key("shaft.ocr.downloadEnabled")
    @DefaultValue("true")
    boolean downloadEnabled();

    @Key("shaft.ocr.document.renderDpi") @DefaultValue("300") int documentRenderDpi();
    @Key("shaft.ocr.document.maximumInputBytes") @DefaultValue("536870912") long documentMaximumInputBytes();
    @Key("shaft.ocr.document.maximumPages") @DefaultValue("1000") int documentMaximumPages();
    @Key("shaft.ocr.document.maximumPixelsPerPage") @DefaultValue("40000000") long documentMaximumPixelsPerPage();
    @Key("shaft.ocr.document.pageTimeoutSeconds") @DefaultValue("120") long documentPageTimeoutSeconds();
    @Key("shaft.ocr.document.maximumAllureArtifactBytes") @DefaultValue("26214400") long documentMaximumAllureArtifactBytes();
    @Key("shaft.ocr.document.batchParallelism") @DefaultValue("4") int documentBatchParallelism();
    @Key("shaft.ocr.document.maximumInFlightRasterBytes") @DefaultValue("268435456") long documentMaximumInFlightRasterBytes();

    @Override
    default OcrPropertyBuilder set() {
        return new OcrPropertyBuilder();
    }

    final class OcrPropertyBuilder implements EngineProperties.SetProperty {
        public OcrPropertyBuilder cacheDirectory(String value) {
            setProperty("shaft.ocr.cacheDirectory", value);
            return this;
        }

        public OcrPropertyBuilder downloadEnabled(boolean value) {
            setProperty("shaft.ocr.downloadEnabled", String.valueOf(value));
            return this;
        }

        public OcrPropertyBuilder documentRenderDpi(int value) { return set("shaft.ocr.document.renderDpi", value); }
        public OcrPropertyBuilder documentMaximumInputBytes(long value) { return set("shaft.ocr.document.maximumInputBytes", value); }
        public OcrPropertyBuilder documentMaximumPages(int value) { return set("shaft.ocr.document.maximumPages", value); }
        public OcrPropertyBuilder documentMaximumPixelsPerPage(long value) { return set("shaft.ocr.document.maximumPixelsPerPage", value); }
        public OcrPropertyBuilder documentPageTimeoutSeconds(long value) { return set("shaft.ocr.document.pageTimeoutSeconds", value); }
        public OcrPropertyBuilder documentMaximumAllureArtifactBytes(long value) { return set("shaft.ocr.document.maximumAllureArtifactBytes", value); }
        public OcrPropertyBuilder documentBatchParallelism(int value) { return set("shaft.ocr.document.batchParallelism", value); }
        public OcrPropertyBuilder documentMaximumInFlightRasterBytes(long value) { return set("shaft.ocr.document.maximumInFlightRasterBytes", value); }

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
