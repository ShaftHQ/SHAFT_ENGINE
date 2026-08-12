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

    @Override
    default SetProperty set() {
        return new SetProperty();
    }

    final class SetProperty implements EngineProperties.SetProperty {
        public SetProperty cacheDirectory(String value) {
            setProperty("shaft.ocr.cacheDirectory", value);
            return this;
        }

        public SetProperty downloadEnabled(boolean value) {
            setProperty("shaft.ocr.downloadEnabled", String.valueOf(value));
            return this;
        }

        private static void setProperty(String key, String value) {
            ThreadLocalPropertiesManager.setProperty(key, value);
            Properties.ocrOverride.set(ConfigFactory.create(Ocr.class, ThreadLocalPropertiesManager.getOverrides()));
            EngineProperties.logPropertyUpdate(key, value);
        }
    }
}
