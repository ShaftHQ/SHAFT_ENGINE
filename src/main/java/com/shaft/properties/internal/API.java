package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.ConfigFactory;

public interface API extends EngineProperties<API> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.apiOverride.set(ConfigFactory.create(API.class, ThreadLocalPropertiesManager.getOverrides()));
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("swagger.validation.enabled")
    @DefaultValue("false")
    boolean swaggerValidationEnabled();

    @Key("swagger.validation.url")
    @DefaultValue("")
    String swaggerValidationUrl();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty swaggerValidationEnabled(boolean value) {
            setProperty("swagger.validation.enabled", String.valueOf(value));
            return this;
        }

        public SetProperty swaggerValidationUrl(String value) {
            setProperty("swagger.validation.url", value);
            return this;
        }
    }
}

