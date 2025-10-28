package com.shaft.properties.internal;

import static java.lang.System.setProperty;

public interface API extends EngineProperties<API> {
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

