package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for API capture recording settings in the SHAFT framework.
 * Controls HTTP/WebSocket request and response capture behavior during test execution.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.capture.set().enabled(true).maxBodyBytes(2097152);
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/custom.properties",
        "file:src/main/resources/properties/default/custom.properties",
        "classpath:custom.properties"})
public interface Capture extends EngineProperties<Capture> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.captureOverride.set(ConfigFactory.create(
                Capture.class,
                ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    /** @return whether API capture recording is enabled */
    @Key("capture.api.enabled")
    @DefaultValue("false")
    boolean enabled();

    /** @return maximum request/response body size in bytes to capture */
    @Key("capture.api.maxBodyBytes")
    @DefaultValue("1048576")
    int maxBodyBytes();

    /** @return whether to capture static asset requests (images, stylesheets, scripts) */
    @Key("capture.api.includeAssets")
    @DefaultValue("false")
    boolean includeAssets();

    /** @return whether to capture only same-origin requests (first-party) */
    @Key("capture.api.firstPartyOnly")
    @DefaultValue("true")
    boolean firstPartyOnly();

    /** @return whether to store sensitive request/response data locally in reports */
    @Key("capture.api.storeSecretsLocally")
    @DefaultValue("false")
    boolean storeSecretsLocally();

    /** @return maximum number of network transactions to retain per capture session */
    @Key("capture.api.maxTransactions")
    @DefaultValue("500")
    int maxTransactions();

    /** @return comma-separated glob patterns; when non-empty, only matching URLs are captured */
    @Key("capture.api.urlIncludeGlobs")
    @DefaultValue("")
    String urlIncludeGlobs();

    /** @return comma-separated glob patterns; matching URLs are always excluded from capture */
    @Key("capture.api.urlExcludeGlobs")
    @DefaultValue("")
    String urlExcludeGlobs();

    /**
     * Returns a fluent builder for current-thread overrides.
     *
     * @return current-thread property builder
     */
    default SetProperty set() {
        return new SetProperty();
    }

    /**
     * Fluent current-thread overrides for API capture recording.
     */
    class SetProperty implements EngineProperties.SetProperty {
        /** @param value enabled state @return this builder */
        public SetProperty enabled(boolean value) {
            setProperty("capture.api.enabled", String.valueOf(value));
            return this;
        }

        /** @param value maximum body size in bytes @return this builder */
        public SetProperty maxBodyBytes(int value) {
            setProperty("capture.api.maxBodyBytes", String.valueOf(Math.max(0, value)));
            return this;
        }

        /** @param value whether to include asset requests @return this builder */
        public SetProperty includeAssets(boolean value) {
            setProperty("capture.api.includeAssets", String.valueOf(value));
            return this;
        }

        /** @param value whether to capture only same-origin requests @return this builder */
        public SetProperty firstPartyOnly(boolean value) {
            setProperty("capture.api.firstPartyOnly", String.valueOf(value));
            return this;
        }

        /** @param value whether to store secrets locally @return this builder */
        public SetProperty storeSecretsLocally(boolean value) {
            setProperty("capture.api.storeSecretsLocally", String.valueOf(value));
            return this;
        }

        /** @param value maximum number of network transactions to retain per session @return this builder */
        public SetProperty maxTransactions(int value) {
            setProperty("capture.api.maxTransactions", String.valueOf(Math.max(0, value)));
            return this;
        }

        /** @param value comma-separated URL include glob patterns @return this builder */
        public SetProperty urlIncludeGlobs(String value) {
            setProperty("capture.api.urlIncludeGlobs", value == null ? "" : value);
            return this;
        }

        /** @param value comma-separated URL exclude glob patterns @return this builder */
        public SetProperty urlExcludeGlobs(String value) {
            setProperty("capture.api.urlExcludeGlobs", value == null ? "" : value);
            return this;
        }
    }
}
