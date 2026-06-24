package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for execution platform settings in the SHAFT framework.
 * Controls the target execution address, port, and operating-system configuration.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.platform.set().executionAddress("localhost:4444");
 * }</pre>
 */
@Sources({"system:properties",
        "file:src/main/resources/properties/ExecutionPlatform.properties",
        "file:src/main/resources/properties/default/ExecutionPlatform.properties",
        "classpath:ExecutionPlatform.properties",
})
public interface Platform extends EngineProperties<Platform> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.platformOverride.set(ConfigFactory.create(Platform.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    @Key("SHAFT.CrossBrowserMode")
    @DefaultValue("off")
    String crossBrowserMode();

    @Key("executionAddress")
    @DefaultValue("local")
    String executionAddress();

    @Key("targetOperatingSystem")
    @DefaultValue("Linux")
    String targetPlatform();

    @Key("com.SHAFT.proxySettings")
    @DefaultValue("")
    String proxy();

    @Key("driverProxySettings")
    @DefaultValue("true")
    boolean driverProxySettings();

    @Key("jvmProxySettings")
    @DefaultValue("true")
    boolean jvmProxySettings();

    @Key("enableBiDi")
    @DefaultValue("true")
    boolean enableBiDi();

    /**
     * Enables Selenium Grid status and GraphQL preflight checks before remote session creation.
     *
     * @return {@code true} when remote Grid preflight checks are enabled
     */
    @Key("remotePreflightEnabled")
    @DefaultValue("false")
    boolean remotePreflightEnabled();

    /**
     * Enables per-endpoint session throttling based on detected Selenium Grid slot capacity.
     *
     * @return {@code true} when SHAFT should throttle local remote-session creation
     */
    @Key("remoteAdaptiveSessionThrottling")
    @DefaultValue("false")
    boolean remoteAdaptiveSessionThrottling();

    /**
     * Fails remote session startup when Grid preflight proves the requested capabilities cannot run.
     *
     * @return {@code true} when incompatible Grid capacity should fail before session creation
     */
    @Key("remotePreflightFailFast")
    @DefaultValue("false")
    boolean remotePreflightFailFast();

    /**
     * Timeout, in seconds, for each Selenium Grid preflight endpoint call.
     *
     * @return preflight HTTP timeout in seconds
     */
    @Key("remotePreflightTimeoutSeconds")
    @DefaultValue("5")
    int remotePreflightTimeoutSeconds();

    default SetProperty set() {
        return new SetProperty();
    }

    @SuppressWarnings("unused")
    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty crossBrowserMode(String value) {
            setProperty("SHAFT.CrossBrowserMode", value);
            return this;
        }

        public SetProperty executionAddress(String value) {
            setProperty("executionAddress", value);
            return this;
        }

        /**
         * @param value io.github.shafthq.shaft.enums.OperatingSystems
         */
        public SetProperty targetPlatform(String value) {
            setProperty("targetOperatingSystem", value);
            return this;
        }

        public SetProperty proxySettings(String value) {
            setProperty("com.SHAFT.proxySettings", value);
            return this;
        }

        public SetProperty driverProxySettings(boolean value) {
            setProperty("driverProxySettings", String.valueOf(value));
            return this;
        }

        public SetProperty jvmProxySettings(boolean value) {
            setProperty("jvmProxySettings", String.valueOf(value));
            return this;
        }

        public SetProperty enableBiDi(boolean value) {
            setProperty("enableBiDi", String.valueOf(value));
            return this;
        }

        public SetProperty remotePreflightEnabled(boolean value) {
            setProperty("remotePreflightEnabled", String.valueOf(value));
            return this;
        }

        public SetProperty remoteAdaptiveSessionThrottling(boolean value) {
            setProperty("remoteAdaptiveSessionThrottling", String.valueOf(value));
            return this;
        }

        public SetProperty remotePreflightFailFast(boolean value) {
            setProperty("remotePreflightFailFast", String.valueOf(value));
            return this;
        }

        public SetProperty remotePreflightTimeoutSeconds(int value) {
            setProperty("remotePreflightTimeoutSeconds", String.valueOf(value));
            return this;
        }
    }
}
