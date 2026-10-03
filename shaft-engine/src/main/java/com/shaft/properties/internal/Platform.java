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

    /**
     * Cross Browser Mode allows SHAFT to run your test class against Chrome, Firefox, and Safari! You
     * need to have 'Docker Desktop' installed on your machine, and configured to use Linux images. Off
     * → Your tests will run normally and respect your configuration. Sequential → Your tests will run
     * on Chrome, Firefox, and Safari in sequence. Parallelized → Your tests will run on Chrome,
     * Firefox and Safari in parallel. And for each browser they will run in sequence.
     *
     * <p>Default: {@code off}. Possible values: off, sequential, parallelized.
     *
     * @return the configured value of {@code SHAFT.CrossBrowserMode}
     */
    @Key("SHAFT.CrossBrowserMode")
    @DefaultValue("off")
    String crossBrowserMode();

    /**
     * For Appium, set the below settings and move to the Mobile tab to continue. For BrowserStack, set
     * the "Target Operating System" below, and the "Automation Name" in the Mobile tab, then configure
     * the "browserStack.properties" file in your project directory.
     *
     * <p>Default: {@code local}. Possible values: local, dockerized, testcontainers, browserstack, host:port,
     * http://host:port/wd/hub.
     *
     * @return the configured value of {@code executionAddress}
     */
    @Key("executionAddress")
    @DefaultValue("local")
    String executionAddress();

    /**
     * The target operating system for test execution.
     *
     * <p>Default: {@code Linux}. Possible values: Linux, Windows, Mac, Android, iOS.
     *
     * @return the configured value of {@code targetOperatingSystem}
     */
    @Key("targetOperatingSystem")
    @DefaultValue("Linux")
    String targetPlatform();

    /**
     * Used to configure testing behind a proxy. e.g. corporate proxy.
     *
     * <p>Possible values: host:port.
     *
     * @return the configured value of {@code com.SHAFT.proxySettings}
     */
    @Key("com.SHAFT.proxySettings")
    @DefaultValue("")
    String proxy();

    /**
     * To enable or disable the driver proxy.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code driverProxySettings}
     */
    @Key("driverProxySettings")
    @DefaultValue("true")
    boolean driverProxySettings();

    /**
     * To enable or disable the JVM proxy.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code jvmProxySettings}
     */
    @Key("jvmProxySettings")
    @DefaultValue("true")
    boolean jvmProxySettings();

    /**
     * To enable or disable the WebDriver BiDi protocol.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code enableBiDi}
     */
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

    /**
     * Starts a fluent, thread-local override of these properties for the current test thread.
     *
     * @return a new {@link SetProperty} builder
     */
    default SetProperty set() {
        return new SetProperty();
    }

    @SuppressWarnings("unused")
    class SetProperty implements EngineProperties.SetProperty {
        /**
         * Overrides the {@code SHAFT.CrossBrowserMode} property at runtime. Cross Browser Mode allows
         * SHAFT to run your test class against Chrome, Firefox, and Safari!
         *
         * @param value the new value of {@code SHAFT.CrossBrowserMode}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty crossBrowserMode(String value) {
            setProperty("SHAFT.CrossBrowserMode", value);
            return this;
        }

        /**
         * Overrides the {@code executionAddress} property at runtime. For Appium, set the below settings
         * and move to the Mobile tab to continue.
         *
         * @param value the new value of {@code executionAddress}
         * @return this {@link SetProperty} instance for chaining
         */
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

        /**
         * Overrides the {@code com.SHAFT.proxySettings} property at runtime. Used to configure testing
         * behind a proxy.
         *
         * @param value the new value of {@code com.SHAFT.proxySettings}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty proxySettings(String value) {
            setProperty("com.SHAFT.proxySettings", value);
            return this;
        }

        /**
         * Overrides the {@code driverProxySettings} property at runtime. To enable or disable the driver
         * proxy.
         *
         * @param value the new value of {@code driverProxySettings}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty driverProxySettings(boolean value) {
            setProperty("driverProxySettings", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code jvmProxySettings} property at runtime. To enable or disable the JVM proxy.
         *
         * @param value the new value of {@code jvmProxySettings}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty jvmProxySettings(boolean value) {
            setProperty("jvmProxySettings", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code enableBiDi} property at runtime. To enable or disable the WebDriver BiDi
         * protocol.
         *
         * @param value the new value of {@code enableBiDi}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty enableBiDi(boolean value) {
            setProperty("enableBiDi", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code remotePreflightEnabled} property at runtime. Query Selenium Grid /status
         * and /graphql before remote sessions and attach a preflight summary when Grid metadata is
         * available.
         *
         * @param value the new value of {@code remotePreflightEnabled}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty remotePreflightEnabled(boolean value) {
            setProperty("remotePreflightEnabled", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code remoteAdaptiveSessionThrottling} property at runtime. Limit local remote-
         * session creation to the detected matching Selenium Grid slot count.
         *
         * @param value the new value of {@code remoteAdaptiveSessionThrottling}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty remoteAdaptiveSessionThrottling(boolean value) {
            setProperty("remoteAdaptiveSessionThrottling", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code remotePreflightFailFast} property at runtime. Fail before WebDriver retries
         * when Grid preflight proves the requested browser slot is unavailable.
         *
         * @param value the new value of {@code remotePreflightFailFast}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty remotePreflightFailFast(boolean value) {
            setProperty("remotePreflightFailFast", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code remotePreflightTimeoutSeconds} property at runtime. Timeout for each Grid
         * preflight /status or /graphql call.
         *
         * @param value the new value of {@code remotePreflightTimeoutSeconds}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty remotePreflightTimeoutSeconds(int value) {
            setProperty("remotePreflightTimeoutSeconds", String.valueOf(value));
            return this;
        }
    }
}
