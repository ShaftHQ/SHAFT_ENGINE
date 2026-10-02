package com.shaft.properties.internal;

import org.aeonbits.owner.Config;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for LambdaTest cloud testing in the SHAFT framework.
 * Controls credentials, OS/browser versions, and platform capabilities for remote execution.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.lambdaTest.set().username("user").accessKey("key");
 * }</pre>
 */
@SuppressWarnings({"unused"})
@Config.Sources({"system:properties", "file:src/main/resources/properties/LambdaTest.properties", "file:src/main/resources/properties/default/LambdaTest.properties", "classpath:LambdaTest.properties",})
public interface LambdaTest extends EngineProperties<LambdaTest> {
    //Based on LambdaTest capability builder
    // For Web and Mobile Native https://www.lambdatest.com/capabilities-generator/

    //In case of Mobile Native testing:
    //You must set the "targetOperatingSystem" property under "ExecutionPlatform.properties" or programmatically
    //You must set the "mobile_automationName" property under "MobileCapabilities.properties" or programmatically

    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.lambdaTestOverride.set(ConfigFactory.create(LambdaTest.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    //Below properties are all required
    /**
     * LambdaTest username for authentication.
     *
     * @return the configured value of {@code LambdaTest.username}
     */
    @Key("LambdaTest.username")
    @DefaultValue("")
    String username();

    /**
     * LambdaTest access key for authentication.
     *
     * @return the configured value of {@code LambdaTest.accessKey}
     */
    @Key("LambdaTest.accessKey")
    @DefaultValue("")
    String accessKey();

    //Below properties are needed for native mobile app testing:
    //Required
    /**
     * Mobile platform version for LambdaTest execution.
     *
     * @return the configured value of {@code LambdaTest.platformVersion}
     */
    @Key("LambdaTest.platformVersion")
    @DefaultValue("")
    String platformVersion();

    /**
     * Mobile device name for LambdaTest execution.
     *
     * @return the configured value of {@code LambdaTest.deviceName}
     */
    @Key("LambdaTest.deviceName")
    @DefaultValue("")
    String deviceName();

    //Use appUrl to test a previously uploaded app file
    /**
     * Use appUrl to test a previously uploaded app file.
     *
     * @return the configured value of {@code LambdaTest.appUrl}
     */
    @Key("LambdaTest.appUrl")
    @DefaultValue("")
    String appUrl();

    /**
     * Enable app profiling during mobile testing.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.appProfiling}
     */
    @Key("LambdaTest.appProfiling")
    @DefaultValue("false")
    boolean appProfiling();

    /**
     * OS version for desktop browser testing on LambdaTest.
     *
     * @return the configured value of {@code LambdaTest.osVersion}
     */
    @Key("LambdaTest.osVersion")
    @DefaultValue("")
    String osVersion();

    /**
     * Enable visual logs (screenshots) on LambdaTest.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.visual}
     */
    @Key("LambdaTest.visual")
    @DefaultValue("false")
    boolean visual();

    /**
     * Enable video recording on LambdaTest.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.video}
     */
    @Key("LambdaTest.video")
    @DefaultValue("false")
    boolean video();

    //Use appName and appRelativeFilePath to upload a new app file and test it
    /**
     * Use appName and appRelativeFilePath to upload a new app file and test it.
     *
     * @return the configured value of {@code LambdaTest.appName}
     */
    @Key("LambdaTest.appName")
    @DefaultValue("")
    String appName();

    /**
     * Use appName and appRelativeFilePath to upload a new app file and test it.
     *
     * @return the configured value of {@code LambdaTest.appRelativeFilePath}
     */
    @Key("LambdaTest.appRelativeFilePath")
    @DefaultValue("")
    String appRelativeFilePath();

    /**
     * Screen resolution for desktop browser testing (e.g. 1920x1080).
     *
     * @return the configured value of {@code LambdaTest.resolution}
     */
    @Key("LambdaTest.resolution")
    @DefaultValue("")
    String resolution();

    /**
     * Enable headless browser execution on LambdaTest.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.headless}
     */
    @Key("LambdaTest.headless")
    @DefaultValue("false")
    boolean headless();

    /**
     * Timezone for test execution on LambdaTest (e.g. UTC+5:30).
     *
     * @return the configured value of {@code LambdaTest.timezone}
     */
    @Key("LambdaTest.timezone")
    @DefaultValue("")
    String timezone();

    /**
     * Project name to group builds in the LambdaTest dashboard.
     *
     * <p>Default: {@code shaft-engine}.
     *
     * @return the configured value of {@code LambdaTest.project}
     */
    @Key("LambdaTest.project")
    @DefaultValue("shaft-engine")
    String project();

    /**
     * Build name to group test runs in the LambdaTest dashboard.
     *
     * <p>Default: {@code Build Name}.
     *
     * @return the configured value of {@code LambdaTest.build}
     */
    @Key("LambdaTest.build")
    @DefaultValue("Build Name")
    String build();

    /**
     * Enable LambdaTest tunnel for testing locally hosted applications.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.tunnel}
     */
    @Key("LambdaTest.tunnel")
    @DefaultValue("false")
    boolean tunnel();

    /**
     * Name of the LambdaTest tunnel to use when LambdaTest.tunnel is enabled.
     *
     * <p>Default: {@code false}.
     *
     * @return the configured value of {@code LambdaTest.tunnelName}
     */
    @Key("LambdaTest.tunnelName")
    @DefaultValue("false")
    String tunnelName();

    /**
     * Build name used in the LambdaTest SDK configuration. Use this instead of LambdaTest.build when
     * using the LambdaTest SDK (automate-config.yml).
     *
     * @return the configured value of {@code LambdaTest.buildName}
     */
    @Key("LambdaTest.buildName")
    @DefaultValue("")
    String buildName();

    /**
     * Selenium version to use on LambdaTest.
     *
     * @return the configured value of {@code LambdaTest.selenium_version}
     */
    @Key("LambdaTest.selenium_version")
    @DefaultValue("")
    String selenium_version();

    /**
     * Browser driver version to use on LambdaTest.
     *
     * @return the configured value of {@code LambdaTest.driver_version}
     */
    @Key("LambdaTest.driver_version")
    @DefaultValue("")
    String driver_version();

    /**
     * Enable W3C WebDriver protocol on LambdaTest.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.w3c}
     */
    @Key("LambdaTest.w3c")
    @DefaultValue("true")
    boolean w3c();

    //optional, uses random by default
    /**
     * Browser version, optional, uses random by default.
     *
     * @return the configured value of {@code LambdaTest.browserVersion}
     */
    @Key("LambdaTest.browserVersion")
    @DefaultValue("")
    String browserVersion();

    //Optional extra settings
    /**
     * Geolocation for test execution (e.g. US, IN).
     *
     * @return the configured value of {@code LambdaTest.geoLocation}
     */
    @Key("LambdaTest.geoLocation")
    @DefaultValue("")
    String geoLocation();

    /**
     * Enable debug mode (command logs) on LambdaTest.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.debug}
     */
    @Key("LambdaTest.debug")
    @DefaultValue("false")
    boolean debug();

    /**
     * Accept insecure SSL certificates on LambdaTest.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.acceptInsecureCerts}
     */
    @Key("LambdaTest.acceptInsecureCerts")
    @DefaultValue("true")
    boolean acceptInsecureCerts();

    /**
     * Enable network logs capture on LambdaTest.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.networkLogs}
     */
    @Key("LambdaTest.networkLogs")
    @DefaultValue("false")
    boolean networkLogs();

    /**
     * Appium version to use on LambdaTest for mobile testing.
     *
     * <p>Default: {@code 3.0.2}.
     *
     * @return the configured value of {@code LambdaTest.appiumVersion}
     */
    @Key("LambdaTest.appiumVersion")
    @DefaultValue("3.0.2")
    String appiumVersion();

    /**
     * Automatically grant app permissions on mobile devices.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.autoGrantPermissions}
     */
    @Key("LambdaTest.autoGrantPermissions")
    @DefaultValue("true")
    boolean autoGrantPermissions();

    /**
     * Automatically accept alerts on mobile devices.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.autoAcceptAlerts}
     */
    @Key("LambdaTest.autoAcceptAlerts")
    @DefaultValue("true")
    boolean autoAcceptAlerts();

    /**
     * Run tests on real mobile devices (as opposed to emulators/simulators).
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.isRealMobile}
     */
    @Key("LambdaTest.isRealMobile")
    @DefaultValue("true")
    boolean isRealMobile();

    /**
     * Enable console log capture on LambdaTest.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code LambdaTest.console}
     */
    @Key("LambdaTest.console")
    @DefaultValue("false")
    boolean console();

    /**
     * Custom app ID for testing a previously uploaded app by its custom identifier.
     *
     * @return the configured value of {@code LambdaTest.customID}
     */
    @Key("LambdaTest.customID")
    @DefaultValue("")
    String customID();

    /**
     * Starts a fluent, thread-local override of these properties for the current test thread.
     *
     * @return a new {@link LambdaTest.SetProperty} builder
     */
    default LambdaTest.SetProperty set() {
        return new LambdaTest.SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        /**
         * Overrides the {@code LambdaTest.username} property at runtime. LambdaTest username for
         * authentication.
         *
         * @param value the new value of {@code LambdaTest.username}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty username(String value) {
            setProperty("LambdaTest.username", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.accessKey} property at runtime. LambdaTest access key for
         * authentication.
         *
         * @param value the new value of {@code LambdaTest.accessKey}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty accessKey(String value) {
            setProperty("LambdaTest.accessKey", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.platformVersion} property at runtime. Mobile platform version
         * for LambdaTest execution.
         *
         * @param value the new value of {@code LambdaTest.platformVersion}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty platformVersion(String value) {
            setProperty("LambdaTest.platformVersion", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.osVersion} property at runtime. OS version for desktop browser
         * testing on LambdaTest.
         *
         * @param value the new value of {@code LambdaTest.osVersion}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty osVersion(String value) {
            setProperty("LambdaTest.osVersion", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.appUrl} property at runtime. Use appUrl to test a previously
         * uploaded app file.
         *
         * @param value the new value of {@code LambdaTest.appUrl}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty appUrl(String value) {
            setProperty("LambdaTest.appUrl", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.appProfiling} property at runtime. Enable app profiling during
         * mobile testing.
         *
         * @param value the new value of {@code LambdaTest.appProfiling}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty appProfiling(Boolean value) {
            setProperty("LambdaTest.appProfiling", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.deviceName} property at runtime. Mobile device name for
         * LambdaTest execution.
         *
         * @param value the new value of {@code LambdaTest.deviceName}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty deviceName(String value) {
            setProperty("LambdaTest.deviceName", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.visual} property at runtime. Enable visual logs (screenshots) on
         * LambdaTest.
         *
         * @param value the new value of {@code LambdaTest.visual}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty visual(boolean value) {
            setProperty("LambdaTest.visual", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code browserStack.video} property at runtime.
         *
         * @param value the new value of {@code browserStack.video}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty video(boolean value) {
            setProperty("browserStack.video", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.platformVersion} property at runtime. Mobile platform version
         * for LambdaTest execution.
         *
         * @param value the new value of {@code LambdaTest.platformVersion}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty resolution(String value) {
            setProperty("LambdaTest.platformVersion", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.headless} property at runtime. Enable headless browser execution
         * on LambdaTest.
         *
         * @param value the new value of {@code LambdaTest.headless}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty headless(boolean value) {
            setProperty("LambdaTest.headless", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.timezone} property at runtime. Timezone for test execution on
         * LambdaTest (e.g.
         *
         * @param value the new value of {@code LambdaTest.timezone}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty timezone(String value) {
            setProperty("LambdaTest.timezone", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.project} property at runtime. Project name to group builds in
         * the LambdaTest dashboard.
         *
         * @param value the new value of {@code LambdaTest.project}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty project(String value) {
            setProperty("LambdaTest.project", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.build} property at runtime. Build name to group test runs in the
         * LambdaTest dashboard.
         *
         * @param value the new value of {@code LambdaTest.build}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty build(String value) {
            setProperty("LambdaTest.build", value);
            return this;
        }


        /**
         * Overrides the {@code LambdaTest.tunnel} property at runtime. Enable LambdaTest tunnel for
         * testing locally hosted applications.
         *
         * @param value the new value of {@code LambdaTest.tunnel}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty tunnel(boolean value) {
            setProperty("LambdaTest.tunnel", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.tunnelName} property at runtime. Name of the LambdaTest tunnel
         * to use when LambdaTest.tunnel is enabled.
         *
         * @param value the new value of {@code LambdaTest.tunnelName}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty tunnelName(String value) {
            setProperty("LambdaTest.tunnelName", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.buildName} property at runtime. Build name used in the
         * LambdaTest SDK configuration.
         *
         * @param value the new value of {@code LambdaTest.buildName}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty buildName(String value) {
            setProperty("LambdaTest.buildName", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.autoGrantPermissions} property at runtime. Automatically grant
         * app permissions on mobile devices.
         *
         * @param value the new value of {@code LambdaTest.autoGrantPermissions}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty autoGrantPermissions(boolean value) {
            setProperty("LambdaTest.autoGrantPermissions", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.autoAcceptAlerts} property at runtime. Automatically accept
         * alerts on mobile devices.
         *
         * @param value the new value of {@code LambdaTest.autoAcceptAlerts}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty autoAcceptAlerts(boolean value) {
            setProperty("LambdaTest.autoAcceptAlerts", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.acceptInsecureCerts} property at runtime. Accept insecure SSL
         * certificates on LambdaTest.
         *
         * @param value the new value of {@code LambdaTest.acceptInsecureCerts}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty acceptInsecureCerts(boolean value) {
            setProperty("LambdaTest.acceptInsecureCerts", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.isRealMobile} property at runtime. Run tests on real mobile
         * devices (as opposed to emulators/simulators).
         *
         * @param value the new value of {@code LambdaTest.isRealMobile}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty isRealMobile(boolean value) {
            setProperty("LambdaTest.isRealMobile", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.debug} property at runtime. Enable debug mode (command logs) on
         * LambdaTest.
         *
         * @param value the new value of {@code LambdaTest.debug}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty debug(boolean value) {
            setProperty("LambdaTest.debug", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.console} property at runtime. Enable console log capture on
         * LambdaTest.
         *
         * @param value the new value of {@code LambdaTest.console}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty console(boolean value) {
            setProperty("LambdaTest.console", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.selenium_version} property at runtime. Selenium version to use
         * on LambdaTest.
         *
         * @param value the new value of {@code LambdaTest.selenium_version}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty selenium_version(String value) {
            setProperty("LambdaTest.selenium_version", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.browserVersion} property at runtime. Browser version, optional,
         * uses random by default.
         *
         * @param value the new value of {@code LambdaTest.browserVersion}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty browserVersion(String value) {
            setProperty("LambdaTest.browserVersion", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.appiumVersion} property at runtime. Appium version to use on
         * LambdaTest for mobile testing.
         *
         * @param value the new value of {@code LambdaTest.appiumVersion}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty appiumVersion(String value) {
            setProperty("LambdaTest.appiumVersion", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.networkLogs} property at runtime. Enable network logs capture on
         * LambdaTest.
         *
         * @param value the new value of {@code LambdaTest.networkLogs}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty networkLogs(boolean value) {
            setProperty("LambdaTest.networkLogs", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.appRelativeFilePath} property at runtime. Use appName and
         * appRelativeFilePath to upload a new app file and test it.
         *
         * @param value the new value of {@code LambdaTest.appRelativeFilePath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty appRelativeFilePath(String value) {
            setProperty("LambdaTest.appRelativeFilePath", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.appName} property at runtime. Use appName and
         * appRelativeFilePath to upload a new app file and test it.
         *
         * @param value the new value of {@code LambdaTest.appName}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty appName(String value) {
            setProperty("LambdaTest.appName", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.driver_version} property at runtime. Browser driver version to
         * use on LambdaTest.
         *
         * @param value the new value of {@code LambdaTest.driver_version}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty driver_version(String value) {
            setProperty("LambdaTest.driver_version", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.w3c} property at runtime. Enable W3C WebDriver protocol on
         * LambdaTest.
         *
         * @param value the new value of {@code LambdaTest.w3c}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty w3c(boolean value) {
            setProperty("LambdaTest.w3c", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.geoLocation} property at runtime. Geolocation for test execution
         * (e.g.
         *
         * @param value the new value of {@code LambdaTest.geoLocation}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty geoLocation(String value) {
            setProperty("LambdaTest.geoLocation", value);
            return this;
        }

        /**
         * Overrides the {@code LambdaTest.customID} property at runtime. Custom app ID for testing a
         * previously uploaded app by its custom identifier.
         *
         * @param value the new value of {@code LambdaTest.customID}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty customID(String value) {
            setProperty("LambdaTest.customID", value);
            return this;
        }

    }
}
