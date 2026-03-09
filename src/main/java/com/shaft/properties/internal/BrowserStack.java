package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for BrowserStack cloud execution in the SHAFT framework.
 * Covers all capabilities required for both desktop-web and native-mobile testing via BrowserStack's
 * Automate and App Automate platforms.
 *
 * <p>Refer to the BrowserStack capability builders for the full list of supported options:
 * <ul>
 *   <li>Mobile Native: <a href="https://www.browserstack.com/app-automate/capabilities?tag=w3c">App Automate Capabilities</a></li>
 *   <li>Web: <a href="https://www.browserstack.com/automate/capabilities?tag=selenium-4">Automate Capabilities</a></li>
 * </ul>
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.browserStack.set()
 *     .userName("myUser")
 *     .accessKey("myKey")
 *     .osVersion("11")
 *     .browserVersion("latest");
 * }</pre>
 */
@SuppressWarnings({"SpellCheckingInspection", "unused"})
@Sources({"system:properties",
        "file:src/main/resources/properties/browserStack.properties",
        "file:src/main/resources/properties/default/browserStack.properties",
        "classpath:browserStack.properties",
})
public interface BrowserStack extends EngineProperties<BrowserStack> {
    //Based on BrowserStack capability builder
    // For Mobile Native https://www.browserstack.com/app-automate/capabilities?tag=w3c
    // For Web https://www.browserstack.com/automate/capabilities?tag=selenium-4

    //In case of Mobile Native testing:
    //You must set the "targetOperatingSystem" property under "ExecutionPlatform.properties" or programmatically
    //You must set the "mobile_automationName" property under "MobileCapabilities.properties" or programmatically

    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.browserStackOverride.set(ConfigFactory.create(BrowserStack.class, ThreadLocalPropertiesManager.getOverrides()));
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    //Below properties are all required
    /**
     * BrowserStack account username used for authentication.
     * <p>Property key: {@code browserStack.userName}. No default is provided by the framework;
     * supply this value via a properties file or system property.
     *
     * @return the BrowserStack username string
     */
    @Key("browserStack.userName")
    @DefaultValue("")
    String userName();

    /**
     * BrowserStack access key used for authentication alongside {@link #userName()}.
     * <p>Property key: {@code browserStack.accessKey}. No default is provided by the framework;
     * supply this value via a properties file or system property.
     *
     * @return the BrowserStack access key string
     */
    @Key("browserStack.accessKey")
    @DefaultValue("")
    String accessKey();

    //Below properties are needed for native mobile app testing:
    //Required
    /**
     * Target mobile platform version for native app testing on BrowserStack App Automate
     * (e.g., {@code "11"} for Android 11).
     * <p>Property key: {@code browserStack.platformVersion} — default: {@code ""}
     *
     * @return the platform version string, or empty if not configured
     */
    @Key("browserStack.platformVersion")
    @DefaultValue("")
    String platformVersion();

    /**
     * Target device name for mobile app testing on BrowserStack App Automate
     * (e.g., {@code "Samsung Galaxy S21"}).
     * <p>Property key: {@code browserStack.deviceName} — default: {@code ""}
     *
     * @return the device name string, or empty if not configured
     */
    @Key("browserStack.deviceName")
    @DefaultValue("")
    String deviceName();

    //Use appUrl to test a previously uploaded app file
    /**
     * URL of a previously uploaded app on BrowserStack App Automate.
     * Note that this URL expires regularly; use {@link #customID()} to reference the latest upload.
     * <p>Property key: {@code browserStack.appUrl} — default: {@code ""}
     *
     * @return the app URL string, or empty if not configured
     */
    @Key("browserStack.appUrl")
    @DefaultValue("")
    String appUrl();

    //Use customID to test the latest uploaded version as the above url expires regularly
    /**
     * Custom ID assigned to an app uploaded to BrowserStack App Automate.
     * Using a custom ID always resolves to the latest uploaded version with that ID,
     * avoiding expiry issues associated with {@link #appUrl()}.
     * <p>Property key: {@code browserStack.customID} — default: {@code ""}
     *
     * @return the custom ID string, or empty if not configured
     */
    @Key("browserStack.customID")
    @DefaultValue("")
    String customID();

    //Use appName and appRelativeFilePath to upload a new app file and test it
    /**
     * Name assigned to a new app file being uploaded to BrowserStack App Automate.
     * Used together with {@link #appRelativeFilePath()}.
     * <p>Property key: {@code browserStack.appName} — default: {@code ""}
     *
     * @return the app name string, or empty if not configured
     */
    @Key("browserStack.appName")
    @DefaultValue("")
    String appName();

    /**
     * Relative file path of the app to be uploaded to BrowserStack App Automate.
     * Used together with {@link #appName()}.
     * <p>Property key: {@code browserStack.appRelativeFilePath} — default: {@code ""}
     *
     * @return the relative file path string, or empty if not configured
     */
    @Key("browserStack.appRelativeFilePath")
    @DefaultValue("")
    String appRelativeFilePath();

    //In case of Desktop web testing:
    //You must set the "targetOperatingSystem" property under "ExecutionPlatform.properties" or programmatically
    //You must set the "targetBrowserName" property under "ExecutionPlatform.properties" or programmatically
    //Required
    /**
     * Target operating system version for desktop web testing on BrowserStack Automate
     * (e.g., {@code "10"} for Windows 10).
     * <p>Property key: {@code browserStack.osVersion} — default: {@code ""}
     *
     * @return the OS version string, or empty if not configured
     */
    @Key("browserStack.osVersion")
    @DefaultValue("")
    String osVersion();

    //optional, uses random by default
    /**
     * Target browser version for desktop web testing on BrowserStack Automate.
     * When empty, BrowserStack selects an available version at random.
     * <p>Property key: {@code browserStack.browserVersion} — default: {@code ""}
     *
     * @return the browser version string, or empty to use a random version
     */
    @Key("browserStack.browserVersion")
    @DefaultValue("")
    String browserVersion();

    //Do not change these unless you know what you're doing
    /**
     * Whether BrowserStack Local testing is enabled for accessing localhost or internal servers.
     * Requires the BrowserStack Local binary to be running.
     * <p>Property key: {@code browserStack.local} — default: {@code false}
     *
     * @return {@code true} to enable Local testing; {@code false} otherwise
     */
    @Key("browserStack.local")
    @DefaultValue("false")
    boolean local();

    /**
     * Selenium version to use on BrowserStack Automate.
     * <p>Property key: {@code browserStack.seleniumVersion} — default: {@code "4.41.0"}
     *
     * @return the Selenium version string
     */
    @Key("browserStack.seleniumVersion")
    @DefaultValue("4.41.0")
    String seleniumVersion();

    /**
     * Whether to accept insecure SSL certificates during test execution on BrowserStack.
     * <p>Property key: {@code browserStack.acceptInsecureCerts} — default: {@code true}
     *
     * @return {@code true} to accept insecure certificates; {@code false} otherwise
     */
    @Key("browserStack.acceptInsecureCerts")
    @DefaultValue("true")
    boolean acceptInsecureCerts();

    /**
     * Whether BrowserStack debug mode is enabled, capturing additional screenshots
     * to aid in diagnosing test failures.
     * <p>Property key: {@code browserStack.debug} — default: {@code false}
     *
     * @return {@code true} to enable debug mode; {@code false} otherwise
     */
    @Key("browserStack.debug")
    @DefaultValue("false")
    boolean debug();

    //This was removed due to a change with browserstack whereby the user should not attempt to set any attribute that is outside their license scope
//    @Key("browserStack.enableBiometric")
//    @DefaultValue("false")
//    boolean enableBiometric();

    /**
     * Whether network log capture is enabled during test execution on BrowserStack.
     * <p>Property key: {@code browserStack.networkLogs} — default: {@code false}
     *
     * @return {@code true} to capture network logs; {@code false} otherwise
     */
    @Key("browserStack.networkLogs")
    @DefaultValue("false")
    boolean networkLogs();

    //Optional extra settings
    //Enterprise accounts only: https://www.browserstack.com/ip-geolocation
    /**
     * Geographic location code for IP geolocation testing on BrowserStack (enterprise accounts only).
     * Refer to <a href="https://www.browserstack.com/ip-geolocation">BrowserStack IP Geolocation</a>
     * for supported location codes.
     * <p>Property key: {@code browserStack.geoLocation} — default: {@code ""}
     *
     * @return the geolocation code string, or empty if not configured
     */
    @Key("browserStack.geoLocation")
    @DefaultValue("")
    String geoLocation();

    /**
     * Appium version to use on BrowserStack App Automate for mobile native testing.
     * <p>Property key: {@code browserStack.appiumVersion} — default: {@code "3.1.0"}
     *
     * @return the Appium version string
     */
    @Key("browserStack.appiumVersion")
    @DefaultValue("2.19.0")
    String appiumVersion();

    default SetProperty set() {
        return new SetProperty();
    }

    /**
     * Fluent builder that allows programmatic override of individual BrowserStack configuration
     * properties at runtime. All setter methods return {@code this} to support method chaining.
     *
     * <p>Example:
     * <pre>{@code
     * SHAFT.Properties.browserStack.set()
     *     .userName("myUser")
     *     .accessKey("myKey");
     * }</pre>
     */
    class SetProperty implements EngineProperties.SetProperty {

        /**
         * Creates a new {@code SetProperty} instance.
         */
        public SetProperty() {
        }

        /**
         * Overrides the {@code browserStack.userName} property at runtime.
         *
         * @param value the BrowserStack account username
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty userName(String value) {
            setProperty("browserStack.userName", value);
            return this;
        }

        /**
         * Overrides the {@code browserStack.accessKey} property at runtime.
         *
         * @param value the BrowserStack access key
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty accessKey(String value) {
            setProperty("browserStack.accessKey", value);
            return this;
        }

        /**
         * Overrides the {@code browserStack.platformVersion} property at runtime.
         *
         * @param value the target mobile platform version (e.g., {@code "11"})
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty platformVersion(String value) {
            setProperty("browserStack.platformVersion", value);
            return this;
        }

        /**
         * Overrides the {@code browserStack.deviceName} property at runtime.
         *
         * @param value the target device name for mobile testing
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty deviceName(String value) {
            setProperty("browserStack.deviceName", value);
            return this;
        }

        /**
         * Overrides the {@code browserStack.appUrl} property at runtime.
         *
         * @param value the URL of the previously uploaded app on BrowserStack
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty appUrl(String value) {
            setProperty("browserStack.appUrl", value);
            return this;
        }

        /**
         * Overrides the {@code browserStack.customID} property at runtime.
         *
         * @param value the custom ID of the uploaded app on BrowserStack
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty customID(String value) {
            setProperty("browserStack.customID", value);
            return this;
        }

        /**
         * Overrides the {@code browserStack.appName} property at runtime.
         *
         * @param value the name to assign to the app being uploaded to BrowserStack
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty appName(String value) {
            setProperty("browserStack.appName", value);
            return this;
        }

        /**
         * Overrides the {@code browserStack.appRelativeFilePath} property at runtime.
         *
         * @param value the relative file path of the app to upload to BrowserStack
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty appRelativeFilePath(String value) {
            setProperty("browserStack.appRelativeFilePath", value);
            return this;
        }

        /**
         * Overrides the {@code browserStack.osVersion} property at runtime.
         *
         * @param value the target OS version for desktop web testing (e.g., {@code "10"})
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty osVersion(String value) {
            setProperty("browserStack.osVersion", value);
            return this;
        }

        /**
         * Overrides the {@code browserStack.browserVersion} property at runtime.
         *
         * @param value the target browser version, or empty to use a random version
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty browserVersion(String value) {
            setProperty("browserStack.browserVersion", value);
            return this;
        }

        /**
         * Overrides the {@code browserStack.local} property at runtime.
         *
         * @param value {@code true} to enable BrowserStack Local testing
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty local(boolean value) {
            setProperty("browserStack.local", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code browserStack.seleniumVersion} property at runtime.
         *
         * @param value the Selenium version to use on BrowserStack Automate
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty seleniumVersion(String value) {
            setProperty("browserStack.seleniumVersion", value);
            return this;
        }

        /**
         * Overrides the {@code browserStack.appiumVersion} property at runtime.
         *
         * @param value the Appium version to use on BrowserStack App Automate
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty appiumVersion(String value) {
            setProperty("browserStack.appiumVersion", value);
            return this;
        }

        /**
         * Overrides the {@code browserStack.acceptInsecureCerts} property at runtime.
         *
         * @param value {@code true} to accept insecure SSL certificates during testing
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty acceptInsecureCerts(boolean value) {
            setProperty("browserStack.acceptInsecureCerts", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code browserStack.debug} property at runtime.
         *
         * @param value {@code true} to enable BrowserStack debug mode
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty debug(boolean value) {
            setProperty("browserStack.debug", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code browserStack.enableBiometric} property at runtime.
         *
         * @param value {@code true} to enable biometric authentication on the test device
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty enableBiometric(boolean value) {
            setProperty("browserStack.enableBiometric", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code browserStack.networkLogs} property at runtime.
         *
         * @param value {@code true} to enable network log capture during testing
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty networkLogs(boolean value) {
            setProperty("browserStack.networkLogs", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code browserStack.geoLocation} property at runtime.
         *
         * @param value the BrowserStack geolocation code for IP geolocation testing
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty geoLocation(String value) {
            setProperty("browserStack.geoLocation", value);
            return this;
        }

    }
}
