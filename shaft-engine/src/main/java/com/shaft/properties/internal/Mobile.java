package com.shaft.properties.internal;

import io.appium.java_client.remote.AutomationName;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for mobile and Appium testing in the SHAFT framework.
 * Controls device name, platform version, app path, automation engine, and Appium server settings.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.mobile.set().deviceName("Pixel_5").platformVersion("13.0");
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/MobileCapabilities.properties",
        "file:src/main/resources/properties/default/MobileCapabilities.properties",
        "classpath:MobileCapabilities.properties",
})
public interface Mobile extends EngineProperties<Mobile> {

    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.mobileOverride.set(ConfigFactory.create(Mobile.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * Raw Appium platformName capability. Usually derived automatically from targetOperatingSystem;
     * set this to override it directly.
     *
     * <p>Possible values: example: Android, iOS.
     *
     * @return the configured value of {@code platformName}
     */
    @Key("platformName")
    @DefaultValue("")
    String platformName();

    /**
     * You can add any property from the List of Appium Capabilities directly to your .property files
     * or via CLI arguments, just make sure to add mobile_ as a prefix.
     *
     * <p>Possible values: example: 11.0, 13.0.
     *
     * @return the configured value of {@code mobile_platformVersion}
     */
    @Key("mobile_platformVersion")
    @DefaultValue("")
    String platformVersion();

    /**
     * You can add any property from the List of Appium Capabilities directly to your .property files
     * or via CLI arguments, just make sure to add mobile_ as a prefix.
     *
     * <p>Possible values: example: ANDROID_EMULATOR.
     *
     * @return the configured value of {@code mobile_deviceName}
     */
    @Key("mobile_deviceName")
    @DefaultValue("")
    String deviceName();

    /**
     * You can add any property from the List of Appium Capabilities directly to your .property files
     * or via CLI arguments, just make sure to add mobile_ as a prefix.
     *
     * <p>Default: {@code UiAutomator2}. Possible values: UiAutomator2, Espresso, XCUITest.
     *
     * @return the configured value of {@code mobile_automationName}
     */
    @Key("mobile_automationName")
    @DefaultValue(AutomationName.ANDROID_UIAUTOMATOR2)
    String automationName();

    /**
     * Unique device identifier of the connected physical device (leave empty if not applicable). You
     * can add any property from the List of Appium Capabilities directly to your .property files or
     * via CLI arguments, just make sure to add mobile_ as a prefix.
     *
     * <p>Possible values: example: RQ3005TAQP.
     *
     * @return the configured value of {@code mobile_udid}
     */
    @Key("mobile_udid")
    @DefaultValue("")
    String udid();

    /**
     * You can add any property from the List of Appium Capabilities directly to your .property files
     * or via CLI arguments, just make sure to add mobile_ as a prefix.
     *
     * <p>Possible values: chrome, Chromium, Browser, Safari, samsung.
     *
     * @return the configured value of {@code browserName}
     */
    @Key("browserName")
    @DefaultValue("")
    String browserName();

    /**
     * The WebDriver executable version compatible with the target mobile browser. Check Selenium
     * driver requirements for browser-specific compatibility. You can add any property from the List
     * of Appium Capabilities directly to your .property files or via CLI arguments, just make sure to
     * add mobile_ as a prefix.
     *
     * <p>Possible values: example: 83.0.4103.39.
     *
     * @return the configured value of {@code MobileBrowserVersion}
     */
    @Key("MobileBrowserVersion")
    @DefaultValue("")
    String browserVersion();

    /**
     * You can add any property from the List of Appium Capabilities directly to your .property files
     * or via CLI arguments, just make sure to add mobile_ as a prefix.
     *
     * <p>Possible values: relativePath/to/myApp.apk, absolutePath/to/myApp.apk,
     * http://myapp.com/app.ipa.
     *
     * @return the configured value of {@code mobile_app}
     */
    @Key("mobile_app")
    @DefaultValue("")
    String app();

    /**
     * You can add any property from the List of Appium Capabilities directly to your .property files
     * or via CLI arguments, just make sure to add mobile_ as a prefix.
     *
     * <p>Possible values: example: com.example.android.myApp.
     *
     * @return the configured value of {@code mobile_appPackage}
     */
    @Key("mobile_appPackage")
    @DefaultValue("")
    String appPackage();

    /**
     * You can add any property from the List of Appium Capabilities directly to your .property files
     * or via CLI arguments, just make sure to add mobile_ as a prefix.
     *
     * <p>Possible values: example: .MainActivity.
     *
     * @return the configured value of {@code mobile_appActivity}
     */
    @Key("mobile_appActivity")
    @DefaultValue("")
    String appActivity();

    /**
     * iOS bundle identifier for launching an already installed app without providing mobile_app. You
     * can add any property from the List of Appium Capabilities directly to your .property files or
     * via CLI arguments, just make sure to add mobile_ as a prefix.
     *
     * <p>Possible values: example: com.example.ios.myApp.
     *
     * @return the configured value of {@code mobile_bundleId}
     */
    @Key("mobile_bundleId")
    @DefaultValue("")
    String bundleId();

    /** Maximum time in seconds to wait for a Flutter element. Zero keeps the default wait behavior. */
    @Key("mobile_flutterElementWaitTimeout")
    @DefaultValue("60")
    int flutterElementWaitTimeout();

    /** Maximum time in seconds to wait for the Flutter driver server to start. */
    @Key("mobile_flutterServerLaunchTimeout")
    @DefaultValue("60")
    int flutterServerLaunchTimeout();

    /** Local system port used by the Flutter driver; zero lets the driver choose. */
    @Key("mobile_flutterSystemPort")
    @DefaultValue("0")
    int flutterSystemPort();

    /** Whether Flutter tests use a mocked camera instead of the device camera. */
    @Key("mobile_flutterEnableMockCamera")
    @DefaultValue("false")
    boolean flutterEnableMockCamera();

    /**
     * Starts a fluent, thread-local override of these properties for the current test thread.
     *
     * @return a new {@link SetProperty} builder
     */
    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {

        /**
         * Overrides the {@code selfManagedAndroidSDKVersion} property at runtime.
         *
         * @param value the new value of {@code selfManagedAndroidSDKVersion}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty selfManagedAndroidSDKVersion(int value) {
            setProperty("selfManagedAndroidSDKVersion", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code selfManaged} property at runtime.
         *
         * @param value the new value of {@code selfManaged}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty selfManaged(boolean value) {
            setProperty("selfManaged", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code platformName} property at runtime. Raw Appium platformName capability.
         *
         * @param value the new value of {@code platformName}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty platformName(String value) {
            setProperty("platformName", value);
            return this;
        }

        /**
         * Overrides the {@code mobile_platformVersion} property at runtime. You can add any property from
         * the List of Appium Capabilities directly to your .property files or via CLI arguments, just make
         * sure to add mobile_ as a prefix.
         *
         * @param value the new value of {@code mobile_platformVersion}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty platformVersion(String value) {
            setProperty("mobile_platformVersion", value);
            return this;
        }

        /**
         * Overrides the {@code mobile_deviceName} property at runtime. You can add any property from the
         * List of Appium Capabilities directly to your .property files or via CLI arguments, just make
         * sure to add mobile_ as a prefix.
         *
         * @param value the new value of {@code mobile_deviceName}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty deviceName(String value) {
            setProperty("mobile_deviceName", value);
            return this;
        }

        /**
         * @param value io.appium.java_client.remote.AutomationName
         */
        public SetProperty automationName(String value) {
            setProperty("mobile_automationName", value);
            return this;
        }

        /**
         * Overrides the {@code mobile_udid} property at runtime. Unique device identifier of the connected
         * physical device (leave empty if not applicable).
         *
         * @param value the new value of {@code mobile_udid}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty udid(String value) {
            setProperty("mobile_udid", value);
            return this;
        }

        /**
         * Overrides the {@code browserName} property at runtime. You can add any property from the List of
         * Appium Capabilities directly to your .property files or via CLI arguments, just make sure to add
         * mobile_ as a prefix.
         *
         * @param value the new value of {@code browserName}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty browserName(String value) {
            setProperty("browserName", value);
            return this;
        }

        /**
         * Overrides the {@code MobileBrowserVersion} property at runtime. The WebDriver executable version
         * compatible with the target mobile browser.
         *
         * @param value the new value of {@code MobileBrowserVersion}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty browserVersion(String value) {
            setProperty("MobileBrowserVersion", value);
            return this;
        }

        /**
         * Overrides the {@code mobile_app} property at runtime. You can add any property from the List of
         * Appium Capabilities directly to your .property files or via CLI arguments, just make sure to add
         * mobile_ as a prefix.
         *
         * @param value the new value of {@code mobile_app}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty app(String value) {
            setProperty("mobile_app", value);
            return this;
        }

        /**
         * Overrides the {@code mobile_appPackage} property at runtime. You can add any property from the
         * List of Appium Capabilities directly to your .property files or via CLI arguments, just make
         * sure to add mobile_ as a prefix.
         *
         * @param value the new value of {@code mobile_appPackage}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty appPackage(String value) {
            setProperty("mobile_appPackage", value);
            return this;
        }

        /**
         * Overrides the {@code mobile_appActivity} property at runtime. You can add any property from the
         * List of Appium Capabilities directly to your .property files or via CLI arguments, just make
         * sure to add mobile_ as a prefix.
         *
         * @param value the new value of {@code mobile_appActivity}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty appActivity(String value) {
            setProperty("mobile_appActivity", value);
            return this;
        }

        /**
         * Overrides the {@code mobile_bundleId} property at runtime. iOS bundle identifier for launching
         * an already installed app without providing mobile_app.
         *
         * @param value the new value of {@code mobile_bundleId}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty bundleId(String value) {
            setProperty("mobile_bundleId", value);
            return this;
        }

        /**
         * Overrides the {@code mobile_flutterElementWaitTimeout} property at runtime. Mobile flutter
         * element wait timeout.
         *
         * @param value the new value of {@code mobile_flutterElementWaitTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty flutterElementWaitTimeout(int value) {
            setProperty("mobile_flutterElementWaitTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code mobile_flutterServerLaunchTimeout} property at runtime. Mobile flutter
         * server launch timeout.
         *
         * @param value the new value of {@code mobile_flutterServerLaunchTimeout}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty flutterServerLaunchTimeout(int value) {
            setProperty("mobile_flutterServerLaunchTimeout", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code mobile_flutterSystemPort} property at runtime. Mobile flutter system port.
         *
         * @param value the new value of {@code mobile_flutterSystemPort}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty flutterSystemPort(int value) {
            setProperty("mobile_flutterSystemPort", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code mobile_flutterEnableMockCamera} property at runtime. Mobile flutter enable
         * mock camera.
         *
         * @param value the new value of {@code mobile_flutterEnableMockCamera}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty flutterEnableMockCamera(boolean value) {
            setProperty("mobile_flutterEnableMockCamera", String.valueOf(value));
            return this;
        }
    }
}
