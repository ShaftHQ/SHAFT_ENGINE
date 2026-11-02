package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for web browser capabilities in the SHAFT framework.
 * Controls target browser type, headless mode, window size, page load strategy, and mobile emulation.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.web.set().targetBrowserName("firefox").headlessExecution(true);
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/WebCapabilities.properties", "file:src/main/resources/properties/default/WebCapabilities.properties", "classpath:WebCapabilities.properties"})
public interface Web extends EngineProperties<Web> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.webOverride.set(ConfigFactory.create(Web.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * The target web browser for test execution.
     *
     * <p>Default: {@code chrome}. Possible values: chrome, firefox, safari, edge.
     *
     * @return the configured value of {@code targetBrowserName}
     */
    @Key("targetBrowserName")
    @DefaultValue("chrome")
    String targetBrowserName();

    /**
     * This only works for Chrome and Firefox. Allows Selenium Manager to download a Chrome for Testing
     * browser build compatible with the current Selenium/browser resolution flow.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code forceBrowserDownload}
     */
    @Key("forceBrowserDownload")
    @DefaultValue("false")
    boolean forceBrowserDownload();

    /**
     * This only works for Chrome, Firefox and Edge.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code headlessExecution}
     */
    @Key("headlessExecution")
    @DefaultValue("false")
    boolean headlessExecution();

    /**
     * Enable browser incognito/private mode.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code incognitoMode}
     */
    @Key("incognitoMode")
    @DefaultValue("false")
    boolean incognitoMode();

    /**
     * This only works for Chrome and Edge.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code isMobileEmulation}
     */
    @Key("isMobileEmulation")
    @DefaultValue("false")
    boolean isMobileEmulation();

    /**
     * This only works for Chrome and Edge.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code mobileEmulation.isCustomDevice}
     */
    @Key("mobileEmulation.isCustomDevice")
    @DefaultValue("false")
    boolean mobileEmulationIsCustomDevice();

    /**
     * This only works for Chrome and Edge.
     *
     * <p>Possible values: blackberryZ30, BlackberryPlayBook, galaxyNote3, galaxyNoteII, galaxySIII,
     * galaxyS5, galaxyS8, samsungGalaxyS8+, galaxyS9+, galaxyTabS4, galaxyFold, samsungGalaxyS20Ultra,
     * samsungGalaxyA51/71, kindleFireHDX, lgOptimusL70, microsoftLumia550, microsoftLumia950, motoG4,
     * nexus10, nexus4, nexus5, nexus5X, nexus6, nexus6P, nexus7, nokiaLumia520, nokiaN9, nestHub,
     * nestHubMax, pixel2, pixel2XL, pixel3, pixel3XL, pixel4, pixel5, jioPhone2, iPhone4, iPhone5/SE,
     * iPhone6/7/8, iPhone6/7/8Plus, iPhoneSE, iPhoneX, iPhoneXR, iPhone12Pro, iPad, iPadPro, iPadAir,
     * iPadMini, surfacePro7, surfaceDuo.
     *
     * @return the configured value of {@code mobileEmulation.deviceName}
     */
    @Key("mobileEmulation.deviceName")
    @DefaultValue("")
    String mobileEmulationDeviceName();

    /**
     * This only works for Chrome and Edge.
     *
     * <p>Possible values: example: 360.
     *
     * @return the configured value of {@code mobileEmulation.width}
     */
    @Key("mobileEmulation.width")
    @DefaultValue("")
    int mobileEmulationWidth();

    /**
     * This only works for Chrome and Edge.
     *
     * <p>Possible values: example: 600.
     *
     * @return the configured value of {@code mobileEmulation.height}
     */
    @Key("mobileEmulation.height")
    @DefaultValue("")
    int mobileEmulationHeight();

    /**
     * This only works for Chrome and Edge.
     *
     * <p>Default: {@code 1.0}. Possible values: example: 2.0.
     *
     * @return the configured value of {@code mobileEmulation.pixelRatio}
     */
    @Key("mobileEmulation.pixelRatio")
    @DefaultValue("1.0")
    double mobileEmulationPixelRatio();

    /**
     * This only works for Chrome and Edge.
     *
     * <p>Possible values: example: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:35.0) Gecko/20100101
     * Firefox/35.0.
     *
     * @return the configured value of {@code mobileEmulation.userAgent}
     */
    @Key("mobileEmulation.userAgent")
    @DefaultValue("")
    String mobileEmulationUserAgent();

    /**
     * Base URL for the application under test.
     *
     * <p>Possible values: example: https://github.com/ShaftHQ/SHAFT_ENGINE.
     *
     * @return the configured value of {@code baseURL}
     */
    @Key("baseURL")
    @DefaultValue("")
    String baseURL();

    /**
     * Won't work if autoMaximizeBrowserWindow is enabled.
     *
     * <p>Default: {@code 1920}.
     *
     * @return the configured value of {@code browserWindowWidth}
     */
    @Key("browserWindowWidth")
    @DefaultValue("1920")
    int browserWindowWidth();

    /**
     * Won't work if autoMaximizeBrowserWindow is enabled.
     *
     * <p>Default: {@code 1080}.
     *
     * @return the configured value of {@code browserWindowHeight}
     */
    @Key("browserWindowHeight")
    @DefaultValue("1080")
    int browserWindowHeight();

    // none, eager, or normal
    /**
     * Controls when Selenium considers the page loaded. Default eager waits until DOMContentLoaded.
     *
     * <p>Default: {@code eager}. Possible values: none, eager, normal.
     *
     * @return the configured value of {@code pageLoadStrategy}
     */
    @Key("pageLoadStrategy")
    @DefaultValue("eager")
    String pageLoadStrategy();

    // interactive, none, or complete
    /**
     * Controls the document readiness state to wait for on BiDi navigation. Default interactive.
     *
     * <p>Default: {@code interactive}. Possible values: interactive, none, complete.
     *
     * @return the configured value of {@code readinessState}
     */
    @Key("readinessState")
    @DefaultValue("interactive")
    String readinessState();

    /**
     * Path to a storage-state JSON file (the schema produced by {@code BrowserActions.saveStorageState})
     * that a freshly-initialized driver should automatically load, restoring cookies and, when possible,
     * {@code localStorage}/{@code sessionStorage}.
     *
     * <p>At driver-init time there is no page loaded yet, so cookies cannot simply be added for an
     * arbitrary domain. SHAFT reads the {@code origin} recorded inside the storage-state file (falling
     * back to {@link #baseURL()} when the file has none) and navigates the fresh driver there first,
     * mirroring how {@code loadStorageState} recommends navigating before loading. Origin-scoped
     * {@code localStorage}/{@code sessionStorage} is therefore restored correctly only when an origin
     * could be resolved; if neither is available, only cookies that the driver accepts without a page
     * navigation are restored. A failure to load (missing file, unreachable origin, etc.) is logged as a
     * warning and does not fail driver initialization.
     *
     * <p>Default is blank, meaning the feature is disabled.
     */
    @Key("storageStatePath")
    @DefaultValue("")
    String storageStatePath();

    /**
     * Fail a passing test when the browser console captured error-level messages that do not match
     * {@code browserConsoleErrorAllowlist}. The messages are attached to the report.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code failOnBrowserConsoleErrors}
     */
    @Key("failOnBrowserConsoleErrors")
    @DefaultValue("false")
    boolean failOnBrowserConsoleErrors();

    /**
     * Java regular expression for console errors that never fail a test; combine several patterns with
     * {@code |}, for example {@code favicon\.ico|ResizeObserver loop}.
     *
     * <p>Default: empty (nothing is allowlisted).
     *
     * @return the configured value of {@code browserConsoleErrorAllowlist}
     */
    @Key("browserConsoleErrorAllowlist")
    @DefaultValue("")
    String browserConsoleErrorAllowlist();

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
         * Overrides the {@code baseURL} property at runtime. Base URL for the application under test.
         *
         * @param value the new value of {@code baseURL}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty baseURL(String value) {
            setProperty("baseURL", value);
            return this;
        }

        /**
         * @param value io.github.shafthq.shaft.enums.Browsers
         */
        public SetProperty targetBrowserName(String value) {
            setProperty("targetBrowserName", value);
            return this;
        }

        /**
         * Overrides the {@code forceBrowserDownload} property at runtime. This only works for Chrome and
         * Firefox.
         *
         * @param value the new value of {@code forceBrowserDownload}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty forceBrowserDownload(boolean value) {
            setProperty("forceBrowserDownload", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code headlessExecution} property at runtime. This only works for Chrome, Firefox
         * and Edge.
         *
         * @param value the new value of {@code headlessExecution}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty headlessExecution(boolean value) {
            setProperty("headlessExecution", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code incognitoMode} property at runtime. Enable browser incognito/private mode.
         *
         * @param value the new value of {@code incognitoMode}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty incognitoMode(boolean value) {
            setProperty("incognitoMode", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code isMobileEmulation} property at runtime. This only works for Chrome and
         * Edge.
         *
         * @param value the new value of {@code isMobileEmulation}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty isMobileEmulation(boolean value) {
            setProperty("isMobileEmulation", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code mobileEmulation.isCustomDevice} property at runtime. This only works for
         * Chrome and Edge.
         *
         * @param value the new value of {@code mobileEmulation.isCustomDevice}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty mobileEmulationIsCustomDevice(boolean value) {
            setProperty("mobileEmulation.isCustomDevice", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code mobileEmulation.deviceName} property at runtime. This only works for Chrome
         * and Edge.
         *
         * @param value the new value of {@code mobileEmulation.deviceName}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty mobileEmulationDeviceName(String value) {
            setProperty("mobileEmulation.deviceName", value);
            return this;
        }

        /**
         * Overrides the {@code mobileEmulation.width} property at runtime. This only works for Chrome and
         * Edge.
         *
         * @param value the new value of {@code mobileEmulation.width}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty mobileEmulationWidth(int value) {
            setProperty("mobileEmulation.width", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code mobileEmulation.height} property at runtime. This only works for Chrome and
         * Edge.
         *
         * @param value the new value of {@code mobileEmulation.height}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty mobileEmulationHeight(int value) {
            setProperty("mobileEmulation.height", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code mobileEmulation.pixelRatio} property at runtime. This only works for Chrome
         * and Edge.
         *
         * @param value the new value of {@code mobileEmulation.pixelRatio}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty mobileEmulationPixelRatio(double value) {
            setProperty("mobileEmulation.pixelRatio", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code mobileEmulation.userAgent} property at runtime. This only works for Chrome
         * and Edge.
         *
         * @param value the new value of {@code mobileEmulation.userAgent}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty mobileEmulationUserAgent(String value) {
            setProperty("mobileEmulation.userAgent", value);
            return this;
        }

        /**
         * Overrides the {@code browserWindowWidth} property at runtime. Won't work if
         * autoMaximizeBrowserWindow is enabled.
         *
         * @param value the new value of {@code browserWindowWidth}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty browserWindowWidth(int value) {
            setProperty("browserWindowWidth", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code browserWindowHeight} property at runtime. Won't work if
         * autoMaximizeBrowserWindow is enabled.
         *
         * @param value the new value of {@code browserWindowHeight}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty browserWindowHeight(int value) {
            setProperty("browserWindowHeight", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code pageLoadStrategy} property at runtime. Controls when Selenium considers the
         * page loaded.
         *
         * @param value the new value of {@code pageLoadStrategy}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty pageLoadStrategy(String value) {
            setProperty("pageLoadStrategy", value);
            return this;
        }

        /**
         * Overrides the {@code readinessState} property at runtime. Controls the document readiness state
         * to wait for on BiDi navigation.
         *
         * @param value the new value of {@code readinessState}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty readinessState(String value) {
            setProperty("readinessState", value);
            return this;
        }

        /**
         * Overrides the {@code storageStatePath} property at runtime. Path to a storage-state JSON file
         * automatically loaded into a freshly-initialized driver.
         *
         * @param value the new value of {@code storageStatePath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty storageStatePath(String value) {
            setProperty("storageStatePath", value);
            return this;
        }

        /**
         * Overrides the {@code failOnBrowserConsoleErrors} property at runtime.
         *
         * @param value the new value of {@code failOnBrowserConsoleErrors}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty failOnBrowserConsoleErrors(boolean value) {
            setProperty("failOnBrowserConsoleErrors", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code browserConsoleErrorAllowlist} property at runtime.
         *
         * @param value the new value of {@code browserConsoleErrorAllowlist}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty browserConsoleErrorAllowlist(String value) {
            setProperty("browserConsoleErrorAllowlist", value);
            return this;
        }
    }

}
