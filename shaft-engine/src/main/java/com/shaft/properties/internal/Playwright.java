package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Playwright-specific GUI backend settings. Browser name, headless mode, base
 * URL, and viewport are reused from {@link Web} unless an explicit Playwright
 * override is exposed here.
 */
@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/custom.properties",
        "file:src/main/resources/properties/default/custom.properties",
        "classpath:custom.properties"})
public interface Playwright extends EngineProperties<Playwright> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.playwrightOverride.set(ConfigFactory.create(Playwright.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * Optional Playwright browser engine override. When empty, SHAFT uses the device descriptor
     * browser type or falls back to targetBrowserName.
     *
     * <p>Possible values: chromium, chrome, firefox, webkit, safari, edge.
     *
     * @return the configured value of {@code playwright.browserName}
     */
    @Key("playwright.browserName")
    @DefaultValue("")
    String browserName();

    /**
     * Playwright device descriptor name to apply when creating a browser context.
     * Matches Playwright registry names when available, plus SHAFT-provided latest
     * device aliases for pinned Playwright versions.
     *
     * @return the configured Playwright device descriptor name
     */
    @Key("playwright.deviceName")
    @DefaultValue("")
    String deviceName();

    /**
     * Launch a local browser, connect over Playwright protocol, or connect over CDP.
     *
     * <p>Default: {@code local}. Possible values: local, connect, connectOverCDP.
     *
     * @return the configured value of {@code playwright.connectionMode}
     */
    @Key("playwright.connectionMode")
    @DefaultValue("local")
    String connectionMode();

    /**
     * Remote endpoint used when connectionMode is connect or connectOverCDP.
     *
     * <p>Possible values: WebSocket or CDP endpoint.
     *
     * @return the configured value of {@code playwright.endpoint}
     */
    @Key("playwright.endpoint")
    @DefaultValue("")
    String endpoint();

    /**
     * Optional Chromium channel passed to Playwright launch options.
     *
     * <p>Possible values: Chromium channel such as chrome or msedge.
     *
     * @return the configured value of {@code playwright.channel}
     */
    @Key("playwright.channel")
    @DefaultValue("")
    String channel();

    /**
     * Slow motion delay for Playwright actions.
     *
     * <p>Default: {@code 0}. Possible values: milliseconds.
     *
     * @return the configured value of {@code playwright.slowMo}
     */
    @Key("playwright.slowMo")
    @DefaultValue("0")
    int slowMo();

    /**
     * Timeout for launching or connecting to a browser.
     *
     * <p>Default: {@code 30000}. Possible values: milliseconds.
     *
     * @return the configured value of {@code playwright.launchTimeoutMilliseconds}
     */
    @Key("playwright.launchTimeoutMilliseconds")
    @DefaultValue("30000")
    int launchTimeoutMilliseconds();

    /**
     * Default timeout applied to Playwright page actions.
     *
     * <p>Default: {@code 30000}. Possible values: milliseconds.
     *
     * @return the configured value of {@code playwright.defaultTimeoutMilliseconds}
     */
    @Key("playwright.defaultTimeoutMilliseconds")
    @DefaultValue("30000")
    int defaultTimeoutMilliseconds();

    /**
     * Default timeout applied to Playwright page navigation.
     *
     * <p>Default: {@code 30000}. Possible values: milliseconds.
     *
     * @return the configured value of {@code playwright.navigationTimeoutMilliseconds}
     */
    @Key("playwright.navigationTimeoutMilliseconds")
    @DefaultValue("30000")
    int navigationTimeoutMilliseconds();

    /**
     * Directory for Playwright traces and artifacts.
     *
     * <p>Default: {@code target/playwright-artifacts}. Possible values: relative or absolute path.
     *
     * @return the configured value of {@code playwright.artifactsDirectory}
     */
    @Key("playwright.artifactsDirectory")
    @DefaultValue("target/playwright-artifacts")
    String artifactsDirectory();

    /**
     * Optional downloads directory. Empty uses Playwright defaults.
     *
     * <p>Possible values: relative or absolute path.
     *
     * @return the configured value of {@code playwright.downloadsDirectory}
     */
    @Key("playwright.downloadsDirectory")
    @DefaultValue("")
    String downloadsDirectory();

    /**
     * Allow Playwright browser contexts to accept downloads.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code playwright.acceptDownloads}
     */
    @Key("playwright.acceptDownloads")
    @DefaultValue("true")
    boolean acceptDownloads();

    /** @return timezone identifier applied when a new browser context is created, or blank for the browser default */
    @Key("playwright.timezoneId")
    @DefaultValue("")
    String timezoneId();

    /** @return locale applied when a new browser context is created, or blank for the browser default */
    @Key("playwright.locale")
    @DefaultValue("")
    String locale();

    /** @return user agent applied when a new browser context is created, or blank for the browser/device default */
    @Key("playwright.userAgent")
    @DefaultValue("")
    String userAgent();

    /** @return whether JavaScript is enabled in newly created browser contexts */
    @Key("playwright.javaScriptEnabled")
    @DefaultValue("true")
    boolean javaScriptEnabled();

    /**
     * Start Playwright tracing for the session.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code playwright.tracing.enabled}
     */
    @Key("playwright.tracing.enabled")
    @DefaultValue("false")
    boolean tracingEnabled();

    /**
     * Enable tracing automatically only during SHAFT retry evidence capture.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code playwright.tracing.onRetryOnly}
     */
    @Key("playwright.tracing.onRetryOnly")
    @DefaultValue("true")
    boolean tracingOnRetryOnly();

    /**
     * Include screenshots in trace artifacts.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code playwright.tracing.screenshots}
     */
    @Key("playwright.tracing.screenshots")
    @DefaultValue("true")
    boolean tracingScreenshots();

    /**
     * Include DOM snapshots in trace artifacts.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code playwright.tracing.snapshots}
     */
    @Key("playwright.tracing.snapshots")
    @DefaultValue("true")
    boolean tracingSnapshots();

    /**
     * Include source files in trace artifacts.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code playwright.tracing.sources}
     */
    @Key("playwright.tracing.sources")
    @DefaultValue("true")
    boolean tracingSources();

    @Override
    default PlaywrightSetProperty set() {
        return new PlaywrightSetProperty();
    }

    class PlaywrightSetProperty implements EngineProperties.SetProperty {
        /**
         * Overrides the {@code playwright.browserName} property at runtime. Optional Playwright browser
         * engine override.
         *
         * @param value the new value of {@code playwright.browserName}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty browserName(String value) {
            setProperty("playwright.browserName", value);
            return this;
        }

        /**
         * Sets the Playwright device descriptor name for new contexts.
         *
         * @param value descriptor name, for example {@code iPhone 17 Pro Max}
         * @return this property setter
         */
        public PlaywrightSetProperty deviceName(String value) {
            setProperty("playwright.deviceName", value);
            return this;
        }

        /**
         * Overrides the {@code playwright.connectionMode} property at runtime. Launch a local browser,
         * connect over Playwright protocol, or connect over CDP.
         *
         * @param value the new value of {@code playwright.connectionMode}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty connectionMode(String value) {
            setProperty("playwright.connectionMode", value);
            return this;
        }

        /**
         * Overrides the {@code playwright.endpoint} property at runtime. Remote endpoint used when
         * connectionMode is connect or connectOverCDP.
         *
         * @param value the new value of {@code playwright.endpoint}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty endpoint(String value) {
            setProperty("playwright.endpoint", value);
            return this;
        }

        /**
         * Overrides the {@code playwright.channel} property at runtime. Optional Chromium channel passed
         * to Playwright launch options.
         *
         * @param value the new value of {@code playwright.channel}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty channel(String value) {
            setProperty("playwright.channel", value);
            return this;
        }

        /**
         * Overrides the {@code playwright.slowMo} property at runtime. Slow motion delay for Playwright
         * actions.
         *
         * @param value the new value of {@code playwright.slowMo}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty slowMo(int value) {
            setProperty("playwright.slowMo", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code playwright.launchTimeoutMilliseconds} property at runtime. Timeout for
         * launching or connecting to a browser.
         *
         * @param value the new value of {@code playwright.launchTimeoutMilliseconds}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty launchTimeoutMilliseconds(int value) {
            setProperty("playwright.launchTimeoutMilliseconds", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code playwright.defaultTimeoutMilliseconds} property at runtime. Default timeout
         * applied to Playwright page actions.
         *
         * @param value the new value of {@code playwright.defaultTimeoutMilliseconds}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty defaultTimeoutMilliseconds(int value) {
            setProperty("playwright.defaultTimeoutMilliseconds", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code playwright.navigationTimeoutMilliseconds} property at runtime. Default
         * timeout applied to Playwright page navigation.
         *
         * @param value the new value of {@code playwright.navigationTimeoutMilliseconds}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty navigationTimeoutMilliseconds(int value) {
            setProperty("playwright.navigationTimeoutMilliseconds", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code playwright.artifactsDirectory} property at runtime. Directory for
         * Playwright traces and artifacts.
         *
         * @param value the new value of {@code playwright.artifactsDirectory}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty artifactsDirectory(String value) {
            setProperty("playwright.artifactsDirectory", value);
            return this;
        }

        /**
         * Overrides the {@code playwright.downloadsDirectory} property at runtime. Optional downloads
         * directory.
         *
         * @param value the new value of {@code playwright.downloadsDirectory}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty downloadsDirectory(String value) {
            setProperty("playwright.downloadsDirectory", value);
            return this;
        }

        /**
         * Overrides the {@code playwright.acceptDownloads} property at runtime. Allow Playwright browser
         * contexts to accept downloads.
         *
         * @param value the new value of {@code playwright.acceptDownloads}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty acceptDownloads(boolean value) {
            setProperty("playwright.acceptDownloads", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code playwright.timezoneId} property at runtime. Optional time zone for new
         * browser contexts.
         *
         * @param value the new value of {@code playwright.timezoneId}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty timezoneId(String value) {
            setProperty("playwright.timezoneId", value);
            return this;
        }

        /**
         * Overrides the {@code playwright.locale} property at runtime. Optional locale for new browser
         * contexts.
         *
         * @param value the new value of {@code playwright.locale}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty locale(String value) {
            setProperty("playwright.locale", value);
            return this;
        }

        /**
         * Overrides the {@code playwright.userAgent} property at runtime. Optional user agent for new
         * browser contexts.
         *
         * @param value the new value of {@code playwright.userAgent}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty userAgent(String value) {
            setProperty("playwright.userAgent", value);
            return this;
        }

        /**
         * Overrides the {@code playwright.javaScriptEnabled} property at runtime. Enables JavaScript in
         * new browser contexts.
         *
         * @param value the new value of {@code playwright.javaScriptEnabled}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty javaScriptEnabled(boolean value) {
            setProperty("playwright.javaScriptEnabled", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code playwright.tracing.enabled} property at runtime. Start Playwright tracing
         * for the session.
         *
         * @param value the new value of {@code playwright.tracing.enabled}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty tracingEnabled(boolean value) {
            setProperty("playwright.tracing.enabled", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code playwright.tracing.onRetryOnly} property at runtime. Enable tracing
         * automatically only during SHAFT retry evidence capture.
         *
         * @param value the new value of {@code playwright.tracing.onRetryOnly}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty tracingOnRetryOnly(boolean value) {
            setProperty("playwright.tracing.onRetryOnly", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code playwright.tracing.screenshots} property at runtime. Include screenshots in
         * trace artifacts.
         *
         * @param value the new value of {@code playwright.tracing.screenshots}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty tracingScreenshots(boolean value) {
            setProperty("playwright.tracing.screenshots", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code playwright.tracing.snapshots} property at runtime. Include DOM snapshots in
         * trace artifacts.
         *
         * @param value the new value of {@code playwright.tracing.snapshots}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty tracingSnapshots(boolean value) {
            setProperty("playwright.tracing.snapshots", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code playwright.tracing.sources} property at runtime. Include source files in
         * trace artifacts.
         *
         * @param value the new value of {@code playwright.tracing.sources}
         * @return this {@link PlaywrightSetProperty} instance for chaining
         */
        public PlaywrightSetProperty tracingSources(boolean value) {
            setProperty("playwright.tracing.sources", String.valueOf(value));
            return this;
        }
    }
}
