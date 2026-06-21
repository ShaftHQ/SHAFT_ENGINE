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

    @Key("playwright.connectionMode")
    @DefaultValue("local")
    String connectionMode();

    @Key("playwright.endpoint")
    @DefaultValue("")
    String endpoint();

    @Key("playwright.channel")
    @DefaultValue("")
    String channel();

    @Key("playwright.slowMo")
    @DefaultValue("0")
    int slowMo();

    @Key("playwright.launchTimeoutMilliseconds")
    @DefaultValue("30000")
    int launchTimeoutMilliseconds();

    @Key("playwright.defaultTimeoutMilliseconds")
    @DefaultValue("30000")
    int defaultTimeoutMilliseconds();

    @Key("playwright.navigationTimeoutMilliseconds")
    @DefaultValue("30000")
    int navigationTimeoutMilliseconds();

    @Key("playwright.artifactsDirectory")
    @DefaultValue("target/playwright-artifacts")
    String artifactsDirectory();

    @Key("playwright.downloadsDirectory")
    @DefaultValue("")
    String downloadsDirectory();

    @Key("playwright.acceptDownloads")
    @DefaultValue("true")
    boolean acceptDownloads();

    @Key("playwright.tracing.enabled")
    @DefaultValue("false")
    boolean tracingEnabled();

    @Key("playwright.tracing.onRetryOnly")
    @DefaultValue("true")
    boolean tracingOnRetryOnly();

    @Key("playwright.tracing.screenshots")
    @DefaultValue("true")
    boolean tracingScreenshots();

    @Key("playwright.tracing.snapshots")
    @DefaultValue("true")
    boolean tracingSnapshots();

    @Key("playwright.tracing.sources")
    @DefaultValue("true")
    boolean tracingSources();

    @Override
    default PlaywrightSetProperty set() {
        return new PlaywrightSetProperty();
    }

    class PlaywrightSetProperty implements EngineProperties.SetProperty {
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

        public PlaywrightSetProperty connectionMode(String value) {
            setProperty("playwright.connectionMode", value);
            return this;
        }

        public PlaywrightSetProperty endpoint(String value) {
            setProperty("playwright.endpoint", value);
            return this;
        }

        public PlaywrightSetProperty channel(String value) {
            setProperty("playwright.channel", value);
            return this;
        }

        public PlaywrightSetProperty slowMo(int value) {
            setProperty("playwright.slowMo", String.valueOf(value));
            return this;
        }

        public PlaywrightSetProperty launchTimeoutMilliseconds(int value) {
            setProperty("playwright.launchTimeoutMilliseconds", String.valueOf(value));
            return this;
        }

        public PlaywrightSetProperty defaultTimeoutMilliseconds(int value) {
            setProperty("playwright.defaultTimeoutMilliseconds", String.valueOf(value));
            return this;
        }

        public PlaywrightSetProperty navigationTimeoutMilliseconds(int value) {
            setProperty("playwright.navigationTimeoutMilliseconds", String.valueOf(value));
            return this;
        }

        public PlaywrightSetProperty artifactsDirectory(String value) {
            setProperty("playwright.artifactsDirectory", value);
            return this;
        }

        public PlaywrightSetProperty downloadsDirectory(String value) {
            setProperty("playwright.downloadsDirectory", value);
            return this;
        }

        public PlaywrightSetProperty acceptDownloads(boolean value) {
            setProperty("playwright.acceptDownloads", String.valueOf(value));
            return this;
        }

        public PlaywrightSetProperty tracingEnabled(boolean value) {
            setProperty("playwright.tracing.enabled", String.valueOf(value));
            return this;
        }

        public PlaywrightSetProperty tracingOnRetryOnly(boolean value) {
            setProperty("playwright.tracing.onRetryOnly", String.valueOf(value));
            return this;
        }

        public PlaywrightSetProperty tracingScreenshots(boolean value) {
            setProperty("playwright.tracing.screenshots", String.valueOf(value));
            return this;
        }

        public PlaywrightSetProperty tracingSnapshots(boolean value) {
            setProperty("playwright.tracing.snapshots", String.valueOf(value));
            return this;
        }

        public PlaywrightSetProperty tracingSources(boolean value) {
            setProperty("playwright.tracing.sources", String.valueOf(value));
            return this;
        }
    }
}
