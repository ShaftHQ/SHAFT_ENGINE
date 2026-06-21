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

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty browserName(String value) {
            setProperty("playwright.browserName", value);
            return this;
        }

        public SetProperty connectionMode(String value) {
            setProperty("playwright.connectionMode", value);
            return this;
        }

        public SetProperty endpoint(String value) {
            setProperty("playwright.endpoint", value);
            return this;
        }

        public SetProperty channel(String value) {
            setProperty("playwright.channel", value);
            return this;
        }

        public SetProperty slowMo(int value) {
            setProperty("playwright.slowMo", String.valueOf(value));
            return this;
        }

        public SetProperty launchTimeoutMilliseconds(int value) {
            setProperty("playwright.launchTimeoutMilliseconds", String.valueOf(value));
            return this;
        }

        public SetProperty defaultTimeoutMilliseconds(int value) {
            setProperty("playwright.defaultTimeoutMilliseconds", String.valueOf(value));
            return this;
        }

        public SetProperty navigationTimeoutMilliseconds(int value) {
            setProperty("playwright.navigationTimeoutMilliseconds", String.valueOf(value));
            return this;
        }

        public SetProperty artifactsDirectory(String value) {
            setProperty("playwright.artifactsDirectory", value);
            return this;
        }

        public SetProperty downloadsDirectory(String value) {
            setProperty("playwright.downloadsDirectory", value);
            return this;
        }

        public SetProperty acceptDownloads(boolean value) {
            setProperty("playwright.acceptDownloads", String.valueOf(value));
            return this;
        }

        public SetProperty tracingEnabled(boolean value) {
            setProperty("playwright.tracing.enabled", String.valueOf(value));
            return this;
        }

        public SetProperty tracingOnRetryOnly(boolean value) {
            setProperty("playwright.tracing.onRetryOnly", String.valueOf(value));
            return this;
        }

        public SetProperty tracingScreenshots(boolean value) {
            setProperty("playwright.tracing.screenshots", String.valueOf(value));
            return this;
        }

        public SetProperty tracingSnapshots(boolean value) {
            setProperty("playwright.tracing.snapshots", String.valueOf(value));
            return this;
        }

        public SetProperty tracingSources(boolean value) {
            setProperty("playwright.tracing.sources", String.valueOf(value));
            return this;
        }
    }
}
