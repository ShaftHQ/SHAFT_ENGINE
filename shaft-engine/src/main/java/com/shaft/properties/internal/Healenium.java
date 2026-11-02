package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for Healenium self-healing locator support in the SHAFT framework.
 * Controls whether Healenium is enabled and its recovery score threshold.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.healenium.set().healEnabled(true).recoveryScore(0.6);
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/healenium.properties", "file:src/main/resources/properties/default/healenium.properties", "classpath:healenium.properties",})
public interface Healenium extends EngineProperties<Healenium> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.healeniumOverride.set(ConfigFactory.create(Healenium.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * Number of Healenium self-healing recovery attempts before giving up on a broken locator.
     *
     * <p>Default: {@code 1}. Possible values: positive integer.
     *
     * @return the configured value of {@code recovery-tries}
     */
    @Key("recovery-tries")
    @DefaultValue("1")
    int recoveryTries();

    /**
     * Minimum Healenium similarity score required to accept a healed locator candidate.
     *
     * <p>Default: {@code 0.5}. Possible values: 0.0 to 1.0.
     *
     * @return the configured value of {@code score-cap}
     */
    @Key("score-cap")
    @DefaultValue("0.5")
    String scoreCap();

    /**
     * Enable the Healenium self-healing proxy for this run.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code heal-enabled}
     */
    @Key("heal-enabled")
    @DefaultValue("false")
    boolean healEnabled();

    /**
     * Hostname of the Healenium server that scores locator-healing candidates.
     *
     * <p>Default: {@code localhost}. Possible values: hostname.
     *
     * @return the configured value of {@code serverHost}
     */
    @Key("serverHost")
    @DefaultValue("localhost")
    String serverHost();

    /**
     * Port of the Healenium server that scores locator-healing candidates.
     *
     * <p>Default: {@code 7878}. Possible values: port number.
     *
     * @return the configured value of {@code serverPort}
     */
    @Key("serverPort")
    @DefaultValue("7878")
    int serverPort();

    /**
     * Local port SHAFT uses so Healenium can intercept browser traffic.
     *
     * <p>Default: {@code 8000}. Possible values: port number.
     *
     * @return the configured value of {@code imitatePort}
     */
    @Key("imitatePort")
    @DefaultValue("8000")
    int imitatePort();

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
         * Overrides the {@code recovery-tries} property at runtime. Number of Healenium self-healing
         * recovery attempts before giving up on a broken locator.
         *
         * @param value the new value of {@code recovery-tries}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty recoveryTries(int value) {
            setProperty("recovery-tries", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code score-cap} property at runtime. Minimum Healenium similarity score required
         * to accept a healed locator candidate.
         *
         * @param value the new value of {@code score-cap}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty scoreCap(String value) {
            setProperty("score-cap", value);
            return this;
        }

        /**
         * Overrides the {@code heal-enabled} property at runtime. Enable the Healenium self-healing proxy
         * for this run.
         *
         * @param value the new value of {@code heal-enabled}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty healEnabled(boolean value) {
            setProperty("heal-enabled", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code serverHost} property at runtime. Hostname of the Healenium server that
         * scores locator-healing candidates.
         *
         * @param value the new value of {@code serverHost}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty serverHost(String value) {
            setProperty("serverHost", value);
            return this;
        }

        /**
         * Overrides the {@code serverPort} property at runtime. Port of the Healenium server that scores
         * locator-healing candidates.
         *
         * @param value the new value of {@code serverPort}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty serverPort(int value) {
            setProperty("serverPort", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code imitatePort} property at runtime. Local port SHAFT uses so Healenium can
         * intercept browser traffic.
         *
         * @param value the new value of {@code imitatePort}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty imitatePort(int value) {
            setProperty("imitatePort", String.valueOf(value));
            return this;
        }

    }

}
