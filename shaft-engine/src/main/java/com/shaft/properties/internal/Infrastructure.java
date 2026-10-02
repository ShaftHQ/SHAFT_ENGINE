package com.shaft.properties.internal;

import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupProfile;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/** Configuration for the shared batteries-included setup subsystem. */
@Sources({"system:properties", "file:src/main/resources/properties/custom.properties",
        "file:src/main/resources/properties/default/custom.properties", "classpath:custom.properties"})
public interface Infrastructure extends EngineProperties<Infrastructure> {
    /**
     * Selects setup ownership; EXTERNAL is non-mutating.
     *
     * <p>Default: {@code EXTERNAL}. Possible values: EXTERNAL, MANAGED, HYBRID.
     *
     * @return the configured value of {@code infrastructure.mode}
     */
    @Key("infrastructure.mode")
    @DefaultValue("EXTERNAL")
    SetupMode mode();

    /**
     * Selects the profile used by SHAFT.Infrastructure.
     *
     * <p>Default: {@code REPORTING}. Possible values: setup profile name.
     *
     * @return the configured value of {@code infrastructure.profile}
     */
    @Key("infrastructure.profile")
    @DefaultValue("REPORTING")
    SetupProfile profile();

    /**
     * Overrides SHAFT-owned setup storage when non-empty.
     *
     * <p>Possible values: absolute path.
     *
     * @return the configured value of {@code infrastructure.cacheDirectory}
     */
    @Key("infrastructure.cacheDirectory")
    @DefaultValue("")
    String cacheDirectory();

    /**
     * Requires verified cached artifacts and disables network access.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code infrastructure.offline}
     */
    @Key("infrastructure.offline")
    @DefaultValue("false")
    boolean offline();

    /**
     * Requests startup for providers that own a service.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code infrastructure.autoStart}
     */
    @Key("infrastructure.autoStart")
    @DefaultValue("false")
    boolean autoStart();

    /**
     * Allows supporting providers to prefer compatible host tools.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code infrastructure.preferSystemTools}
     */
    @Key("infrastructure.preferSystemTools")
    @DefaultValue("true")
    boolean preferSystemTools();

    /**
     * Allows supporting providers to reuse compatible SHAFT-owned processes.
     *
     * <p>Default: {@code true}. Possible values: true, false.
     *
     * @return the configured value of {@code infrastructure.reuseOwnedProcesses}
     */
    @Key("infrastructure.reuseOwnedProcesses")
    @DefaultValue("true")
    boolean reuseOwnedProcesses();

    /**
     * Sets the startup timeout for supporting lifecycle providers.
     *
     * <p>Default: {@code PT2M}. Possible values: positive ISO-8601 duration.
     *
     * @return the configured value of {@code infrastructure.startupTimeout}
     */
    @Key("infrastructure.startupTimeout")
    @DefaultValue("PT2M")
    String startupTimeout();

    /**
     * Sets the shutdown timeout for supporting lifecycle providers.
     *
     * <p>Default: {@code PT30S}. Possible values: positive ISO-8601 duration.
     *
     * @return the configured value of {@code infrastructure.shutdownTimeout}
     */
    @Key("infrastructure.shutdownTimeout")
    @DefaultValue("PT30S")
    String shutdownTimeout();

    @Override
    default InfrastructurePropertyBuilder set() { return new InfrastructurePropertyBuilder(); }

    /** Thread-local fluent overrides for setup policy. */
    final class InfrastructurePropertyBuilder implements EngineProperties.SetProperty {
        /**
         * Overrides the {@code infrastructure.mode} property at runtime. Selects setup ownership; EXTERNAL
         * is non-mutating.
         *
         * @param value the new value of {@code infrastructure.mode}
         * @return this {@link InfrastructurePropertyBuilder} instance for chaining
         */
        public InfrastructurePropertyBuilder mode(SetupMode value) {
            return set("infrastructure.mode", value.name());
        }

        /**
         * Overrides the {@code infrastructure.profile} property at runtime. Selects the profile used by
         * SHAFT.Infrastructure.
         *
         * @param value the new value of {@code infrastructure.profile}
         * @return this {@link InfrastructurePropertyBuilder} instance for chaining
         */
        public InfrastructurePropertyBuilder profile(SetupProfile value) {
            return set("infrastructure.profile", value.name());
        }

        /**
         * Overrides the {@code infrastructure.cacheDirectory} property at runtime. Overrides SHAFT-owned
         * setup storage when non-empty.
         *
         * @param value the new value of {@code infrastructure.cacheDirectory}
         * @return this {@link InfrastructurePropertyBuilder} instance for chaining
         */
        public InfrastructurePropertyBuilder cacheDirectory(String value) {
            return set("infrastructure.cacheDirectory", value);
        }

        /**
         * Overrides the {@code infrastructure.offline} property at runtime. Requires verified cached
         * artifacts and disables network access.
         *
         * @param value the new value of {@code infrastructure.offline}
         * @return this {@link InfrastructurePropertyBuilder} instance for chaining
         */
        public InfrastructurePropertyBuilder offline(boolean value) {
            return set("infrastructure.offline", value);
        }

        /**
         * Overrides the {@code infrastructure.autoStart} property at runtime. Requests startup for
         * providers that own a service.
         *
         * @param value the new value of {@code infrastructure.autoStart}
         * @return this {@link InfrastructurePropertyBuilder} instance for chaining
         */
        public InfrastructurePropertyBuilder autoStart(boolean value) {
            return set("infrastructure.autoStart", value);
        }

        /**
         * Overrides the {@code infrastructure.preferSystemTools} property at runtime. Allows supporting
         * providers to prefer compatible host tools.
         *
         * @param value the new value of {@code infrastructure.preferSystemTools}
         * @return this {@link InfrastructurePropertyBuilder} instance for chaining
         */
        public InfrastructurePropertyBuilder preferSystemTools(boolean value) {
            return set("infrastructure.preferSystemTools", value);
        }

        /**
         * Overrides the {@code infrastructure.reuseOwnedProcesses} property at runtime. Allows supporting
         * providers to reuse compatible SHAFT-owned processes.
         *
         * @param value the new value of {@code infrastructure.reuseOwnedProcesses}
         * @return this {@link InfrastructurePropertyBuilder} instance for chaining
         */
        public InfrastructurePropertyBuilder reuseOwnedProcesses(boolean value) {
            return set("infrastructure.reuseOwnedProcesses", value);
        }

        /**
         * Overrides the {@code infrastructure.startupTimeout} property at runtime. Sets the startup
         * timeout for supporting lifecycle providers.
         *
         * @param value the new value of {@code infrastructure.startupTimeout}
         * @return this {@link InfrastructurePropertyBuilder} instance for chaining
         */
        public InfrastructurePropertyBuilder startupTimeout(String value) {
            return set("infrastructure.startupTimeout", value);
        }

        /**
         * Overrides the {@code infrastructure.shutdownTimeout} property at runtime. Sets the shutdown
         * timeout for supporting lifecycle providers.
         *
         * @param value the new value of {@code infrastructure.shutdownTimeout}
         * @return this {@link InfrastructurePropertyBuilder} instance for chaining
         */
        public InfrastructurePropertyBuilder shutdownTimeout(String value) {
            return set("infrastructure.shutdownTimeout", value);
        }

        private InfrastructurePropertyBuilder set(String key, Object value) {
            if (value == null) throw new IllegalArgumentException(key + " must not be null.");
            ThreadLocalPropertiesManager.setProperty(key, String.valueOf(value));
            Properties.infrastructureOverride.set(ConfigFactory.create(Infrastructure.class,
                    ThreadLocalPropertiesManager.getOverrides()));
            EngineProperties.logPropertyUpdate(key, value);
            return this;
        }
    }
}
