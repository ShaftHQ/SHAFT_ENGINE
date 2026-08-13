package com.shaft.properties.internal;

import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupProfile;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/** Configuration for the shared batteries-included setup subsystem. */
@Sources({"system:properties", "file:src/main/resources/properties/custom.properties",
        "file:src/main/resources/properties/default/custom.properties", "classpath:custom.properties"})
public interface Infrastructure extends EngineProperties<Infrastructure> {
    @Key("infrastructure.mode") @DefaultValue("EXTERNAL") SetupMode mode();
    @Key("infrastructure.profile") @DefaultValue("REPORTING") SetupProfile profile();
    @Key("infrastructure.cacheDirectory") @DefaultValue("") String cacheDirectory();
    @Key("infrastructure.offline") @DefaultValue("false") boolean offline();
    @Key("infrastructure.autoStart") @DefaultValue("false") boolean autoStart();
    @Key("infrastructure.preferSystemTools") @DefaultValue("true") boolean preferSystemTools();
    @Key("infrastructure.reuseOwnedProcesses") @DefaultValue("true") boolean reuseOwnedProcesses();
    @Key("infrastructure.startupTimeout") @DefaultValue("PT2M") String startupTimeout();
    @Key("infrastructure.shutdownTimeout") @DefaultValue("PT30S") String shutdownTimeout();

    @Override
    default InfrastructurePropertyBuilder set() { return new InfrastructurePropertyBuilder(); }

    /** Thread-local fluent overrides for setup policy. */
    final class InfrastructurePropertyBuilder implements EngineProperties.SetProperty {
        public InfrastructurePropertyBuilder mode(SetupMode value) { return set("infrastructure.mode", value.name()); }
        public InfrastructurePropertyBuilder profile(SetupProfile value) { return set("infrastructure.profile", value.name()); }
        public InfrastructurePropertyBuilder cacheDirectory(String value) { return set("infrastructure.cacheDirectory", value); }
        public InfrastructurePropertyBuilder offline(boolean value) { return set("infrastructure.offline", value); }
        public InfrastructurePropertyBuilder autoStart(boolean value) { return set("infrastructure.autoStart", value); }
        public InfrastructurePropertyBuilder preferSystemTools(boolean value) { return set("infrastructure.preferSystemTools", value); }
        public InfrastructurePropertyBuilder reuseOwnedProcesses(boolean value) { return set("infrastructure.reuseOwnedProcesses", value); }
        public InfrastructurePropertyBuilder startupTimeout(String value) { return set("infrastructure.startupTimeout", value); }
        public InfrastructurePropertyBuilder shutdownTimeout(String value) { return set("infrastructure.shutdownTimeout", value); }

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
