package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/** Configuration for the optional SHAFT-managed local inference runtime. */
@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/custom.properties",
        "file:src/main/resources/properties/default/custom.properties",
        "classpath:custom.properties"})
public interface ManagedLocalAi extends EngineProperties<ManagedLocalAi> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.managedLocalAiOverride.set(ConfigFactory.create(
                ManagedLocalAi.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    /** @return whether SHAFT-managed local inference is enabled for the current thread */
    @Key("managedLocalAi.enabled")
    @DefaultValue("false")
    boolean enabled();

    /** @return whether an approved request may provision missing reviewed artifacts */
    @Key("managedLocalAi.transparentProvisioning")
    @DefaultValue("true")
    boolean transparentProvisioning();

    /** @return reviewed model identifier or {@code auto} */
    @Key("managedLocalAi.model")
    @DefaultValue("auto")
    String model();

    /** @return explicit cache directory, or blank for the SHAFT per-user cache */
    @Key("managedLocalAi.cacheDirectory")
    @DefaultValue("")
    String cacheDirectory();

    /** @return artifact download timeout in seconds */
    @Key("managedLocalAi.downloadTimeoutSeconds")
    @DefaultValue("900")
    int downloadTimeoutSeconds();

    /** @return cross-process cache lock timeout in seconds */
    @Key("managedLocalAi.lockTimeoutSeconds")
    @DefaultValue("30")
    int lockTimeoutSeconds();

    /** @return authenticated runtime launch timeout in seconds */
    @Key("managedLocalAi.launchTimeoutSeconds")
    @DefaultValue("120")
    int launchTimeoutSeconds();

    /** @return a current-thread property setter */
    default SetProperty set() {
        return new SetProperty();
    }

    /** Current-thread managed local AI property updates. */
    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty enabled(boolean value) {
            setProperty("managedLocalAi.enabled", Boolean.toString(value));
            return this;
        }

        public SetProperty transparentProvisioning(boolean value) {
            setProperty("managedLocalAi.transparentProvisioning", Boolean.toString(value));
            return this;
        }

        public SetProperty model(String value) {
            setProperty("managedLocalAi.model", value);
            return this;
        }

        public SetProperty cacheDirectory(String value) {
            setProperty("managedLocalAi.cacheDirectory", value);
            return this;
        }

        public SetProperty downloadTimeoutSeconds(int value) {
            setProperty("managedLocalAi.downloadTimeoutSeconds", Integer.toString(value));
            return this;
        }

        public SetProperty lockTimeoutSeconds(int value) {
            setProperty("managedLocalAi.lockTimeoutSeconds", Integer.toString(value));
            return this;
        }

        public SetProperty launchTimeoutSeconds(int value) {
            setProperty("managedLocalAi.launchTimeoutSeconds", Integer.toString(value));
            return this;
        }
    }
}
