package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration for deterministic and optional provider-assisted natural-language GUI actions.
 */
@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/custom.properties",
        "file:src/main/resources/properties/default/custom.properties",
        "classpath:custom.properties"})
public interface NaturalActions extends EngineProperties<NaturalActions> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.naturalActionsOverride.set(ConfigFactory.create(
                NaturalActions.class,
                ThreadLocalPropertiesManager.getOverrides()));
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    /** @return whether natural-language actions are enabled */
    @Key("naturalActions.enabled")
    @DefaultValue("false")
    boolean enabled();

    /** @return minimum accepted trust percentage from 0 to 100 */
    @Key("naturalActions.minimumTrustPercentage")
    @DefaultValue("85")
    int minimumTrustPercentage();

    /** @return selected planner identifier, such as deterministic, auto, or a ServiceLoader planner id */
    @Key("naturalActions.planner")
    @DefaultValue("deterministic")
    String planner();

    /** @return whether optional AI/provider fallback planners may be used */
    @Key("naturalActions.aiFallback.enabled")
    @DefaultValue("false")
    boolean aiFallbackEnabled();

    /** @return comma-separated action target categories allowed for execution */
    @Key("naturalActions.allowedActions")
    @DefaultValue("browser,element,touch")
    String allowedActions();

    /**
     * Returns a fluent builder for current-thread overrides.
     *
     * @return current-thread property builder
     */
    default SetProperty set() {
        return new SetProperty();
    }

    /**
     * Fluent current-thread overrides for natural-language actions.
     */
    class SetProperty implements EngineProperties.SetProperty {
        /** @param value enabled state @return this builder */
        public SetProperty enabled(boolean value) {
            setProperty("naturalActions.enabled", String.valueOf(value));
            return this;
        }

        /** @param value minimum trust percentage from 0 to 100 @return this builder */
        public SetProperty minimumTrustPercentage(int value) {
            setProperty("naturalActions.minimumTrustPercentage", String.valueOf(value));
            return this;
        }

        /** @param value planner identifier @return this builder */
        public SetProperty planner(String value) {
            setProperty("naturalActions.planner", value);
            return this;
        }

        /** @param value AI fallback enabled state @return this builder */
        public SetProperty aiFallbackEnabled(boolean value) {
            setProperty("naturalActions.aiFallback.enabled", String.valueOf(value));
            return this;
        }

        /** @param value comma-separated allowed target categories @return this builder */
        public SetProperty allowedActions(String value) {
            setProperty("naturalActions.allowedActions", value);
            return this;
        }
    }
}
