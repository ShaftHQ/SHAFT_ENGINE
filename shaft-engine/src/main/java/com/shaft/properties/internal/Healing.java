package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration for optional SHAFT Heal element recovery.
 *
 * <p>Healing is disabled by default. The deterministic SHAFT Heal provider is
 * supplied by the optional {@code io.github.shafthq:shaft-heal} artifact.</p>
 */
@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/custom.properties",
        "file:src/main/resources/properties/default/custom.properties",
        "classpath:custom.properties"})
public interface Healing extends EngineProperties<Healing> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.healingOverride.set(ConfigFactory.create(Healing.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    /** @return disabled, healenium, shaft-heal, or composite */
    @Key("healing.strategy")
    @DefaultValue("disabled")
    String strategy();

    /** @return minimum accepted deterministic confidence */
    @Key("healing.minimumConfidence")
    @DefaultValue("0.75")
    double minimumConfidence();

    /** @return preferred minimum accepted trust percentage, or -1 to use healing.minimumConfidence */
    @Key("healing.minimumTrustPercentage")
    @DefaultValue("-1")
    int minimumTrustPercentage();

    /** @return required score separation from the next candidate */
    @Key("healing.ambiguityMargin")
    @DefaultValue("0.10")
    double ambiguityMargin();

    /** @return comma-separated evidence categories permitted for deterministic recovery */
    @Key("healing.evidenceCategories")
    @DefaultValue("accessibility,label,test-id,stable-id-name,semantic,dom-fingerprint,native-state,ancestor-context,history")
    String evidenceCategories();

    /** @return comma-separated attributes treated as configured test IDs */
    @Key("healing.testIdAttributes")
    @DefaultValue("data-testid,data-test,data-qa")
    String testIdAttributes();

    /** @return whether bounded local history is enabled */
    @Key("healing.history.enabled")
    @DefaultValue("true")
    boolean historyEnabled();

    /** @return local history file path */
    @Key("healing.history.path")
    @DefaultValue("target/shaft-heal/history.json")
    String historyPath();

    /** @return maximum retained history records */
    @Key("healing.history.maxEntries")
    @DefaultValue("500")
    int historyMaxEntries();

    /** @return maximum history age in days */
    @Key("healing.history.retentionDays")
    @DefaultValue("30")
    int historyRetentionDays();

    /** @return whether optional visual evidence may be evaluated */
    @Key("healing.visual.enabled")
    @DefaultValue("false")
    boolean visualEnabled();

    /** @return whether optional provider reranking may be requested */
    @Key("healing.ai.enabled")
    @DefaultValue("false")
    boolean aiEnabled();

    /** @return when optional provider reranking may run: never, ambiguous, below-threshold, or always */
    @Key("healing.ai.trigger")
    @DefaultValue("ambiguous")
    String aiTrigger();

    /** @return whether reviewed source-patch proposals may be emitted */
    @Key("healing.sourcePatch.enabled")
    @DefaultValue("false")
    boolean sourcePatchEnabled();

    /**
     * @return hard total budget in seconds across the whole deterministic re-suggestion ladder,
     *         not per attempt (issue #4027). {@code 0} (the default) preserves today's one-shot
     *         {@code resolve()} behavior unchanged; only a positive value retries until the
     *         element is found or the budget is hit. Every existing healing user keeps today's
     *         single attempt unless this is explicitly raised (e.g. by SHAFT-Assistant-generated
     *         tests, which set it in their generated {@code setUp()}).
     */
    @Key("healing.ladder.budgetSeconds")
    @DefaultValue("0")
    int ladderBudgetSeconds();

    /**
     * Returns a fluent builder for current-thread overrides.
     *
     * @return current-thread property builder
     */
    default SetProperty set() {
        return new SetProperty();
    }

    /**
     * Fluent current-thread overrides for SHAFT Heal.
     */
    class SetProperty implements EngineProperties.SetProperty {
        /** @param value strategy name @return this builder */
        public SetProperty strategy(String value) {
            setProperty("healing.strategy", value);
            return this;
        }

        /** @param value minimum confidence @return this builder */
        public SetProperty minimumConfidence(double value) {
            setProperty("healing.minimumConfidence", String.valueOf(value));
            return this;
        }

        /** @param value minimum trust percentage from 0 to 100 @return this builder */
        public SetProperty minimumTrustPercentage(int value) {
            setProperty("healing.minimumTrustPercentage", String.valueOf(value));
            return this;
        }

        /** @param value ambiguity margin @return this builder */
        public SetProperty ambiguityMargin(double value) {
            setProperty("healing.ambiguityMargin", String.valueOf(value));
            return this;
        }

        /** @param value evidence category list @return this builder */
        public SetProperty evidenceCategories(String value) {
            setProperty("healing.evidenceCategories", value);
            return this;
        }

        /** @param value test ID attribute list @return this builder */
        public SetProperty testIdAttributes(String value) {
            setProperty("healing.testIdAttributes", value);
            return this;
        }

        /** @param value history enabled state @return this builder */
        public SetProperty historyEnabled(boolean value) {
            setProperty("healing.history.enabled", String.valueOf(value));
            return this;
        }

        /** @param value history path @return this builder */
        public SetProperty historyPath(String value) {
            setProperty("healing.history.path", value);
            return this;
        }

        /** @param value maximum entries @return this builder */
        public SetProperty historyMaxEntries(int value) {
            setProperty("healing.history.maxEntries", String.valueOf(value));
            return this;
        }

        /** @param value retention days @return this builder */
        public SetProperty historyRetentionDays(int value) {
            setProperty("healing.history.retentionDays", String.valueOf(value));
            return this;
        }

        /** @param value visual evidence enabled state @return this builder */
        public SetProperty visualEnabled(boolean value) {
            setProperty("healing.visual.enabled", String.valueOf(value));
            return this;
        }

        /** @param value AI reranking enabled state @return this builder */
        public SetProperty aiEnabled(boolean value) {
            setProperty("healing.ai.enabled", String.valueOf(value));
            return this;
        }

        /** @param value AI trigger policy @return this builder */
        public SetProperty aiTrigger(String value) {
            setProperty("healing.ai.trigger", value);
            return this;
        }

        /** @param value source-patch proposal enabled state @return this builder */
        public SetProperty sourcePatchEnabled(boolean value) {
            setProperty("healing.sourcePatch.enabled", String.valueOf(value));
            return this;
        }

        /** @param value hard total ladder budget in seconds (0 preserves today's one-shot behavior) @return this builder */
        public SetProperty ladderBudgetSeconds(int value) {
            setProperty("healing.ladder.budgetSeconds", String.valueOf(value));
            return this;
        }
    }
}
