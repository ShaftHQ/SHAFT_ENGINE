package com.shaft.gui.internal.healing;

import com.shaft.driver.SHAFT;

import java.util.Locale;

/**
 * Effective element recovery strategy.
 */
public enum HealingStrategy {
    DISABLED(false, false),
    HEALENIUM(true, false),
    SHAFT_HEAL(false, true),
    COMPOSITE(true, true);

    private final boolean usesHealenium;
    private final boolean usesShaftHeal;

    HealingStrategy(boolean usesHealenium, boolean usesShaftHeal) {
        this.usesHealenium = usesHealenium;
        this.usesShaftHeal = usesShaftHeal;
    }

    /**
     * Resolves the current strategy while preserving the legacy
     * {@code heal-enabled=true} Healenium configuration.
     *
     * @return effective strategy
     */
    public static HealingStrategy current() {
        HealingStrategy configured = parse(SHAFT.Properties.healing.strategy());
        if (configured == DISABLED && SHAFT.Properties.healenium.healEnabled()) {
            return HEALENIUM;
        }
        return configured;
    }

    /**
     * Parses a configured strategy.
     *
     * @param value configured value
     * @return normalized strategy, or {@link #DISABLED} for unknown values
     */
    public static HealingStrategy parse(String value) {
        String normalized = value == null ? "" : value.trim()
                .replace('_', '-')
                .toLowerCase(Locale.ROOT);
        return switch (normalized) {
            case "healenium" -> HEALENIUM;
            case "shaft-heal", "shaftheal" -> SHAFT_HEAL;
            case "composite" -> COMPOSITE;
            default -> DISABLED;
        };
    }

    /**
     * @return whether Healenium should wrap the driver
     */
    public boolean usesHealenium() {
        return usesHealenium;
    }

    /**
     * @return whether the optional SHAFT Heal provider should be called
     */
    public boolean usesShaftHeal() {
        return usesShaftHeal;
    }
}
