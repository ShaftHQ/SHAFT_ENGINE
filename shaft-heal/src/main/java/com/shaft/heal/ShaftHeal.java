package com.shaft.heal;

import com.shaft.heal.model.HealingReport;

import java.util.Optional;

/**
 * Runtime access to the most recent SHAFT Heal report on the current thread.
 */
public final class ShaftHeal {
    private static final ThreadLocal<HealingReport> LAST_REPORT = new ThreadLocal<>();

    private ShaftHeal() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns the most recent current-thread healing report.
     *
     * @return optional report
     */
    public static Optional<HealingReport> lastReport() {
        return Optional.ofNullable(LAST_REPORT.get());
    }

    /**
     * Clears the current-thread report.
     */
    public static void clear() {
        LAST_REPORT.remove();
    }

    /**
     * Records the latest report.
     *
     * @param report report
     */
    public static void record(HealingReport report) {
        LAST_REPORT.set(report);
    }
}
