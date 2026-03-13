package com.shaft.properties.internal;

/**
 * Manages per-thread property overrides for SHAFT properties.
 * <p>
 * When a property is set via the {@code SHAFT.Properties} API, it is stored in a
 * thread-local map so that each test thread has its own isolated configuration.
 * This prevents cross-thread contamination during parallel test execution.
 * </p>
 * <p>
 * Call {@link #clear()} after each test class lifecycle completes to reset
 * per-thread overrides and prevent stale state when thread pools reuse threads.
 * </p>
 */
public final class ThreadLocalPropertiesManager {

    private static final ThreadLocal<java.util.Properties> threadLocalOverrides =
            ThreadLocal.withInitial(java.util.Properties::new);

    private ThreadLocalPropertiesManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Sets a property override for the current thread only.
     * Does not affect other threads or global system properties.
     *
     * @param key   the property key
     * @param value the property value
     */
    public static void setProperty(String key, String value) {
        threadLocalOverrides.get().setProperty(key, value);
    }

    /**
     * Returns the effective value for the given property key by checking the
     * current thread's overrides first and falling back to the corresponding
     * system property.  This is the single-key equivalent of
     * {@link #getEffectiveProperties()} and should be used in place of
     * {@code System.getProperty(key)} wherever SHAFT configuration values
     * are read, so that per-thread overrides are honoured.
     *
     * @param key the property key
     * @return the thread-local value if set, otherwise the system property value,
     *         or {@code null} if neither is set
     */
    public static String getProperty(String key) {
        String value = threadLocalOverrides.get().getProperty(key);
        return value != null ? value : System.getProperty(key);
    }

    /**
     * Returns the live thread-local overrides map for the current thread.
     * This map is passed to {@code ConfigFactory.create()} as the highest-priority
     * property source so that any overrides set by the current thread take precedence
     * over system properties and file-based properties.
     *
     * @return the current thread's property overrides
     */
    public static java.util.Properties getOverrides() {
        return threadLocalOverrides.get();
    }

    /**
     * Returns a merged view of system properties and thread-local overrides for the
     * current thread.  Thread-local overrides take precedence over system properties.
     * <p>
     * This method should be used wherever code previously called
     * {@code System.getProperties()} to look up SHAFT configuration values, so that
     * per-thread overrides set via the {@code SHAFT.Properties} API are visible.
     *
     * @return a new {@link java.util.Properties} instance containing all system
     *         properties with thread-local overrides applied on top
     */
    public static java.util.Properties getEffectiveProperties() {
        java.util.Properties merged = new java.util.Properties();
        merged.putAll(System.getProperties());
        merged.putAll(threadLocalOverrides.get());
        return merged;
    }

    /**
     * Clears all thread-local property overrides for the current thread.
     * Should be called at the start of each new test class lifecycle (before
     * {@code @BeforeClass} runs) to prevent stale overrides from a previously
     * executed test class on the same pooled thread.
     */
    public static void clear() {
        threadLocalOverrides.remove();
    }
}
