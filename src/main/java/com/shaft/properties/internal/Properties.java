package com.shaft.properties.internal;

import org.aeonbits.owner.ConfigFactory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Proxy;

/**
 * Central holder for all SHAFT property configuration objects.
 * <p>
 * Mutable driver/session configuration is exposed through thread-safe proxies that
 * dispatch reads to the current thread's config instance (if a per-thread override
 * is active) or fall back to the globally-initialized base config. When a test calls
 * {@code SHAFT.Properties.web.set().targetBrowserName("firefox")}, only that
 * thread's config is updated — other threads continue to see their own values,
 * preventing cross-thread contamination during parallel test execution.
 * </p>
 * <p>
 * Engine-wide configuration ({@link Flags}, plus the read-only {@link Internal},
 * {@link TestNG}, {@link Log4j}, and {@link Cucumber} types) is backed by a
 * single global config instance because these values are initialized once and
 * must remain consistent across all execution threads.
 * </p>
 */
public class Properties {

    // -------------------------------------------------------------------------
    // Base configs – set once during framework initialization by PropertiesHelper.
    // -------------------------------------------------------------------------
    /** Set to {@code true} only after {@code loadProperties()} completes successfully. */
    static volatile boolean initialized = false;
    static BrowserStack baseBrowserStack;
    static Platform basePlatform;
    static Healenium baseHealenium;
    static Jira baseJira;
    static Mobile baseMobile;
    static Paths basePaths;
    static Pattern basePattern;
    static Flags baseFlags;
    static Reporting baseReporting;
    static Allure baseAllure;
    static Timeouts baseTimeouts;
    static Tinkey baseTinkey;
    static Visuals baseVisuals;
    static Web baseWeb;
    static Performance basePerformance;
    static LambdaTest baseLambdaTest;
    static API baseApi;

    // -------------------------------------------------------------------------
    // Thread-local override configs – updated by set() calls on each thread.
    // -------------------------------------------------------------------------
    static final ThreadLocal<BrowserStack> browserStackOverride = new ThreadLocal<>();
    static final ThreadLocal<Platform> platformOverride = new ThreadLocal<>();
    static final ThreadLocal<Healenium> healeniumOverride = new ThreadLocal<>();
    static final ThreadLocal<Jira> jiraOverride = new ThreadLocal<>();
    static final ThreadLocal<Mobile> mobileOverride = new ThreadLocal<>();
    static final ThreadLocal<Paths> pathsOverride = new ThreadLocal<>();
    static final ThreadLocal<Pattern> patternOverride = new ThreadLocal<>();
    static final ThreadLocal<Reporting> reportingOverride = new ThreadLocal<>();
    static final ThreadLocal<Allure> allureOverride = new ThreadLocal<>();
    static final ThreadLocal<Timeouts> timeoutsOverride = new ThreadLocal<>();
    static final ThreadLocal<Tinkey> tinkeyOverride = new ThreadLocal<>();
    static final ThreadLocal<Visuals> visualsOverride = new ThreadLocal<>();
    static final ThreadLocal<Web> webOverride = new ThreadLocal<>();
    static final ThreadLocal<Performance> performanceOverride = new ThreadLocal<>();
    static final ThreadLocal<LambdaTest> lambdaTestOverride = new ThreadLocal<>();
    static final ThreadLocal<API> apiOverride = new ThreadLocal<>();

    // -------------------------------------------------------------------------
    // Public proxy fields – fully API-compatible with the previous static fields.
    // -------------------------------------------------------------------------
    public static final BrowserStack browserStack = createProxy(BrowserStack.class, browserStackOverride, () -> baseBrowserStack);
    public static final Platform platform = createProxy(Platform.class, platformOverride, () -> basePlatform);
    public static final Healenium healenium = createProxy(Healenium.class, healeniumOverride, () -> baseHealenium);
    public static final Jira jira = createProxy(Jira.class, jiraOverride, () -> baseJira);
    public static final Mobile mobile = createProxy(Mobile.class, mobileOverride, () -> baseMobile);
    public static final Paths paths = createProxy(Paths.class, pathsOverride, () -> basePaths);
    public static final Pattern pattern = createProxy(Pattern.class, patternOverride, () -> basePattern);
    public static final Flags flags = createGlobalProxy(Flags.class, () -> baseFlags);
    public static final Reporting reporting = createProxy(Reporting.class, reportingOverride, () -> baseReporting);
    public static final Allure allure = createProxy(Allure.class, allureOverride, () -> baseAllure);
    public static final Timeouts timeouts = createProxy(Timeouts.class, timeoutsOverride, () -> baseTimeouts);
    public static final Tinkey tinkey = createProxy(Tinkey.class, tinkeyOverride, () -> baseTinkey);
    public static final Visuals visuals = createProxy(Visuals.class, visualsOverride, () -> baseVisuals);
    public static final Web web = createProxy(Web.class, webOverride, () -> baseWeb);
    public static final Performance performance = createProxy(Performance.class, performanceOverride, () -> basePerformance);
    public static final LambdaTest lambdaTest = createProxy(LambdaTest.class, lambdaTestOverride, () -> baseLambdaTest);
    public static final API api = createProxy(API.class, apiOverride, () -> baseApi);

    // Read-only properties – not mutable after initialisation, no proxy needed.
    public static Internal internal;
    public static TestNG testNG;
    public static Log4j log4j;
    public static Cucumber cucumber;

    // -------------------------------------------------------------------------
    // Helper – create a thread-dispatching proxy for a config interface.
    // -------------------------------------------------------------------------

    /**
     * Creates a dynamic proxy for the given config interface that dispatches every
     * method call to the current thread's override instance (if present) or the
     * globally-initialized base instance.  This makes the public static field
     * behave as a thread-local value while keeping the familiar field-access API.
     *
     * @param <T>          the config interface type
     * @param configClass  the config interface class
     * @param override     the {@link ThreadLocal} holding per-thread override instances
     * @param baseSupplier a supplier for the global base instance
     * @return a proxy that implements {@code T}
     */
    @SuppressWarnings("unchecked")
    static <T extends EngineProperties<T>> T createProxy(
            Class<T> configClass,
            ThreadLocal<T> override,
            java.util.function.Supplier<T> baseSupplier) {
        return (T) Proxy.newProxyInstance(
                configClass.getClassLoader(),
                new Class<?>[]{configClass},
                (proxy, method, args) -> {
                    T instance = override.get();
                    if (instance == null) {
                        instance = baseSupplier.get();
                    }
                    if (instance == null) {
                        // Fallback: lazily create from current system properties + thread-local overrides.
                        instance = ConfigFactory.create(configClass, ThreadLocalPropertiesManager.getOverrides());
                    }
                    try {
                        return method.invoke(instance, args);
                    } catch (InvocationTargetException e) {
                        throw e.getCause() != null ? e.getCause() : e;
                    }
                 });
    }

    /**
     * Creates a dynamic proxy for engine-global configuration that always resolves
     * against the globally initialized base instance.
     *
     * @param <T>          the config interface type
     * @param configClass  the config interface class
     * @param baseSupplier a supplier for the global base instance
     * @return a proxy that implements {@code T}
     */
    @SuppressWarnings("unchecked")
    static <T extends EngineProperties<T>> T createGlobalProxy(
            Class<T> configClass,
            java.util.function.Supplier<T> baseSupplier) {
        return (T) Proxy.newProxyInstance(
                configClass.getClassLoader(),
                new Class<?>[]{configClass},
                (proxy, method, args) -> {
                    T instance = baseSupplier.get();
                    if (instance == null) {
                        instance = ConfigFactory.create(configClass);
                    }
                    try {
                        return method.invoke(instance, args);
                    } catch (InvocationTargetException e) {
                        throw e.getCause() != null ? e.getCause() : e;
                    }
                });
    }

    /**
     * Returns {@code true} if the framework property system has been fully initialized
     * (i.e. {@link PropertiesHelper#initialize()} or equivalent has been called).
     * Use this instead of {@code Properties.xxx == null} null-checks because the public
     * fields are now always-non-null proxy objects.
     * <p>
     * Uses a dedicated volatile flag rather than a single base-field null check so that
     * partial initialization (an exception partway through {@code loadProperties()}) cannot
     * produce a false positive.
     * </p>
     *
     * @return {@code true} once base configs have been fully loaded
     */
    public static boolean isInitialized() {
        return initialized;
    }

    /**
     * Clears all thread-local property overrides for the current thread and resets
     * the per-thread override map maintained by {@link ThreadLocalPropertiesManager}.
     * <p>
     * This should be called before a new test-class lifecycle begins on a pooled
     * thread so that overrides set by a previous test class do not leak into the
     * next one.
     * </p>
     */
    public static void clearForCurrentThread() {
        ThreadLocalPropertiesManager.clear();
        browserStackOverride.remove();
        platformOverride.remove();
        healeniumOverride.remove();
        jiraOverride.remove();
        mobileOverride.remove();
        pathsOverride.remove();
        patternOverride.remove();
        reportingOverride.remove();
        allureOverride.remove();
        timeoutsOverride.remove();
        tinkeyOverride.remove();
        visualsOverride.remove();
        webOverride.remove();
        performanceOverride.remove();
        lambdaTestOverride.remove();
        apiOverride.remove();
    }
}
