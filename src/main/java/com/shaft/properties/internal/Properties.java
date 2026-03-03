package com.shaft.properties.internal;

import org.aeonbits.owner.ConfigFactory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Proxy;

/**
 * Central holder for all SHAFT property configuration objects.
 * <p>
 * Each writable property type is exposed as a thread-safe proxy that dispatches
 * reads to the current thread's config instance (if a per-thread override is active)
 * or falls back to the globally-initialized base config.  When a test calls
 * {@code SHAFT.Properties.web.set().targetBrowserName("firefox")}, only that
 * thread's config is updated — other threads continue to see their own values,
 * preventing cross-thread contamination during parallel test execution.
 * </p>
 * <p>
 * Read-only property types ({@link Internal}, {@link TestNG}, {@link Log4j},
 * {@link Cucumber}) are backed by simple static fields because they are never
 * mutated after framework initialisation.
 * </p>
 */
public class Properties {

    // -------------------------------------------------------------------------
    // Base configs – set once during framework initialisation by PropertiesHelper.
    // -------------------------------------------------------------------------
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
    static final ThreadLocal<Flags> flagsOverride = new ThreadLocal<>();
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
    public static final Flags flags = createProxy(Flags.class, flagsOverride, () -> baseFlags);
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
     * globally-initialised base instance.  This makes the public static field
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
     * Returns {@code true} if the framework property system has been fully initialised
     * (i.e. {@link PropertiesHelper#initialize()} or equivalent has been called).
     * Use this instead of {@code Properties.xxx == null} null-checks because the public
     * fields are now always-non-null proxy objects.
     *
     * @return {@code true} once base configs have been loaded
     */
    public static boolean isInitialized() {
        return basePlatform != null;
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
        flagsOverride.remove();
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
