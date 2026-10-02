package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;

/**
 * Configuration properties interface for TestNG execution settings in the SHAFT framework.
 * Controls parallel execution mode, thread count, and suite-level configuration.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.testNG.set().parallel("methods").threadCount(4);
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/TestNG.properties", "file:src/main/resources/properties/default/TestNG.properties", "classpath:TestNG.properties",})
public interface TestNG extends EngineProperties<TestNG> {

    /**
     * TestNG parallel execution mode (maps to the generated suite's parallel attribute).
     *
     * <p>Default: {@code NONE}. Possible values: METHODS, CLASSES, TESTS, INSTANCES.
     *
     * @return the configured value of {@code setParallel}
     */
    @Key("setParallel")
    @DefaultValue("NONE")
    String parallel();

    /**
     * Selects how setThreadCount is applied: STATIC uses it as-is, DYNAMIC multiplies it by the
     * available processor cores.
     *
     * <p>Default: {@code STATIC}. Possible values: STATIC, DYNAMIC.
     *
     * @return the configured value of {@code setParallelMode}
     */
    @Key("setParallelMode")
    @DefaultValue("STATIC")
    String parallelMode();

    /**
     * ThreadCount is used as-is in case of STATIC mode. Total ThreadCount is automatically calculated
     * for DYNAMIC mode; (Total ThreadCount = Number of available processor cores * setThreadCount).
     *
     * <p>Default: {@code 1.0d}.
     *
     * @return the configured value of {@code setThreadCount}
     */
    @Key("setThreadCount")
    @DefaultValue("1.0d")
    double threadCount();

    /**
     * TestNG verbose logging level for the generated suite (0 = silent).
     *
     * <p>Default: {@code 0}.
     *
     * @return the configured value of {@code setVerbose}
     */
    @Key("setVerbose")
    @DefaultValue("0")
    Integer verbose();

    /**
     * Preserve declaration order of test methods/classes instead of TestNG's default ordering.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code setPreserveOrder}
     */
    @Key("setPreserveOrder")
    @DefaultValue("false")
    boolean preserveOrder();

    /**
     * Group parallel test methods by instance instead of running them independently.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code setGroupByInstances}
     */
    @Key("setGroupByInstances")
    @DefaultValue("false")
    boolean groupByInstances();

    /**
     * Number of threads TestNG uses to run {@literal @}DataProvider-fed test invocations in parallel.
     *
     * <p>Default: {@code 1}.
     *
     * @return the configured value of {@code setDataProviderThreadCount}
     */
    @Key("setDataProviderThreadCount")
    @DefaultValue("1")
    int dataProviderThreadCount();

    /**
     * Test Suite Timeout in Minutes
     * Default is 1440 minutes == 24 hours
     */
    @Key("testSuiteTimeout")
    @DefaultValue("1440")
    long testSuiteTimeout();

}
