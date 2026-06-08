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

    @Key("setParallel")
    @DefaultValue("NONE")
    String parallel();

    @Key("setParallelMode")
    @DefaultValue("STATIC")
    String parallelMode();

    @Key("setThreadCount")
    @DefaultValue("1.0d")
    double threadCount();

    @Key("setVerbose")
    @DefaultValue("1")
    Integer verbose();

    @Key("setPreserveOrder")
    @DefaultValue("false")
    boolean preserveOrder();

    @Key("setGroupByInstances")
    @DefaultValue("false")
    boolean groupByInstances();

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
