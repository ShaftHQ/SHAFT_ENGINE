package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/TestNG.properties", "file:src/main/resources/properties/default/TestNG.properties", "classpath:TestNG.properties",})
public interface TestNG extends EngineProperties {

    @Key("setParallel")
    @DefaultValue("NONE")
    String parallel();

    @Key("setThreadCount")
    @DefaultValue("1")
    int threadCount();

    @Key("setVerbose")
    @DefaultValue("1")
    Integer verbose();

    @Key("setPreserveOrder")
    @DefaultValue("true")
    boolean preserveOrder();

    @Key("setGroupByInstances")
    @DefaultValue("true")
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
