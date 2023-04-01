package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/TestNG.properties", "file:src/main/resources/properties/default/TestNG.properties", "classpath:TestNG.properties",})
public interface TestNG extends EngineProperties {

    @Key("setParallel")
    @DefaultValue("NONE")
    String setParallel();

    @Key("setThreadCount")
    @DefaultValue("1")
    String setThreadCount();

    @Key("setVerbose")
    @DefaultValue("1")
    String setVerbose();

    @Key("setPreserveOrder")
    @DefaultValue("true")
    boolean setPreserveOrder();

    @Key("setGroupByInstances")
    @DefaultValue("true")
    boolean setGroupByInstances();

    @Key("setDataProviderThreadCount")
    @DefaultValue("1")
    String setDataProviderThreadCount();

}
