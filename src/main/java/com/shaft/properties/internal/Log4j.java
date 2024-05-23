package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/log4j2.properties", "file:src/main/resources/properties/default/log4j2.properties", "classpath:log4j2.properties"})
public interface Log4j extends EngineProperties<Log4j> {

    @Key("name")
    @DefaultValue("PropertiesConfig")
    String name();

    @Key("appender.console.type")
    @DefaultValue("Console")
    String appenderConsoleType();

    @Key("appender.console.name")
    @DefaultValue("STDOUT")
    String appenderConsoleName();

    @Key("appender.console.layout.type")
    @DefaultValue("PatternLayout")
    String appenderConsoleLayoutType();

    @Key("appender.console.layout.disableAnsi")
    @DefaultValue("false")
    boolean appenderConsoleLayoutDisableAnsi();

    @Key("appender.console.layout.pattern")
    @DefaultValue("%highlight{[%p]}{FATAL=red blink, ERROR=red bold, WARN=yellow bold, INFO=fg_#0060a8 bold, DEBUG=fg_#43b02a bold, TRACE=black} %style{%m} %style{| @%d{hh:mm:ss a}}{bright_black} %n")
    String appenderConsoleLayoutPattern();

    @Key("appender.console.filter.threshold.type")
    @DefaultValue("ThresholdFilter")
    String appenderConsoleFilterThresholdType();

    @Key("appender.console.filter.threshold.level")
    @DefaultValue("info")
    String appenderConsoleFilterThresholdLevel();

    @Key("appender.file.type")
    @DefaultValue("File")
    String appenderFileType();

    @Key("appender.file.name")
    @DefaultValue("LOGFILE")
    String appenderFileName();

    @Key("appender.file.fileName")
    @DefaultValue("target/logs/log4j.log")
    String appenderFile_FileName();

    @Key("appender.file.layout.type")
    @DefaultValue("PatternLayout")
    String appenderFileLayoutType();

    @Key("appender.file.layout.pattern")
    @DefaultValue("[%-5level] %d{yyyy-MM-dd HH:mm:ss.SSS} [%t] %c{1} - %msg%n")
    String appenderFileLayoutPattern();

    @Key("appender.file.filter.threshold.type")
    @DefaultValue("ThresholdFilter")
    String appenderFileFilterThresholdType();

    @Key("appender.file.filter.threshold.level")
    @DefaultValue("debug")
    String appenderFileFilterThresholdLevel();

    @Key("rootLogger")
    @DefaultValue("debug, STDOUT, LOGFILE")
    String rootLogger();

    @Key("logger.app.name")
    @DefaultValue("org.apache.http.impl.client")
    String loggerAppName();

    @Key("logger.app.level")
    @DefaultValue("WARN")
    String loggerAppLevel();

}
