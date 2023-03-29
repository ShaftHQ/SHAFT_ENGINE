package com.shaft.internal.properties;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/log4j2.properties", "file:src/main/resources/properties/default/log4j2.properties", "classpath:log4j2.properties"})
public interface Log4j extends EngineProperties {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.log4j = ConfigFactory.create(Log4j.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

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
    @DefaultValue("%highlight{[%p]}{FATAL=red blink, ERROR=red bold, WARN=yellow bold, INFO=fg_#0060a8 bold, DEBUG=fg_#43b02a bold, TRACE=black} %style{%m} %style{| %-logger}{bright_black} %style{- %-thread}{bright_black} %style{- %d{hh:mm:ss a}}{bright_black} %n")
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


    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void name(String value) {
            setProperty("name", value);
        }

        public void appenderConsoleType(String value) {
            setProperty("appender.console.type", value);
        }

        public void appenderConsoleName(String value) {
            setProperty("appender.console.name", value);
        }

        public void appenderConsoleLayoutType(String value) {
            setProperty("appender.console.layout.type", value);
        }

        public void appenderConsoleLayoutDisableAnsi(String value) {
            setProperty("appender.console.layout.disableAnsi", value);
        }

        public void appenderConsoleLayoutPattern(String value) {
            setProperty("appender.console.layout.pattern", value);
        }

        public void appenderConsoleFilterThresholdType(String value) {
            setProperty("appender.console.filter.threshold.type", value);
        }

        public void appenderConsoleFilterThresholdLevel(String value) {
            setProperty("appender.console.filter.threshold.level", value);
        }

        public void appenderFileType(String value) {
            setProperty("appender.file.type", value);
        }

        public void appenderFileName(String value) {
            setProperty("appender.file.name", value);
        }

        public void appenderFile_FileName(String value) {
            setProperty("appender.file.fileName", value);
        }

        public void appenderFileLayoutType(String value) {
            setProperty("appender.file.layout.type", value);
        }

        public void appenderFileLayoutPattern(String value) {
            setProperty("appender.file.layout.pattern", value);
        }

        public void appenderFileFilterThresholdType(String value) {
            setProperty("appender.file.filter.threshold.type", value);
        }

        public void appenderFileFilterThresholdLevel(String value) {
            setProperty("appender.file.filter.threshold.type", value);
        }

        public void rootLogger(String value) {
            setProperty("rootLogger", value);
        }

        public void loggerAppName(String value) {
            setProperty("logger.app.name", value);
        }

        public void loggerAppLevel(String value) {
            setProperty("logger.app.level", value);
        }

    }

}
