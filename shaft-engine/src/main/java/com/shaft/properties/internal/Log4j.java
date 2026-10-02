package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;

/**
 * Configuration properties interface for Log4j logging settings in the SHAFT framework.
 * Controls appender configuration and log-level thresholds.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.log4j.set().rootLogger("INFO, stdout");
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/log4j2.properties", "file:src/main/resources/properties/default/log4j2.properties", "classpath:log4j2.properties"})
public interface Log4j extends EngineProperties<Log4j> {

    /**
     * Log4j2 configuration name, used internally as this configuration's identifier.
     *
     * <p>Default: {@code PropertiesConfig}.
     *
     * @return the configured value of {@code name}
     */
    @Key("name")
    @DefaultValue("PropertiesConfig")
    String name();

    /**
     * Log4j2 appender type for the console output target (Console).
     *
     * <p>Default: {@code Console}.
     *
     * @return the configured value of {@code appender.console.type}
     */
    @Key("appender.console.type")
    @DefaultValue("Console")
    String appenderConsoleType();

    /**
     * Reference name of the console appender, used by rootLogger and other loggers to attach to it.
     *
     * <p>Default: {@code STDOUT}.
     *
     * @return the configured value of {@code appender.console.name}
     */
    @Key("appender.console.name")
    @DefaultValue("STDOUT")
    String appenderConsoleName();

    /**
     * Log4j2 layout implementation used to format console log lines (PatternLayout).
     *
     * <p>Default: {@code PatternLayout}.
     *
     * @return the configured value of {@code appender.console.layout.type}
     */
    @Key("appender.console.layout.type")
    @DefaultValue("PatternLayout")
    String appenderConsoleLayoutType();

    /**
     * Disables ANSI color codes in console log output.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code appender.console.layout.disableAnsi}
     */
    @Key("appender.console.layout.disableAnsi")
    @DefaultValue("false")
    boolean appenderConsoleLayoutDisableAnsi();

    /**
     * Automatically disables ANSI colors when output isn't attached to a real console (e.g. redirected
     * to a file).
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code appender.console.layout.noConsoleNoAnsi}
     */
    @Key("appender.console.layout.noConsoleNoAnsi")
    @DefaultValue("false")
    boolean appenderConsoleLayoutNoConsoleNoAnsi();

    /**
     * Character encoding used when writing console log output.
     *
     * <p>Default: {@code UTF-8}.
     *
     * @return the configured value of {@code appender.console.layout.charset}
     */
    @Key("appender.console.layout.charset")
    @DefaultValue("UTF-8")
    String appenderConsoleLayoutCharset();

    /**
     * Log4j2 pattern layout reference.
     *
     * <p>Default: {@code %highlight{[%p]}{FATAL=red blink, ERROR=red bold, WARN=yellow bold,
     * INFO=fg_#0060a8 bold, DEBUG=fg_#43b02a bold, TRACE=black} %style{%d{HH:mm:ss}}{bright_black}
     * %style{|}{bright_black} %m%n}.
     *
     * @return the configured value of {@code appender.console.layout.pattern}
     */
    @Key("appender.console.layout.pattern")
    @DefaultValue("%highlight{[%p]}{FATAL=red blink, ERROR=red bold, WARN=yellow bold, INFO=fg_#0060a8 bold, DEBUG=fg_#43b02a bold, TRACE=black} %style{%d{HH:mm:ss}}{bright_black} %style{|}{bright_black} %m%n")
    String appenderConsoleLayoutPattern();

    /**
     * Log4j2 filter type applied to the console appender (ThresholdFilter).
     *
     * <p>Default: {@code ThresholdFilter}.
     *
     * @return the configured value of {@code appender.console.filter.threshold.type}
     */
    @Key("appender.console.filter.threshold.type")
    @DefaultValue("ThresholdFilter")
    String appenderConsoleFilterThresholdType();

    /**
     * Minimum log level written to the console; messages below this level are suppressed.
     *
     * <p>Default: {@code info}. Possible values: fatal, error, warn, info, debug, trace.
     *
     * @return the configured value of {@code appender.console.filter.threshold.level}
     */
    @Key("appender.console.filter.threshold.level")
    @DefaultValue("info")
    String appenderConsoleFilterThresholdLevel();

    /**
     * Log4j2 appender type for the rolling log file (RollingFile).
     *
     * <p>Default: {@code File}.
     *
     * @return the configured value of {@code appender.file.type}
     */
    @Key("appender.file.type")
    @DefaultValue("File")
    String appenderFileType();

    /**
     * Reference name of the file appender, used by rootLogger and other loggers to attach to it.
     *
     * <p>Default: {@code LOGFILE}.
     *
     * @return the configured value of {@code appender.file.name}
     */
    @Key("appender.file.name")
    @DefaultValue("LOGFILE")
    String appenderFileName();

    /**
     * Path to the active log file that SHAFT writes execution logs to.
     *
     * <p>Default: {@code target/logs/log4j.log}.
     *
     * @return the configured value of {@code appender.file.fileName}
     */
    @Key("appender.file.fileName")
    @DefaultValue("target/logs/log4j.log")
    String appenderFile_FileName();

    /**
     * Log4j2 layout implementation used to format file log lines (PatternLayout).
     *
     * <p>Default: {@code PatternLayout}.
     *
     * @return the configured value of {@code appender.file.layout.type}
     */
    @Key("appender.file.layout.type")
    @DefaultValue("PatternLayout")
    String appenderFileLayoutType();

    /**
     * Log4j2 PatternLayout string controlling the format of each file log line.
     *
     * <p>Default: {@code [%-5level] %d{yyyy-MM-dd HH:mm:ss.SSS} [%t] %c{1} - %msg%n}.
     *
     * @return the configured value of {@code appender.file.layout.pattern}
     */
    @Key("appender.file.layout.pattern")
    @DefaultValue("[%-5level] %d{yyyy-MM-dd HH:mm:ss.SSS} [%t] %c{1} - %msg%n")
    String appenderFileLayoutPattern();

    /**
     * Character encoding used when writing the log file.
     *
     * <p>Default: {@code UTF-8}.
     *
     * @return the configured value of {@code appender.file.layout.charset}
     */
    @Key("appender.file.layout.charset")
    @DefaultValue("UTF-8")
    String appenderFileLayoutCharset();

    /**
     * Log4j2 filter type applied to the file appender (ThresholdFilter).
     *
     * <p>Default: {@code ThresholdFilter}.
     *
     * @return the configured value of {@code appender.file.filter.threshold.type}
     */
    @Key("appender.file.filter.threshold.type")
    @DefaultValue("ThresholdFilter")
    String appenderFileFilterThresholdType();

    /**
     * Minimum log level written to the log file; messages below this level are suppressed.
     *
     * <p>Default: {@code debug}. Possible values: fatal, error, warn, info, debug, trace.
     *
     * @return the configured value of {@code appender.file.filter.threshold.level}
     */
    @Key("appender.file.filter.threshold.level")
    @DefaultValue("debug")
    String appenderFileFilterThresholdLevel();

    /**
     * Uses asynchronous appenders for console, file, and ReportPortal logging.
     *
     * <p>Default: {@code info, ASYNC_STDOUT, ASYNC_LOGFILE, ASYNC_REPORT_PORTAL}.
     *
     * @return the configured value of {@code rootLogger}
     */
    @Key("rootLogger")
    @DefaultValue("info, ASYNC_STDOUT, ASYNC_LOGFILE, ASYNC_REPORT_PORTAL")
    String rootLogger();

    /**
     * Fully qualified logger name whose level is overridden by logger.app.level (defaults to a noisy
     * third-party logger).
     *
     * <p>Default: {@code org.apache.http.impl.client}.
     *
     * @return the configured value of {@code logger.app.name}
     */
    @Key("logger.app.name")
    @DefaultValue("org.apache.http.impl.client")
    String loggerAppName();

    /**
     * Log level applied to the logger named by logger.app.name, used to quiet noisy third-party
     * libraries.
     *
     * <p>Default: {@code WARN}.
     *
     * @return the configured value of {@code logger.app.level}
     */
    @Key("logger.app.level")
    @DefaultValue("WARN")
    String loggerAppLevel();

}
