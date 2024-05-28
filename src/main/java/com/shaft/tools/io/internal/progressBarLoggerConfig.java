package com.shaft.tools.io.internal;

import org.apache.logging.log4j.core.Filter;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.appender.ConsoleAppender;
import org.apache.logging.log4j.core.config.ConfigurationSource;
import org.apache.logging.log4j.core.config.builder.api.*;
import org.apache.logging.log4j.core.config.builder.impl.BuiltConfiguration;
import org.apache.logging.log4j.core.config.properties.PropertiesConfiguration;

import static org.apache.logging.log4j.core.config.builder.api.ConfigurationBuilderFactory.newConfigurationBuilder;

public class progressBarLoggerConfig extends PropertiesConfiguration {

    public progressBarLoggerConfig(LoggerContext loggerContext, ConfigurationSource source, Component root) {
        super(loggerContext, source, root);
    }

    @Override
    protected void doConfigure() {
        super.doConfigure();
        ConfigurationBuilder<BuiltConfiguration> builder = newConfigurationBuilder();
        final String appenderPattern = "%highlight{[%p]}{FATAL=red blink, ERROR=red bold, WARN=yellow bold, INFO=fg_#0060a8 bold, DEBUG=fg_#43b02a bold, TRACE=black} %style{%m }%style{| %-logger}{bright_black} %style{- %-thread}{bright_black} %style{- %d{hh:mm:ss a}}{bright_black}";
        builder.setConfigurationName(getConfigurationSource().toString());
        //Appender construction
        AppenderComponentBuilder console2 = builder.newAppender("STDOUT2", "CONSOLE").
                addAttribute("target", ConsoleAppender.Target.SYSTEM_OUT);
        LayoutComponentBuilder patternLayout = builder.newLayout("PatternLayout").
                addAttribute("pattern", appenderPattern);
        FilterComponentBuilder thresholdFilter = builder.newFilter("ThresholdFilter", Filter.Result.ACCEPT, Filter.Result.NEUTRAL).
                addAttribute("level", "debug");
        console2.add(patternLayout).add(thresholdFilter);
        builder.add(console2);
        //Logger construction
        LoggerComponentBuilder progressLogger = builder.newLogger("progressConsole").
                addAttribute("additivity", false);
        progressLogger.add(builder.newAppenderRef("STDOUT2"));
        builder.add(progressLogger);
        builder.build();
    }
}
