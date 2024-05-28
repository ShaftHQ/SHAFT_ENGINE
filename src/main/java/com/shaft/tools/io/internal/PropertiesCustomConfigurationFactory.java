package com.shaft.tools.io.internal;

import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.ConfigurationFactory;
import org.apache.logging.log4j.core.config.ConfigurationSource;
import org.apache.logging.log4j.core.config.Order;
import org.apache.logging.log4j.core.config.builder.api.Component;
import org.apache.logging.log4j.core.config.builder.api.ConfigurationBuilder;
import org.apache.logging.log4j.core.config.builder.api.RootLoggerComponentBuilder;
import org.apache.logging.log4j.core.config.builder.impl.BuiltConfiguration;

//@Plugin(name = "PropertiesCustomConfigurationFactory", category = "ConfigurationFactory")
//@Order(10)
public class PropertiesCustomConfigurationFactory extends ConfigurationFactory {
    public static final String[] SUFFIXES = new String[]{".properties", "*"};
    /*Approach 1: I tried this approach, but it completely ignores the properties file*/
/*    static Configuration createConfig(String name, ConfigurationBuilder<BuiltConfiguration> builder){
        final String appenderPattern = "%highlight{[%p]}{FATAL=red blink, ERROR=red bold, WARN=yellow bold, INFO=fg_#0060a8 bold, DEBUG=fg_#43b02a bold, TRACE=black} %style{%m }%style{| %-logger}{bright_black} %style{- %-thread}{bright_black} %style{- %d{hh:mm:ss a}}{bright_black}";
        builder.setConfigurationName(name);
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
        return builder.build();
    }*/
    @Override
    protected String[] getSupportedTypes() {
        return SUFFIXES;
    }

    /*Approach 2: The constructor requires a parameter of type 'Component' and I don't know how to provide this element?? Note: The docs don't state anything about this case*/
    @Override
    public Configuration getConfiguration(LoggerContext loggerContext, ConfigurationSource source) {
        /*this belongs to Approach1*/
/*        ConfigurationBuilder<BuiltConfiguration> builder = newConfigurationBuilder();
        return createConfig(source.toString(), builder);*/
        /*This belongs to Approach 2*/
        //return new progressBarLoggerConfig(loggerContext, source);
        return null;
    }
}
