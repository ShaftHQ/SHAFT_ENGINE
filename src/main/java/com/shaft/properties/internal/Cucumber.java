package com.shaft.properties.internal;

import org.aeonbits.owner.Config;

@SuppressWarnings("unused")
@Config.Sources({"system:properties", "file:src/main/resources/properties/cucumber.properties", "file:src/main/resources/properties/default/cucumber.properties", "classpath:cucumber.properties"})
public interface Cucumber extends EngineProperties<Cucumber> {

    @Key("cucumber.ansi-colors.disabled")
    @DefaultValue("false")
    boolean cucumberAnsiColorsDisabled();

    @Key("cucumber.execution.dry-run")
    @DefaultValue("false")
    boolean cucumberExecutionDryRun();

    @Key("cucumber.execution.limit")
    @DefaultValue("")
    String cucumberExecutionLimit();

    @Key("cucumber.execution.order")
    @DefaultValue("lexical")
    String cucumberExecutionOrder();

    @Key("cucumber.execution.strict")
    @DefaultValue("true")
    boolean cucumberExecutionStrict();

    @Key("cucumber.execution.wip")
    @DefaultValue("false")
    boolean cucumberExecutionWip();

    @Key("cucumber.features")
    @DefaultValue("src/test/resources")
    String cucumberFeatures();

    @Key("cucumber.filter.name")
    @DefaultValue("")
    String cucumberFilterName();

    @Key("cucumber.filter.tags")
    @DefaultValue("")
    String cucumberFilterTags();

    @Key("cucumber.glue")
    @DefaultValue("customCucumberSteps, com.shaft.cucumber")
    String cucumberGlue();

    @Key("cucumber.plugin")
    @DefaultValue("pretty, json:allure-results/cucumber.json, html:allure-results/cucumberReport.html, com.shaft.listeners.CucumberTestRunnerListener")
    String cucumberPlugin();

    @Key("cucumber.object-factory")
    @DefaultValue("")
    String cucumberObjectFactory();

    @Key("cucumber.snippet-type")
    @DefaultValue("underscore")
    String cucumberSnippetType();

    @Key("cucumber.publish.quiet")
    @DefaultValue("true")
    boolean cucumberPublishQuiet();

}
