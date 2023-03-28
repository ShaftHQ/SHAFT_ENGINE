package com.shaft.internal.properties;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/cucumber.properties", "file:src/main/resources/properties/default/cucumber.properties", "classpath:cucumber.properties"})
public interface Cucumber extends EngineProperties {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.cucumber = ConfigFactory.create(Cucumber.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

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

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void cucumberAnsiColorsDisabled(boolean value) {
            setProperty("cucumber.ansi-colors.disabled", String.valueOf(value));
        }

        public void cucumberExecutionDryRun(boolean value) {
            setProperty("cucumber.execution.dry-run", String.valueOf(value));
        }

        public void cucumberExecutionLimit(String value) {
            setProperty("cucumber.execution.limit", value);
        }

        public void cucumberExecutionOrder(String value) {
            setProperty("cucumber.execution.order", value);
        }

        public void cucumberExecutionStrict(boolean value) {
            setProperty("cucumber.execution.strict", String.valueOf(value));
        }

        public void cucumberExecutionWip(boolean value) {
            setProperty("cucumber.execution.wip", String.valueOf(value));
        }

        public void cucumberFeatures(String value) {
            setProperty("cucumber.features", value);
        }

        public void cucumberFilterName(String value) {
            setProperty("cucumber.filter.name", value);
        }

        public void cucumberFilterTags(String value) {
            setProperty("cucumber.filter.tags", value);
        }

        public void cucumberGlue(String value) {
            setProperty("cucumber.glue", value);
        }

        public void cucumberPlugin(String value) {
            setProperty("cucumber.plugin", value);
        }

        public void cucumberObjectFactory(String value) {
            setProperty("cucumber.object-factory", value);
        }

        public void cucumberSnippetType(String value) {
            setProperty("cucumber.snippet-type", value);
        }

        public void cucumberPublishQuiet(boolean value) {
            setProperty("cucumber.publish.quiet", String.valueOf(value));
        }


    }

}
