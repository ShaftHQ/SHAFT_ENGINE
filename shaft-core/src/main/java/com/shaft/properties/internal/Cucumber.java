package com.shaft.properties.internal;

import org.aeonbits.owner.Config;

/**
 * Configuration properties interface for Cucumber test execution in the SHAFT framework.
 * Properties can be supplied via system properties, {@code cucumber.properties} files on the
 * classpath or file-system, and are hot-reloaded at runtime.
 *
 * <p>Use {@link #set()} to override values programmatically.
 */
@SuppressWarnings("unused")
@Config.Sources({"system:properties", "file:src/main/resources/properties/cucumber.properties", "file:src/main/resources/properties/default/cucumber.properties", "classpath:cucumber.properties"})
public interface Cucumber extends EngineProperties<Cucumber> {

    /**
     * Whether ANSI color output is disabled in Cucumber's console output.
     * <p>Property key: {@code cucumber.ansi-colors.disabled} — default: {@code false}
     *
     * @return {@code true} to disable ANSI colors; {@code false} to keep them enabled
     */
    @Key("cucumber.ansi-colors.disabled")
    @DefaultValue("false")
    boolean cucumberAnsiColorsDisabled();

    /**
     * Whether Cucumber runs in dry-run mode, parsing but not executing step definitions.
     * <p>Property key: {@code cucumber.execution.dry-run} — default: {@code false}
     *
     * @return {@code true} to enable dry-run mode; {@code false} for normal execution
     */
    @Key("cucumber.execution.dry-run")
    @DefaultValue("false")
    boolean cucumberExecutionDryRun();

    /**
     * Maximum number of scenarios to execute. An empty value means no limit.
     * <p>Property key: {@code cucumber.execution.limit} — default: {@code ""}
     *
     * @return the execution limit as a string, or empty for unlimited
     */
    @Key("cucumber.execution.limit")
    @DefaultValue("")
    String cucumberExecutionLimit();

    /**
     * Order in which Cucumber scenarios are executed (e.g., {@code "lexical"} or {@code "random"}).
     * <p>Property key: {@code cucumber.execution.order} — default: {@code "lexical"}
     *
     * @return the execution order string
     */
    @Key("cucumber.execution.order")
    @DefaultValue("lexical")
    String cucumberExecutionOrder();

    /**
     * Whether Cucumber runs in strict mode, treating undefined and pending steps as failures.
     * <p>Property key: {@code cucumber.execution.strict} — default: {@code true}
     *
     * @return {@code true} to fail on undefined/pending steps; {@code false} to allow them
     */
    @Key("cucumber.execution.strict")
    @DefaultValue("true")
    boolean cucumberExecutionStrict();

    /**
     * Whether Cucumber runs in WIP (work-in-progress) mode, failing if any scenario passes.
     * <p>Property key: {@code cucumber.execution.wip} — default: {@code false}
     *
     * @return {@code true} to enable WIP mode; {@code false} otherwise
     */
    @Key("cucumber.execution.wip")
    @DefaultValue("false")
    boolean cucumberExecutionWip();

    /**
     * Path(s) to the feature files or directories containing Cucumber feature files.
     * <p>Property key: {@code cucumber.features} — default: {@code "src/test/resources"}
     *
     * @return the features path string
     */
    @Key("cucumber.features")
    @DefaultValue("src/test/resources")
    String cucumberFeatures();

    /**
     * Regular expression filter to select scenarios by name. An empty value runs all scenarios.
     * <p>Property key: {@code cucumber.filter.name} — default: {@code ""}
     *
     * @return the name filter regex, or empty to run all scenarios
     */
    @Key("cucumber.filter.name")
    @DefaultValue("")
    String cucumberFilterName();

    /**
     * Tag expression used to filter which scenarios are executed (e.g., {@code "@smoke and not @slow"}).
     * An empty value runs all scenarios.
     * <p>Property key: {@code cucumber.filter.tags} — default: {@code ""}
     *
     * @return the tag filter expression, or empty to run all scenarios
     */
    @Key("cucumber.filter.tags")
    @DefaultValue("")
    String cucumberFilterTags();

    /**
     * Comma-separated list of packages to scan for Cucumber step definitions and hooks.
     * <p>Property key: {@code cucumber.glue} — default: {@code "customCucumberSteps, com.shaft.cucumber"}
     *
     * @return the glue path(s) string
     */
    @Key("cucumber.glue")
    @DefaultValue("customCucumberSteps, com.shaft.cucumber")
    String cucumberGlue();

    /**
     * Comma-separated list of Cucumber plugins used for reporting and listeners.
     * <p>Property key: {@code cucumber.plugin} — default: pretty, JSON, HTML, and SHAFT listener
     *
     * @return the plugin configuration string
     */
    @Key("cucumber.plugin")
    @DefaultValue("pretty, json:allure-results/cucumber.json, html:allure-results/cucumberReport.html, com.shaft.listeners.CucumberTestRunnerListener")
    String cucumberPlugin();

    /**
     * Fully qualified class name of a custom Cucumber object factory for dependency injection.
     * An empty value uses the default object factory.
     * <p>Property key: {@code cucumber.object-factory} — default: {@code ""}
     *
     * @return the object factory class name, or empty to use the default
     */
    @Key("cucumber.object-factory")
    @DefaultValue("")
    String cucumberObjectFactory();

    /**
     * Snippet style used when generating missing step definition stubs
     * (e.g., {@code "underscore"} or {@code "camelcase"}).
     * <p>Property key: {@code cucumber.snippet-type} — default: {@code "underscore"}
     *
     * @return the snippet type string
     */
    @Key("cucumber.snippet-type")
    @DefaultValue("underscore")
    String cucumberSnippetType();

    /**
     * Whether the Cucumber publish report banner is suppressed from console output.
     * <p>Property key: {@code cucumber.publish.quiet} — default: {@code true}
     *
     * @return {@code true} to suppress the publish banner; {@code false} to display it
     */
    @Key("cucumber.publish.quiet")
    @DefaultValue("true")
    boolean cucumberPublishQuiet();

}
