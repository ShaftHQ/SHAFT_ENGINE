package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for Jira and Xray integration in the SHAFT framework.
 * Controls project keys, authorization, and test-management reporting settings.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.jira.set().projectKey("PROJ").authorization("Bearer token");
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/JiraXRay.properties", "file:src/main/resources/properties/default/JiraXRay.properties", "classpath:JiraXRay.properties"})
public interface Jira extends EngineProperties<Jira> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.jiraOverride.set(ConfigFactory.create(Jira.class, ThreadLocalPropertiesManager.getOverrides()));
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("jiraInteraction")
    @DefaultValue("false")
    boolean isEnabled();

    @Key("jiraUrl")
    @DefaultValue("https://")
    String url();

    @Key("projectKey")
    @DefaultValue("")
    String projectKey();

    @Key("authorization")
    @DefaultValue(":")
    String authorization();

    @Key("authType")
    @DefaultValue("basic")
    String authType();

    @Key("reportTestCasesExecution")
    @DefaultValue("false")
    boolean reportTestCasesExecution();

    @Key("reportPath")
    @DefaultValue("target/surefire-reports/testng-results.xml")
    String reportPath();

    @Key("ExecutionName")
    @DefaultValue("")
    String executionName();

    @Key("ExecutionDescription")
    @DefaultValue("")
    String executionDescription();

    @Key("ReportBugs")
    @DefaultValue("false")
    boolean reportBugs();

    @Key("assignee")
    @DefaultValue("")
    String assignee();

    @Key("allure.link.tms.pattern")
    @DefaultValue("https:///{}")
    String allureLinkTmsPattern();

    @Key("allure.link.custom.pattern")
    @DefaultValue("{}")
    String allureLinkCustomPattern();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty jiraInteraction(boolean value) {
            setProperty("jiraInteraction", String.valueOf(value));
            return this;
        }

        public SetProperty jiraUrl(String value) {
            setProperty("jiraUrl", value);
            return this;
        }

        public SetProperty projectKey(String value) {
            setProperty("projectKey", value);
            return this;
        }

        public SetProperty authorization(String value) {
            setProperty("authorization", value);
            return this;
        }

        public SetProperty authType(String value) {
            setProperty("authType", value);
            return this;
        }

        public SetProperty reportTestCasesExecution(boolean value) {
            setProperty("reportTestCasesExecution", String.valueOf(value));
            return this;
        }

        public SetProperty reportPath(String value) {
            setProperty("reportPath", value);
            return this;
        }

        public SetProperty executionName(String value) {
            setProperty("ExecutionName", value);
            return this;
        }

        public SetProperty executionDescription(String value) {
            setProperty("ExecutionDescription", value);
            return this;
        }

        public SetProperty reportBugs(boolean value) {
            setProperty("ReportBugs", String.valueOf(value));
            return this;
        }

        public SetProperty assignee(String value) {
            setProperty("assignee", value);
            return this;
        }

        public SetProperty allureLinkTmsPattern(String value) {
            setProperty("allure.link.tms.pattern", value);
            return this;
        }

        public SetProperty allureLinkCustomPattern(String value) {
            setProperty("allure.link.custom.pattern", value);
            return this;
        }

    }

}
