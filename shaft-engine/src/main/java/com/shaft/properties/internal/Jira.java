package com.shaft.properties.internal;

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
        EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * Enable Jira integration for test management.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code jiraInteraction}
     */
    @Key("jiraInteraction")
    @DefaultValue("false")
    boolean isEnabled();

    /**
     * Jira instance URL.
     *
     * <p>Default: {@code https://}. Possible values: URL.
     *
     * @return the configured value of {@code jiraUrl}
     */
    @Key("jiraUrl")
    @DefaultValue("https://")
    String url();

    /**
     * Jira project key.
     *
     * <p>Possible values: String.
     *
     * @return the configured value of {@code projectKey}
     */
    @Key("projectKey")
    @DefaultValue("")
    String projectKey();

    /**
     * Jira authorization credentials (username:token or username:password).
     *
     * <p>Possible values: username:token.
     *
     * @return the configured value of {@code authorization}
     */
    @Key("authorization")
    @DefaultValue(":")
    String authorization();

    /**
     * Authorization type for Jira API.
     *
     * <p>Default: {@code basic}. Possible values: basic, bearer.
     *
     * @return the configured value of {@code authType}
     */
    @Key("authType")
    @DefaultValue("basic")
    String authType();

    /**
     * Report test case execution results to Jira/Xray.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code reportTestCasesExecution}
     */
    @Key("reportTestCasesExecution")
    @DefaultValue("false")
    boolean reportTestCasesExecution();

    /**
     * Path to test results file for Jira/Xray reporting.
     *
     * <p>Default: {@code target/surefire-reports/testng-results.xml}. Possible values: File path.
     *
     * @return the configured value of {@code reportPath}
     */
    @Key("reportPath")
    @DefaultValue("target/surefire-reports/testng-results.xml")
    String reportPath();

    /**
     * Name for test execution in Jira/Xray.
     *
     * <p>Possible values: String.
     *
     * @return the configured value of {@code ExecutionName}
     */
    @Key("ExecutionName")
    @DefaultValue("")
    String executionName();

    /**
     * Description for test execution in Jira/Xray.
     *
     * <p>Possible values: String.
     *
     * @return the configured value of {@code ExecutionDescription}
     */
    @Key("ExecutionDescription")
    @DefaultValue("")
    String executionDescription();

    /**
     * Automatically report bugs to Jira on test failures.
     *
     * <p>Default: {@code false}. Possible values: true, false.
     *
     * @return the configured value of {@code ReportBugs}
     */
    @Key("ReportBugs")
    @DefaultValue("false")
    boolean reportBugs();

    /**
     * Default assignee for reported bugs.
     *
     * <p>Possible values: String.
     *
     * @return the configured value of {@code assignee}
     */
    @Key("assignee")
    @DefaultValue("")
    String assignee();

    /**
     * Pattern for TMS (Test Management System) links in Allure.
     *
     * <p>Default: {@code https:///{}}. Possible values: URL pattern.
     *
     * @return the configured value of {@code allure.link.tms.pattern}
     */
    @Key("allure.link.tms.pattern")
    @DefaultValue("https:///{}")
    String allureLinkTmsPattern();

    /**
     * Pattern for custom links in Allure reports.
     *
     * <p>Default: {@code {}}. Possible values: URL pattern.
     *
     * @return the configured value of {@code allure.link.custom.pattern}
     */
    @Key("allure.link.custom.pattern")
    @DefaultValue("{}")
    String allureLinkCustomPattern();

    /**
     * Starts a fluent, thread-local override of these properties for the current test thread.
     *
     * @return a new {@link SetProperty} builder
     */
    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        /**
         * Overrides the {@code jiraInteraction} property at runtime. Enable Jira integration for test
         * management.
         *
         * @param value the new value of {@code jiraInteraction}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty jiraInteraction(boolean value) {
            setProperty("jiraInteraction", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code jiraUrl} property at runtime. Jira instance URL.
         *
         * @param value the new value of {@code jiraUrl}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty jiraUrl(String value) {
            setProperty("jiraUrl", value);
            return this;
        }

        /**
         * Overrides the {@code projectKey} property at runtime. Jira project key.
         *
         * @param value the new value of {@code projectKey}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty projectKey(String value) {
            setProperty("projectKey", value);
            return this;
        }

        /**
         * Overrides the {@code authorization} property at runtime. Jira authorization credentials
         * (username:token or username:password).
         *
         * @param value the new value of {@code authorization}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty authorization(String value) {
            setProperty("authorization", value);
            return this;
        }

        /**
         * Overrides the {@code authType} property at runtime. Authorization type for Jira API.
         *
         * @param value the new value of {@code authType}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty authType(String value) {
            setProperty("authType", value);
            return this;
        }

        /**
         * Overrides the {@code reportTestCasesExecution} property at runtime. Report test case execution
         * results to Jira/Xray.
         *
         * @param value the new value of {@code reportTestCasesExecution}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty reportTestCasesExecution(boolean value) {
            setProperty("reportTestCasesExecution", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code reportPath} property at runtime. Path to test results file for Jira/Xray
         * reporting.
         *
         * @param value the new value of {@code reportPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty reportPath(String value) {
            setProperty("reportPath", value);
            return this;
        }

        /**
         * Overrides the {@code ExecutionName} property at runtime. Name for test execution in Jira/Xray.
         *
         * @param value the new value of {@code ExecutionName}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty executionName(String value) {
            setProperty("ExecutionName", value);
            return this;
        }

        /**
         * Overrides the {@code ExecutionDescription} property at runtime. Description for test execution
         * in Jira/Xray.
         *
         * @param value the new value of {@code ExecutionDescription}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty executionDescription(String value) {
            setProperty("ExecutionDescription", value);
            return this;
        }

        /**
         * Overrides the {@code ReportBugs} property at runtime. Automatically report bugs to Jira on test
         * failures.
         *
         * @param value the new value of {@code ReportBugs}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty reportBugs(boolean value) {
            setProperty("ReportBugs", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code assignee} property at runtime. Default assignee for reported bugs.
         *
         * @param value the new value of {@code assignee}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty assignee(String value) {
            setProperty("assignee", value);
            return this;
        }

        /**
         * Overrides the {@code allure.link.tms.pattern} property at runtime. Pattern for TMS (Test
         * Management System) links in Allure.
         *
         * @param value the new value of {@code allure.link.tms.pattern}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty allureLinkTmsPattern(String value) {
            setProperty("allure.link.tms.pattern", value);
            return this;
        }

        /**
         * Overrides the {@code allure.link.custom.pattern} property at runtime. Pattern for custom links
         * in Allure reports.
         *
         * @param value the new value of {@code allure.link.custom.pattern}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty allureLinkCustomPattern(String value) {
            setProperty("allure.link.custom.pattern", value);
            return this;
        }

    }

}
