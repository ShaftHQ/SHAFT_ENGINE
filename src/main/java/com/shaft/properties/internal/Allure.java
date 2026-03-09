package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for Allure report generation in the SHAFT framework.
 * Properties can be supplied via system properties, {@code Allure.properties} files on the
 * classpath or file-system, and are hot-reloaded at runtime.
 *
 * <p>Use {@link #set()} to change property values programmatically:
 * <pre>{@code
 * SHAFT.Properties.allure.set().automaticallyOpen(false).generateArchive(true);
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/Allure.properties", "file:src/main/resources/properties/default/Allure.properties", "classpath:Allure.properties"})
public interface Allure extends EngineProperties<Allure> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.allureOverride.set(ConfigFactory.create(Allure.class, ThreadLocalPropertiesManager.getOverrides()));
        if (!key.equals("disableLogging"))
            ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    /**
     * Whether the Allure report HTML file should be opened automatically in the default browser
     * after the test run completes.
     * <p>Property key: {@code allure.automaticallyOpen} — default: {@code true}
     *
     * @return {@code true} to open the report automatically; {@code false} otherwise
     */
    @Key("allure.automaticallyOpen")
    @DefaultValue("true")
    boolean automaticallyOpen(); //automaticallyOpen (used to be: openAllureReportAfterExecution)

    /**
     * Whether Allure history data should be accumulated across test runs, enabling trend graphs
     * and historical comparison in the report.
     * <p>Property key: {@code allure.accumulateHistory} — default: {@code true}
     *
     * @return {@code true} to accumulate history; {@code false} to start fresh each run
     */
    @Key("allure.accumulateHistory")
    @DefaultValue("true")
    boolean accumulateHistory(); //accumulateHistory (used to be: cleanAllureResultsDirectoryBeforeExecution)

    /**
     * Whether generated Allure HTML report files should accumulate in the report directory
     * rather than being replaced on each run.
     * <p>Property key: {@code allure.accumulateReports} — default: {@code true}
     *
     * @return {@code true} to keep previous report HTML files; {@code false} to overwrite them
     */
    @Key("allure.accumulateReports")
    @DefaultValue("true")
    boolean accumulateReports(); //allows html files to accumulate in the allure report directory

    /**
     * Whether the Allure results directory should be cleaned before a new test run starts.
     * Disable this when you want to merge results from multiple partial runs.
     * <p>Property key: {@code allure.cleanResultsDirectory} — default: {@code true}
     *
     * @return {@code true} to delete the results directory before execution; {@code false} to preserve it
     */
    @Key("allure.cleanResultsDirectory")
    @DefaultValue("true")
    boolean cleanResultsDirectory();

    /**
     * Whether a self-contained ZIP archive of the generated Allure report should be created
     * after the test run. Useful for sharing or archiving reports in CI pipelines.
     * <p>Property key: {@code allure.generateArchive} — default: {@code false}
     *
     * @return {@code true} to generate a ZIP archive of the report; {@code false} otherwise
     */
    @Key("allure.generateArchive")
    @DefaultValue("false")
    boolean generateArchive(); //generateArchive (used to be: generateAllureReportArchive)

    /**
     * URL pointing to a custom logo image that replaces the default SHAFT logo in the Allure report.
     * <p>Property key: {@code allure.customLogo} — default: SHAFT_ENGINE white logo on GitHub
     *
     * @return a URL string for the custom logo image
     */
    @Key("allure.customLogo")
    @DefaultValue("https://github.com/ShaftHQ/SHAFT_ENGINE/blob/main/src/main/resources/images/shaft_white.png?raw=true")
    String customLogo();

    /**
     * Custom title displayed in the header of the generated Allure report.
     * <p>Property key: {@code allure.customTitle} — default: {@code "Test run report"}
     *
     * @return the report title string
     */
    @Key("allure.customTitle")
    @DefaultValue("Test run report")
    String customTitle();

    /**
     * Returns a fluent {@link SetProperty} builder for programmatically overriding Allure properties.
     *
     * <p>Example:
     * <pre>{@code
     * SHAFT.Properties.allure.set().automaticallyOpen(false).customTitle("My Suite");
     * }</pre>
     *
     * @return a new {@link SetProperty} instance
     */
    default SetProperty set() {
        return new SetProperty();
    }

    /**
     * Fluent builder that allows programmatic override of individual Allure configuration properties.
     * All setter methods return {@code this} to support method chaining.
     *
     * <p>Example:
     * <pre>{@code
     * SHAFT.Properties.allure.set()
     *     .automaticallyOpen(false)
     *     .accumulateHistory(true)
     *     .customTitle("Regression Suite");
     * }</pre>
     */
    class SetProperty implements EngineProperties.SetProperty {

        /**
         * Overrides the {@code allure.automaticallyOpen} property at runtime.
         *
         * @param value {@code true} to open the report automatically after execution
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty automaticallyOpen(boolean value) {
            setProperty("allure.automaticallyOpen", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code allure.accumulateHistory} property at runtime.
         *
         * @param value {@code true} to keep and accumulate Allure history across runs
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty accumulateHistory(boolean value) {
            setProperty("allure.accumulateHistory", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code allure.accumulateReports} property at runtime.
         *
         * @param value {@code true} to keep previous report HTML files in the report directory
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty accumulateReports(boolean value) {
            setProperty("allure.accumulateReports", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code allure.cleanResultsDirectory} property at runtime.
         *
         * @param value {@code true} to clean the Allure results directory before execution
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty cleanResultsDirectory(boolean value) {
            setProperty("allure.cleanResultsDirectory", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code allure.generateArchive} property at runtime.
         *
         * @param value {@code true} to generate a ZIP archive of the report after execution
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty generateArchive(boolean value) {
            setProperty("allure.generateArchive", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code allure.customLogo} property at runtime.
         *
         * @param value a URL string pointing to the replacement logo image
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty customLogo(String value) {
            setProperty("allure.customLogo", value);
            return this;
        }

        /**
         * Overrides the {@code allure.customTitle} property at runtime.
         *
         * @param value the report title to display in the Allure report header
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty customTitle(String value) {
            setProperty("allure.customTitle", value);
            return this;
        }

    }

}
