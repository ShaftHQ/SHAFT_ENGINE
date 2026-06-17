package com.shaft.properties.internal;

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
            EngineProperties.logPropertyUpdate(key, value);
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
     * <p>Property key: {@code allure.customLogo} — default: shaft-engine white logo on GitHub
     *
     * @return a URL string for the custom logo image
     */
    @Key("allure.customLogo")
    @DefaultValue("https://github.com/ShaftHQ/SHAFT_ENGINE/blob/main/shaft-engine/src/main/resources/images/shaft.png?raw=true")
    String customLogo();

    /**
     * Custom title displayed in the header of the generated Allure report.
     * <p>Property key: {@code allure.customTitle} — default: {@code "Test run report"}
     *
     * @return the report title string
     */
    @Key("allure.customTitle")
    @DefaultValue("SHAFT-powered test report")
    String customTitle();

    /**
     * Opt-in switch to enforce usage of the configured Allure 3 CLI version
     * ({@code SHAFT.Properties.internal.allure3Version()}).
     *
     * <p>Property key: {@code allure.forceConfiguredCliVersion} — default: {@code true}
     *
     * <p>When enabled:
     * <ul>
     *   <li>SHAFT bypasses system {@code allure} binary detection (including Allure 2 compatibility checks).</li>
     *   <li>SHAFT uses managed Allure 3 resolution only: {@code npx --yes allure@<allure3Version>} (or downloaded Node.js fallback).</li>
     * </ul>
     *
     * <p>When disabled, SHAFT uses PATH-first behavior and may activate Allure 2 compatibility mode
     * when a system 2.x binary is detected.
     *
     * @return {@code true} to enforce configured Allure 3 CLI usage; {@code false} for legacy PATH-first behavior
     */
    @Key("allure.forceConfiguredCliVersion")
    @DefaultValue("true")
    boolean forceConfiguredCliVersion();

    /**
     * Enables SHAFT-managed real-time Allure 3 report monitoring via {@code allure watch}.
     *
     * <p>Property key: {@code allure.realtimeMonitoring} — default: {@code false}
     *
     * <p>When enabled, SHAFT starts monitoring when Allure 3 CLI resolution succeeds. This feature
     * is unavailable in Allure 2 compatibility mode.
     *
     * @return {@code true} to allow real-time monitoring when Allure 3 is available; {@code false} to disable it
     */
    @Key("allure.realtimeMonitoring")
    @DefaultValue("false")
    boolean realtimeMonitoring();

    /**
     * Controls the Allure 3 awesome plugin {@code singleFile} option written to {@code allurerc.yaml}.
     *
     * <p>Property key: {@code allure.singleFile} — default: {@code true}
     *
     * @return {@code true} to generate a self-contained HTML report; {@code false} for multi-file output
     */
    @Key("allure.singleFile")
    @DefaultValue("true")
    boolean singleFile();

    /**
     * Controls the Allure 3 awesome plugin {@code reportLanguage} option written to {@code allurerc.yaml}.
     *
     * <p>Property key: {@code allure.reportLanguage} — default: {@code en}
     *
     * @return the language code used by the generated Allure 3 report
     */
    @Key("allure.reportLanguage")
    @DefaultValue("en")
    String reportLanguage();

    /**
     * Controls the Allure 3 awesome plugin {@code open} option written to {@code allurerc.yaml}.
     *
     * <p>Property key: {@code allure.open} — default: {@code false}
     *
     * @return {@code true} when the Allure CLI should open the report itself; {@code false} otherwise
     */
    @Key("allure.open")
    @DefaultValue("false")
    boolean open();

    /**
     * Controls the Allure 3 awesome plugin {@code groupBy} values written to {@code allurerc.yaml}.
     * Supply values as a comma-separated list.
     *
     * <p>Property key: {@code allure.groupBy} — default: {@code parentSuite,suite,subSuite}
     *
     * @return comma-separated grouping fields used by the generated Allure 3 report
     */
    @Key("allure.groupBy")
    @DefaultValue("parentSuite,suite,subSuite")
    String groupBy();

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
         * Creates a new {@code SetProperty} instance.
         */
        public SetProperty() {
        }

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

        /**
         * Overrides the {@code allure.forceConfiguredCliVersion} property at runtime.
         *
         * @param value {@code true} to enforce configured {@code allure3Version} and ignore mismatched PATH-installed allure binaries
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty forceConfiguredCliVersion(boolean value) {
            setProperty("allure.forceConfiguredCliVersion", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code allure.realtimeMonitoring} property at runtime.
         *
         * @param value {@code true} to allow real-time monitoring when eligible; {@code false} to disable it
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty realtimeMonitoring(boolean value) {
            setProperty("allure.realtimeMonitoring", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code allure.singleFile} property at runtime.
         *
         * @param value {@code true} for self-contained HTML reports; {@code false} for multi-file output
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty singleFile(boolean value) {
            setProperty("allure.singleFile", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code allure.reportLanguage} property at runtime.
         *
         * @param value the language code used by the generated Allure 3 report
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty reportLanguage(String value) {
            setProperty("allure.reportLanguage", value);
            return this;
        }

        /**
         * Overrides the {@code allure.open} property at runtime.
         *
         * @param value {@code true} when the Allure CLI should open the report itself; {@code false} otherwise
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty open(boolean value) {
            setProperty("allure.open", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code allure.groupBy} property at runtime.
         *
         * @param value comma-separated grouping fields used by the generated Allure 3 report
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty groupBy(String value) {
            setProperty("allure.groupBy", value);
            return this;
        }

    }

}
