package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for file-system path settings in the SHAFT framework.
 * Controls base directories for test data, downloads, and generated reports.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.paths.set().testData("src/test/resources/testDataFiles/");
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/path.properties",
        "file:src/main/resources/properties/default/path.properties",
        "classpath:path.properties",
})
public interface Paths extends EngineProperties<Paths> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.pathsOverride.set(ConfigFactory.create(Paths.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * Path to the custom properties files folder.
     *
     * <p>Default: {@code src/main/resources/properties/}.
     *
     * @return the configured value of {@code propertiesFolderPath}
     */
    @Key("propertiesFolderPath")
    @DefaultValue("src/main/resources/properties/")
    String properties();

    /**
     * Path to the default properties files folder.
     *
     * <p>Default: {@code src/main/resources/properties/default}.
     *
     * @return the configured value of {@code defaultPropertiesFolderPath}
     */
    @Key("defaultPropertiesFolderPath")
    @DefaultValue("src/main/resources/properties/default")
    String defaultProperties();

    /**
     * Workspace root for SHAFT agent tooling.
     *
     * @return the configured value of {@code aiAgentWorkspaceRoot}
     */
    @Key("aiAgentWorkspaceRoot")
    @DefaultValue("")
    String aiAgentWorkspaceRoot();

    /**
     * Path to the dynamic object repository folder.
     *
     * <p>Default: {@code src/main/resources/dynamicObjectRepository/}.
     *
     * @return the configured value of {@code dynamicObjectRepositoryPath}
     */
    @Key("dynamicObjectRepositoryPath")
    @DefaultValue("src/main/resources/dynamicObjectRepository/")
    String dynamicObjectRepository();

    /**
     * Folder that holds saved ARIA-snapshot baselines used for accessibility-tree comparisons;
     * regenerate with -Dshaft.updateSnapshots=true.
     *
     * <p>Default: {@code src/test/resources/aria/}.
     *
     * @return the configured value of {@code ariaSnapshotFolderPath}
     */
    @Key("ariaSnapshotFolderPath")
    @DefaultValue("src/test/resources/aria/")
    String ariaSnapshot();

    /**
     * Path to the test data files folder.
     *
     * <p>Default: {@code src/test/resources/testDataFiles/}.
     *
     * @return the configured value of {@code testDataFolderPath}
     */
    @Key("testDataFolderPath")
    @DefaultValue("src/test/resources/testDataFiles/")
    String testData();

    /**
     * Path to the folder for downloaded files.
     *
     * <p>Default: {@code target/downloadedFiles}.
     *
     * @return the configured value of {@code downloadsFolderPath}
     */
    @Key("downloadsFolderPath")
    @DefaultValue("target/downloadedFiles")
    String downloads();

    /**
     * Path to the Allure results output folder.
     *
     * <p>Default: {@code allure-results/}.
     *
     * @return the configured value of {@code allureResultsFolderPath}
     */
    @Key("allureResultsFolderPath")
    @DefaultValue("allure-results/")
    String allureResults();

    /**
     * Path to the Extent reports output folder.
     *
     * <p>Default: {@code extent-reports/}.
     *
     * @return the configured value of {@code extentReportsFolderPath}
     */
    @Key("extentReportsFolderPath")
    @DefaultValue("extent-reports/")
    String extentReports();

    /**
     * Path to the execution summary report folder.
     *
     * <p>Default: {@code execution-summary/}.
     *
     * @return the configured value of {@code executionSummaryReportFolderPath}
     */
    @Key("executionSummaryReportFolderPath")
    @DefaultValue("execution-summary/")
    String executionSummaryReport();

    /**
     * Path to the performance (Lighthouse) report output folder.
     *
     * <p>Default: {@code performanceReport/}.
     *
     * @return the configured value of {@code PerformanceReportFolderPath}
     */
    @Key("PerformanceReportFolderPath")
    @DefaultValue("performanceReport/")
    String performanceReportPath();


    /**
     * Path to the folder where recorded videos and generated animated GIF files are saved.
     *
     * <p>Default: {@code allure-results/videos}.
     *
     * @return the configured value of {@code video.folder}
     */
    @Key("video.folder")
    @DefaultValue("allure-results/videos")
    String video();

    /**
     * Applitools API key for visual AI testing integration.
     *
     * @return the configured value of {@code applitoolsApiKey}
     */
    @Key("applitoolsApiKey")
    @DefaultValue("")
    String applitoolsApiKey();

    /**
     * Path to the META-INF services folder for custom service implementations.
     *
     * <p>Default: {@code src/test/resources/META-INF/services/}.
     *
     * @return the configured value of {@code servicesFolderPath}
     */
    @Key("servicesFolderPath")
    @DefaultValue("src/test/resources/META-INF/services/")
    String services();

    /**
     * Folder that holds {@code SHAFT.Auth.setup(name, flow)} storage-state cache files (one JSON file
     * per {@code name}, using the same schema as {@code BrowserActions.saveStorageState}).
     */
    @Key("authCacheFolderPath")
    @DefaultValue("target/auth-cache/")
    String authCache();

    /**
     * Folder that holds {@code TouchActions.saveSessionCapabilities}/{@code saveAppState} JSON
     * snapshots used to reuse Appium session capabilities and mobile app state across test runs.
     */
    @Key("mobileSessionCacheFolderPath")
    @DefaultValue("target/mobile-session-cache/")
    String mobileSessionCache();

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
         * Overrides the {@code propertiesFolderPath} property at runtime. Path to the custom properties
         * files folder.
         *
         * @param value the new value of {@code propertiesFolderPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty properties(String value) {
            setProperty("propertiesFolderPath", value);
            return this;
        }

        /**
         * Overrides the {@code aiAgentWorkspaceRoot} property at runtime. Workspace root for SHAFT agent
         * tooling.
         *
         * @param value the new value of {@code aiAgentWorkspaceRoot}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty aiAgentWorkspaceRoot(String value) {
            setProperty("aiAgentWorkspaceRoot", value);
            return this;
        }

        /**
         * Overrides the {@code dynamicObjectRepositoryPath} property at runtime. Path to the dynamic
         * object repository folder.
         *
         * @param value the new value of {@code dynamicObjectRepositoryPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty dynamicObjectRepository(String value) {
            setProperty("dynamicObjectRepositoryPath", value);
            return this;
        }

        /**
         * Overrides the {@code ariaSnapshotFolderPath} property at runtime. Folder that holds saved ARIA-
         * snapshot baselines used for accessibility-tree comparisons; regenerate with
         * -Dshaft.updateSnapshots=true.
         *
         * @param value the new value of {@code ariaSnapshotFolderPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty ariaSnapshot(String value) {
            setProperty("ariaSnapshotFolderPath", value);
            return this;
        }

        /**
         * Overrides the {@code testDataFolderPath} property at runtime. Path to the test data files
         * folder.
         *
         * @param value the new value of {@code testDataFolderPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty testData(String value) {
            setProperty("testDataFolderPath", value);
            return this;
        }

        /**
         * Overrides the {@code downloadsFolderPath} property at runtime. Path to the folder for downloaded
         * files.
         *
         * @param value the new value of {@code downloadsFolderPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty downloads(String value) {
            setProperty("downloadsFolderPath", value);
            return this;
        }

        /**
         * Overrides the {@code allureResultsFolderPath} property at runtime. Path to the Allure results
         * output folder.
         *
         * @param value the new value of {@code allureResultsFolderPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty allureResults(String value) {
            setProperty("allureResultsFolderPath", value);
            return this;
        }

        /**
         * Overrides the {@code extentReportsFolderPath} property at runtime. Path to the Extent reports
         * output folder.
         *
         * @param value the new value of {@code extentReportsFolderPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty extentReports(String value) {
            setProperty("extentReportsFolderPath", value);
            return this;
        }

        /**
         * Overrides the {@code executionSummaryReportFolderPath} property at runtime. Path to the
         * execution summary report folder.
         *
         * @param value the new value of {@code executionSummaryReportFolderPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty executionSummaryReport(String value) {
            setProperty("executionSummaryReportFolderPath", value);
            return this;
        }

        /**
         * Overrides the {@code video.folder} property at runtime. Path to the folder where recorded videos
         * and generated animated GIF files are saved.
         *
         * @param value the new value of {@code video.folder}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty video(String value) {
            setProperty("video.folder", value);
            return this;
        }

        /**
         * Overrides the {@code applitoolsApiKey} property at runtime. Applitools API key for visual AI
         * testing integration.
         *
         * @param value the new value of {@code applitoolsApiKey}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty applitoolsApiKey(String value) {
            setProperty("applitoolsApiKey", value);
            return this;
        }

        /**
         * Overrides the {@code servicesFolderPath} property at runtime. Path to the META-INF services
         * folder for custom service implementations.
         *
         * @param value the new value of {@code servicesFolderPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty services(String value) {
            setProperty("servicesFolderPath", value);
            return this;
        }

        /**
         * Overrides the {@code authCacheFolderPath} property at runtime. Folder that holds
         * SHAFT.Auth.setup(name, flow) storage-state cache files (one JSON file per name).
         *
         * @param value the new value of {@code authCacheFolderPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty authCache(String value) {
            setProperty("authCacheFolderPath", value);
            return this;
        }

        /**
         * Overrides the {@code mobileSessionCacheFolderPath} property at runtime. Folder that holds
         * TouchActions.saveSessionCapabilities/saveAppState JSON snapshots used to reuse Appium session
         * capabilities and mobile app state across test runs.
         *
         * @param value the new value of {@code mobileSessionCacheFolderPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty mobileSessionCache(String value) {
            setProperty("mobileSessionCacheFolderPath", value);
            return this;
        }

    }
}
