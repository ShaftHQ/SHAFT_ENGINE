package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/path.properties",
        "file:src/main/resources/properties/default/path.properties",
        "classpath:path.properties",
})
public interface Paths extends EngineProperties<Paths> {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.paths = ConfigFactory.create(Paths.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("propertiesFolderPath")
    @DefaultValue("src/main/resources/properties/")
    String properties();

    @Key("defaultPropertiesFolderPath")
    @DefaultValue("src/main/resources/properties/default")
    String defaultProperties();

    @Key("dynamicObjectRepositoryPath")
    @DefaultValue("src/main/resources/dynamicObjectRepository/")
    String dynamicObjectRepository();

    @Key("testDataFolderPath")
    @DefaultValue("src/test/resources/testDataFiles/")
    String testData();

    @Key("downloadsFolderPath")
    @DefaultValue("target/downloadedFiles")
    String downloads();

    @Key("allureResultsFolderPath")
    @DefaultValue("allure-results/")
    String allureResults();

    @Key("extentReportsFolderPath")
    @DefaultValue("extent-reports/")
    String extentReports();

    @Key("executionSummaryReportFolderPath")
    @DefaultValue("execution-summary/")
    String executionSummaryReport();

    @Key("video.folder")
    @DefaultValue("allure-results/videos")
    String video();

    @Key("applitoolsApiKey")
    @DefaultValue("")
    String applitoolsApiKey();

    @Key("servicesFolderPath")
    @DefaultValue("src/test/resources/META-INF/services/")
    String services();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty properties(String value) {
            setProperty("propertiesFolderPath", value);
            return this;
        }

        public SetProperty dynamicObjectRepository(String value) {
            setProperty("dynamicObjectRepositoryPath", value);
            return this;
        }

        public SetProperty testData(String value) {
            setProperty("testDataFolderPath", value);
            return this;
        }

        public SetProperty downloads(String value) {
            setProperty("downloadsFolderPath", value);
            return this;
        }

        public SetProperty allureResults(String value) {
            setProperty("allureResultsFolderPath", value);
            return this;
        }

        public SetProperty extentReports(String value) {
            setProperty("extentReportsFolderPath", value);
            return this;
        }

        public SetProperty executionSummaryReport(String value) {
            setProperty("executionSummaryReportFolderPath", value);
            return this;
        }

        public SetProperty video(String value) {
            setProperty("video.folder", value);
            return this;
        }

        public SetProperty applitoolsApiKey(String value) {
            setProperty("applitoolsApiKey", value);
            return this;
        }

        public SetProperty services(String value) {
            setProperty("servicesFolderPath", value);
            return this;
        }

    }
}
