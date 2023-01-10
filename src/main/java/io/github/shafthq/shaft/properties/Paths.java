package io.github.shafthq.shaft.properties;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@Sources({"system:properties",
        "file:src/main/resources/properties/path.properties",
        "file:src/main/resources/properties/default/path.properties",
        "classpath:path.properties",
})
public interface Paths extends EngineProperties {
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
    @DefaultValue("target/downloadedFiles/")
    String downloads();

    @Key("allureResultsFolderPath")
    @DefaultValue("allure-results/")
    String allureResults();

    @Key("extentReportsFolderPath")
    @DefaultValue("extent-reports/")
    String extentReports();

    @Key("video.folder")
    @DefaultValue("allure-results/videos")
    String video();

    @Key("applitoolsApiKey")
    @DefaultValue("")
    String applitoolsApiKey();

    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.paths = ConfigFactory.create(Paths.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void properties(String value) {
            setProperty("propertiesFolderPath", value);
        }

        public void dynamicObjectRepository(String value) {
            setProperty("dynamicObjectRepositoryPath", value);
        }

        public void testData(String value) {
            setProperty("testDataFolderPath", value);
        }

        public void downloads(String value) {
            setProperty("downloadsFolderPath", value);
        }

        public void allureResults(String value) {
            setProperty("allureResultsFolderPath", value);
        }

        public void extentReports(String value) {
            setProperty("extentReportsFolderPath", value);
        }

        public void video(String value) {
            setProperty("video.folder", value);
        }

        public void applitoolsApiKey(String value) {
            setProperty("applitoolsApiKey", value);
        }
    }
}
