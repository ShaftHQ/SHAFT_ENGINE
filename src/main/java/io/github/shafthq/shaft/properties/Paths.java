package io.github.shafthq.shaft.properties;

import org.aeonbits.owner.Config;
import org.aeonbits.owner.ConfigFactory;

@Config.Sources({"system:properties",
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

    @Override
    default void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.paths = ConfigFactory.create(Paths.class, updatedProps);
    }
}
