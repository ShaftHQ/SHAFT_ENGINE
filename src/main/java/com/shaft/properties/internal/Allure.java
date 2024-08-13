package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/Allure.properties", "file:src/main/resources/properties/default/Allure.properties", "classpath:Allure.properties"})
public interface Allure extends EngineProperties<Allure> {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.allure = ConfigFactory.create(Allure.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        if (!key.equals("disableLogging"))
            ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("automaticallyOpen")
    @DefaultValue("true")
    boolean automaticallyOpen(); //automaticallyOpen (used to be: openAllureReportAfterExecution)

    @Key("accumulateHistory")
    @DefaultValue("false")
    boolean accumulateHistory(); //accumulateHistory (used to be: cleanAllureResultsDirectoryBeforeExecution)

    @Key("accumulateReports")
    @DefaultValue("true")
    boolean accumulateReports(); //allows html files to accumulate in the allure report directory

    @Key("generateArchive")
    @DefaultValue("false")
    boolean generateArchive(); //generateArchive (used to be: generateAllureReportArchive)

    @Key("customLogo")
    @DefaultValue("https://github.com/ShaftHQ/SHAFT_ENGINE/blob/main/src/main/resources/images/shaft_white.png?raw=true")
    String customLogo();

    @Key("customTitle")
    @DefaultValue("Test run report")
    String customTitle();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {

        public SetProperty automaticallyOpen(boolean value) {
            setProperty("automaticallyOpen", String.valueOf(value));
            setProperty("openAllureReportAfterExecution", String.valueOf(value));
            return this;
        }

        public SetProperty accumulateHistory(boolean value) {
            setProperty("accumulateHistory", String.valueOf(value));
            setProperty("cleanAllureResultsDirectoryBeforeExecution", String.valueOf(value));
            return this;
        }

        public SetProperty accumulateReports(boolean value) {
            setProperty("accumulateReports", String.valueOf(value));
            return this;
        }

        public SetProperty generateArchive(boolean value) {
            setProperty("generateArchive", String.valueOf(value));
            setProperty("generateAllureReportArchive", String.valueOf(value));
            return this;
        }

        public SetProperty customLogo(String value) {
            setProperty("customLogo", value);
            return this;
        }

        public SetProperty customTitle(String value) {
            setProperty("customTitle", value);
            return this;
        }

    }

}
