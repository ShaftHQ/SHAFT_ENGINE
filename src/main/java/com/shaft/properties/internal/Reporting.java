package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/Reporting.properties", "file:src/main/resources/properties/default/Reporting.properties", "classpath:Reporting.properties"})
public interface Reporting extends EngineProperties {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.reporting = ConfigFactory.create(Reporting.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        if (!key.equals("disableLogging"))
            ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("captureElementName")
    @DefaultValue("true")
    boolean captureElementName();

    @Key("captureWebDriverLogs")
    @DefaultValue("false")
    boolean captureWebDriverLogs();

    @Key("alwaysLogDiscreetly")
    @DefaultValue("false")
    boolean alwaysLogDiscreetly();

    @Key("debugMode")
    @DefaultValue("false")
    boolean debugMode();

    @Key("cleanAllureResultsDirectoryBeforeExecution")
    @DefaultValue("true")
    boolean cleanAllureResultsDirectoryBeforeExecution();

    @Key("generateAllureReportArchive")
    @DefaultValue("false")
    boolean generateAllureReportArchive();

    @Key("openAllureReportAfterExecution")
    @DefaultValue("false")
    boolean openAllureReportAfterExecution();

    @Key("generateExtentReports")
    @DefaultValue("true")
    boolean generateExtentReports();

    @Key("cleanExtentReportsDirectoryBeforeExecution")
    @DefaultValue("true")
    boolean cleanExtentReportsDirectoryBeforeExecution();

    @Key("attachExtentReportsToAllureReport")
    @DefaultValue("false")
    boolean attachExtentReportsToAllureReport();

    @Key("openLighthouseReportWhileExecution")
    @DefaultValue("false")
    boolean openLighthouseReportWhileExecution();

    @Key("openExecutionSummaryReportAfterExecution")
    @DefaultValue("true")
    boolean openExecutionSummaryReportAfterExecution();

    @Key("disableLogging")
    @DefaultValue("true")
    boolean disableLogging();
    
    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {

        public void captureElementName(boolean value) {
            setProperty("captureElementName", String.valueOf(value));
        }

        public void forceCheckForElementVisibility(boolean value) {
            setProperty("forceCheckForElementVisibility", String.valueOf(value));
        }

        public void captureWebDriverLogs(boolean value) {
            setProperty("captureWebDriverLogs", String.valueOf(value));
        }

        public void alwaysLogDiscreetly(boolean value) {
            setProperty("alwaysLogDiscreetly", String.valueOf(value));
        }

        public void debugMode(boolean value) {
            setProperty("debugMode", String.valueOf(value));
        }

        public void cleanAllureResultsDirectoryBeforeExecution(boolean value) {
            setProperty("cleanAllureResultsDirectoryBeforeExecution", String.valueOf(value));
        }

        public void generateAllureReportArchive(boolean value) {
            setProperty("generateAllureReportArchive", String.valueOf(value));
        }

        public void openAllureReportAfterExecution(boolean value) {
            setProperty("openAllureReportAfterExecution", String.valueOf(value));
        }

        public void generateExtentReports(boolean value) {
            setProperty("generateExtentReports", String.valueOf(value));
        }

        public void cleanExtentReportsDirectoryBeforeExecution(boolean value) {
            setProperty("cleanExtentReportsDirectoryBeforeExecution", String.valueOf(value));
        }

        public void attachExtentReportsToAllureReport(boolean value) {
            setProperty("attachExtentReportsToAllureReport", String.valueOf(value));
        }

        public void openLighthouseReportWhileExecution(boolean value) {
            setProperty("openLighthouseReportWhileExecution", String.valueOf(value));
        }

        public void openExecutionSummaryReportAfterExecution(boolean value) {
            setProperty("openExecutionSummaryReportAfterExecution", String.valueOf(value));
        }

        public void disableLogging(boolean value) {
            setProperty("disableLogging", String.valueOf(value));
        }

    }

}
