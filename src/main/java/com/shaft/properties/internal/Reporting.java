package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/Reporting.properties", "file:src/main/resources/properties/default/Reporting.properties", "classpath:Reporting.properties"})
public interface Reporting extends EngineProperties<Reporting> {
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

    @Key("openLighthouseReportWhileExecution")
    @DefaultValue("false")
    boolean openLighthouseReportWhileExecution();

    @Key("cleanSummaryReportsDirectoryBeforeExecution")
    @DefaultValue("true")
    boolean cleanSummaryReportsDirectoryBeforeExecution();

    @Key("openExecutionSummaryReportAfterExecution")
    @DefaultValue("false")
    boolean openExecutionSummaryReportAfterExecution();

    @Key("disableLogging")
    @DefaultValue("true")
    boolean disableLogging();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {

        public SetProperty captureElementName(boolean value) {
            setProperty("captureElementName", String.valueOf(value));
            return this;
        }

        public SetProperty forceCheckForElementVisibility(boolean value) {
            setProperty("forceCheckForElementVisibility", String.valueOf(value));
            return this;
        }

        public SetProperty captureWebDriverLogs(boolean value) {
            setProperty("captureWebDriverLogs", String.valueOf(value));
            return this;
        }

        public SetProperty alwaysLogDiscreetly(boolean value) {
            setProperty("alwaysLogDiscreetly", String.valueOf(value));
            return this;
        }

        public SetProperty debugMode(boolean value) {
            setProperty("debugMode", String.valueOf(value));
            return this;
        }

        public SetProperty openLighthouseReportWhileExecution(boolean value) {
            setProperty("openLighthouseReportWhileExecution", String.valueOf(value));
            return this;
        }

        public SetProperty openExecutionSummaryReportAfterExecution(boolean value) {
            setProperty("openExecutionSummaryReportAfterExecution", String.valueOf(value));
            return this;
        }

        public SetProperty disableLogging(boolean value) {
            setProperty("disableLogging", String.valueOf(value));
            return this;
        }

    }

}
