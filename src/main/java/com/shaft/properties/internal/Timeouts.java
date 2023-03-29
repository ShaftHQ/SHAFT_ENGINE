package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/Timeouts.properties", "file:src/main/resources/properties/default/Timeouts.properties", "classpath:Timeouts.properties"})
public interface Timeouts extends EngineProperties {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.timeouts = ConfigFactory.create(Timeouts.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("waitForLazyLoading")
    @DefaultValue("true")
    boolean waitForLazyLoading();

    @Key("lazyLoadingTimeout")
    @DefaultValue("30")
    String lazyLoadingTimeout();

    @Key("browserNavigationTimeout")
    @DefaultValue("60")
    String browserNavigationTimeout();

    @Key("pageLoadTimeout")
    @DefaultValue("60")
    String pageLoadTimeout();

    @Key("scriptExecutionTimeout")
    @DefaultValue("30")
    boolean scriptExecutionTimeout();

    @Key("defaultElementIdentificationTimeout")
    @DefaultValue("60")
    String defaultElementIdentificationTimeout();

    @Key("apiSocketTimeout")
    @DefaultValue("30")
    String apiSocketTimeout();

    @Key("apiConnectionTimeout")
    @DefaultValue("30")
    boolean apiConnectionTimeout();

    @Key("apiConnectionManagerTimeout")
    @DefaultValue("30")
    String apiConnectionManagerTimeout();

    @Key("shellSessionTimeout")
    @DefaultValue("30")
    boolean shellSessionTimeout();

    @Key("dockerCommandTimeout")
    @DefaultValue("30")
    String dockerCommandTimeout();

    @Key("databaseLoginTimeout")
    @DefaultValue("30")
    boolean databaseLoginTimeout();

    @Key("databaseNetworkTimeout")
    @DefaultValue("30")
    String databaseNetworkTimeout();

    @Key("databaseQueryTimeout")
    @DefaultValue("30")
    String databaseQueryTimeout();


    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void waitForLazyLoading(boolean value) {
            setProperty("waitForLazyLoading", String.valueOf(value));
        }

        public void lazyLoadingTimeout(String value) {
            setProperty("lazyLoadingTimeout", value);
        }

        public void browserNavigationTimeout(String value) {
            setProperty("browserNavigationTimeout", value);
        }

        public void pageLoadTimeout(String value) {
            setProperty("pageLoadTimeout", value);
        }

        public void scriptExecutionTimeout(String value) {
            setProperty("scriptExecutionTimeout", value);
        }

        public void defaultElementIdentificationTimeout(String value) {
            setProperty("defaultElementIdentificationTimeout", value);
        }

        public void apiSocketTimeout(String value) {
            setProperty("apiSocketTimeout", value);
        }

        public void apiConnectionTimeout(String value) {
            setProperty("apiConnectionTimeout", value);
        }

        public void apiConnectionManagerTimeout(String value) {
            setProperty("apiConnectionManagerTimeout", value);
        }

        public void shellSessionTimeout(String value) {
            setProperty("shellSessionTimeout", value);
        }

        public void dockerCommandTimeout(String value) {
            setProperty("dockerCommandTimeout", value);
        }

        public void databaseLoginTimeout(String value) {
            setProperty("databaseLoginTimeout", value);
        }

        public void databaseNetworkTimeout(String value) {
            setProperty("databaseNetworkTimeout", value);
        }

        public void databaseQueryTimeout(String value) {
            setProperty("databaseQueryTimeout", value);
        }


    }

}
