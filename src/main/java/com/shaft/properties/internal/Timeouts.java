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
    Boolean waitForLazyLoading();

    @Key("lazyLoadingTimeout")
    @DefaultValue("30")
    Integer lazyLoadingTimeout();

    @Key("browserNavigationTimeout")
    @DefaultValue("60")
    Integer browserNavigationTimeout();

    @Key("pageLoadTimeout")
    @DefaultValue("60")
    Integer pageLoadTimeout();

    @Key("scriptExecutionTimeout")
    @DefaultValue("30")
    Integer scriptExecutionTimeout();

    @Key("defaultElementIdentificationTimeout")
    @DefaultValue("60")
    Integer defaultElementIdentificationTimeout();

    @Key("apiSocketTimeout")
    @DefaultValue("30")
    Integer apiSocketTimeout();

    @Key("apiConnectionTimeout")
    @DefaultValue("30")
    Integer apiConnectionTimeout();

    @Key("apiConnectionManagerTimeout")
    @DefaultValue("30")
    Integer apiConnectionManagerTimeout();

    @Key("shellSessionTimeout")
    @DefaultValue("30")
    Integer shellSessionTimeout();

    @Key("dockerCommandTimeout")
    @DefaultValue("30")
    Integer dockerCommandTimeout();

    @Key("databaseLoginTimeout")
    @DefaultValue("30")
    Integer databaseLoginTimeout();

    @Key("databaseNetworkTimeout")
    @DefaultValue("30")
    Integer databaseNetworkTimeout();

    @Key("databaseQueryTimeout")
    @DefaultValue("30")
    Integer databaseQueryTimeout();

    @Key("waitForRemoteServerToBeUp")
    @DefaultValue("false")
    Boolean waitForRemoteServerToBeUp();

    @Key("timeoutForRemoteServerToBeUp")
    @DefaultValue("10")
    Integer timeoutForRemoteServerToBeUp();

    @Key("remoteServerInstanceCreationTimeout")
    @DefaultValue("10")
    Integer remoteServerInstanceCreationTimeout();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void waitForLazyLoading(boolean value) {
            setProperty("waitForLazyLoading", String.valueOf(value));
        }

        public void lazyLoadingTimeout(int value) {
            setProperty("lazyLoadingTimeout", String.valueOf(value));
        }

        public void browserNavigationTimeout(int value) {
            setProperty("browserNavigationTimeout", String.valueOf(value));
        }

        public void pageLoadTimeout(int value) {
            setProperty("pageLoadTimeout", String.valueOf(value));
        }

        public void scriptExecutionTimeout(int value) {
            setProperty("scriptExecutionTimeout", String.valueOf(value));
        }

        public void defaultElementIdentificationTimeout(int value) {
            setProperty("defaultElementIdentificationTimeout", String.valueOf(value));
        }

        public void apiSocketTimeout(int value) {
            setProperty("apiSocketTimeout", String.valueOf(value));
        }

        public void apiConnectionTimeout(int value) {
            setProperty("apiConnectionTimeout", String.valueOf(value));
        }

        public void apiConnectionManagerTimeout(int value) {
            setProperty("apiConnectionManagerTimeout", String.valueOf(value));
        }

        public void shellSessionTimeout(int value) {
            setProperty("shellSessionTimeout", String.valueOf(value));
        }

        public void dockerCommandTimeout(int value) {
            setProperty("dockerCommandTimeout", String.valueOf(value));
        }

        public void databaseLoginTimeout(int value) {
            setProperty("databaseLoginTimeout", String.valueOf(value));
        }

        public void databaseNetworkTimeout(int value) {
            setProperty("databaseNetworkTimeout", String.valueOf(value));
        }

        public void databaseQueryTimeout(int value) {
            setProperty("databaseQueryTimeout", String.valueOf(value));
        }

        public void waitForRemoteServerToBeUp(boolean value) {
            setProperty("waitForRemoteServerToBeUp", String.valueOf(value));
        }

        public void timeoutForRemoteServerToBeUp(int value) {
            setProperty("timeoutForRemoteServerToBeUp", String.valueOf(value));
        }

        public void remoteServerInstanceCreationTimeout(int value) {
            setProperty("remoteServerInstanceCreationTimeout", String.valueOf(value));
        }

    }

}
