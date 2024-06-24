package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/Timeouts.properties", "file:src/main/resources/properties/default/Timeouts.properties", "classpath:Timeouts.properties"})
public interface Timeouts extends EngineProperties<Timeouts> {
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

    @Key("browserNavigationTimeout")
    @DefaultValue("60")
    int browserNavigationTimeout();

    @Key("pageLoadTimeout")
    @DefaultValue("60")
    int pageLoadTimeout();

    @Key("scriptExecutionTimeout")
    @DefaultValue("60")
    int scriptExecutionTimeout();

    @Key("defaultElementIdentificationTimeout")
    @DefaultValue("60")
    double defaultElementIdentificationTimeout();

    @Key("apiSocketTimeout")
    @DefaultValue("30")
    int apiSocketTimeout();

    @Key("apiConnectionTimeout")
    @DefaultValue("30")
    int apiConnectionTimeout();

    @Key("apiConnectionManagerTimeout")
    @DefaultValue("30")
    int apiConnectionManagerTimeout();

    @Key("shellSessionTimeout")
    @DefaultValue("30")
    long shellSessionTimeout();

    @Key("dockerCommandTimeout")
    @DefaultValue("30")
    int dockerCommandTimeout();

    @Key("databaseLoginTimeout")
    @DefaultValue("30")
    int databaseLoginTimeout();

    @Key("databaseNetworkTimeout")
    @DefaultValue("30")
    int databaseNetworkTimeout();

    @Key("databaseQueryTimeout")
    @DefaultValue("30")
    int databaseQueryTimeout();

    @Key("waitForRemoteServerToBeUp")
    @DefaultValue("false")
    Boolean waitForRemoteServerToBeUp();

    @Key("timeoutForRemoteServerToBeUp")
    @DefaultValue("1")
    int timeoutForRemoteServerToBeUp();

    @Key("remoteServerInstanceCreationTimeout")
    @DefaultValue("1")
    int remoteServerInstanceCreationTimeout();

    @Key("waitUntilTimeout")
    @DefaultValue("60")
    int waitUntilTimeout();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty waitForLazyLoading(boolean value) {
            setProperty("waitForLazyLoading", String.valueOf(value));
            return this;
        }

        public SetProperty lazyLoadingTimeout(int value) {
            setProperty("lazyLoadingTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty browserNavigationTimeout(int value) {
            setProperty("browserNavigationTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty pageLoadTimeout(int value) {
            setProperty("pageLoadTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty scriptExecutionTimeout(int value) {
            setProperty("scriptExecutionTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty defaultElementIdentificationTimeout(double value) {
            setProperty("defaultElementIdentificationTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty apiSocketTimeout(int value) {
            setProperty("apiSocketTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty apiConnectionTimeout(int value) {
            setProperty("apiConnectionTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty apiConnectionManagerTimeout(int value) {
            setProperty("apiConnectionManagerTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty shellSessionTimeout(long value) {
            setProperty("shellSessionTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty dockerCommandTimeout(int value) {
            setProperty("dockerCommandTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty databaseLoginTimeout(int value) {
            setProperty("databaseLoginTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty databaseNetworkTimeout(int value) {
            setProperty("databaseNetworkTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty databaseQueryTimeout(int value) {
            setProperty("databaseQueryTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty waitForRemoteServerToBeUp(boolean value) {
            setProperty("waitForRemoteServerToBeUp", String.valueOf(value));
            return this;
        }

        public SetProperty timeoutForRemoteServerToBeUp(int value) {
            setProperty("timeoutForRemoteServerToBeUp", String.valueOf(value));
            return this;
        }

        public SetProperty remoteServerInstanceCreationTimeout(int value) {
            setProperty("remoteServerInstanceCreationTimeout", String.valueOf(value));
            return this;
        }

        public SetProperty waitUntilTimeout(int value) {
            setProperty("waitUntilTimeout", String.valueOf(value));
            return this;
        }

    }

}
