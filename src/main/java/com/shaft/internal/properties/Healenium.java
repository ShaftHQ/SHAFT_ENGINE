package com.shaft.internal.properties;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/healenium.properties", "file:src/main/resources/properties/default/healenium.properties", "classpath:healenium.properties",})
public interface Healenium extends EngineProperties {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.healenium = ConfigFactory.create(Healenium.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("recovery-tries")
    @DefaultValue("1")
    String recoveryTries();

    @Key("score-cap")
    @DefaultValue("0.5")
    String scoreCap();

    @Key("heal-enabled")
    @DefaultValue("false")
    boolean healEnabled();

    @Key("serverHost")
    @DefaultValue("localhost")
    String serverHost();

    @Key("serverPort")
    @DefaultValue("7878")
    String serverPort();

    @Key("imitatePort")
    @DefaultValue("8000")
    String imitatePort();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void recoveryTries(String value) {
            setProperty("recovery-tries", value);
        }

        public void scoreCap(String value) {
            setProperty("score-cap", value);
        }

        public void healEnabled(boolean value) {
            setProperty("heal-enabled", String.valueOf(value));
        }

        public void serverHost(String value) {
            setProperty("serverHost", value);
        }

        public void serverPort(String value) {
            setProperty("serverPort", value);
        }

        public void imitatePort(String value) {
            setProperty("imitatePort", value);
        }

    }

}
