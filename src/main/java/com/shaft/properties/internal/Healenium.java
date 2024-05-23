package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/healenium.properties", "file:src/main/resources/properties/default/healenium.properties", "classpath:healenium.properties",})
public interface Healenium extends EngineProperties<Healenium> {
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
    int recoveryTries();

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
    int serverPort();

    @Key("imitatePort")
    @DefaultValue("8000")
    int imitatePort();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty recoveryTries(int value) {
            setProperty("recovery-tries", String.valueOf(value));
            return this;
        }

        public SetProperty scoreCap(String value) {
            setProperty("score-cap", value);
            return this;
        }

        public SetProperty healEnabled(boolean value) {
            setProperty("heal-enabled", String.valueOf(value));
            return this;
        }

        public SetProperty serverHost(String value) {
            setProperty("serverHost", value);
            return this;
        }

        public SetProperty serverPort(int value) {
            setProperty("serverPort", String.valueOf(value));
            return this;
        }

        public SetProperty imitatePort(int value) {
            setProperty("imitatePort", String.valueOf(value));
            return this;
        }

    }

}
