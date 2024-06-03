package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/tinkey.properties", "file:src/main/resources/properties/default/tinkey.properties", "classpath:tinkey.properties"})
public interface Tinkey extends EngineProperties<Tinkey> {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.tinkey = ConfigFactory.create(Tinkey.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("tinkey.keysetFilename")
    @DefaultValue("")
    String keysetFilename();

    @Key("tinkey.kms.serverType")
    @DefaultValue("")
    String kmsServerType();

    @Key("tinkey.kms.credentialPath")
    @DefaultValue("")
    String kmsCredentialPath();

    @Key("tinkey.kms.masterKeyUri")
    @DefaultValue("")
    String kmsMasterKeyUri();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty keysetFilename(String value) {
            setProperty("tinkey.keysetFilename", value);
            return this;
        }

        public SetProperty kmsServerType(String value) {
            setProperty("tinkey.kms.serverType", value);
            return this;
        }

        public SetProperty kmsCredentialPath(String value) {
            setProperty("tinkey.kms.credentialPath", value);
            return this;
        }

        public SetProperty kmsMasterKeyUri(String value) {
            setProperty("tinkey.kms.masterKeyUri", value);
            return this;
        }
    }
}
