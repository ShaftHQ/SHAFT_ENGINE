package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for Google Tink encryption settings in the SHAFT framework.
 * Controls key URI, credentials, and keyset path used for test-data encryption and decryption.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.tinkey.set().keysetFilename("keyset.json");
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/tinkey.properties", "file:src/main/resources/properties/default/tinkey.properties", "classpath:tinkey.properties"})
public interface Tinkey extends EngineProperties<Tinkey> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.tinkeyOverride.set(ConfigFactory.create(Tinkey.class, ThreadLocalPropertiesManager.getOverrides()));
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
