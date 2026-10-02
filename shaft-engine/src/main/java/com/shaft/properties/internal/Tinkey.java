package com.shaft.properties.internal;

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
        EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * Path to the Google Tink keyset file used to encrypt/decrypt sensitive test data.
     *
     * @return the configured value of {@code tinkey.keysetFilename}
     */
    @Key("tinkey.keysetFilename")
    @DefaultValue("")
    String keysetFilename();

    /**
     * Key Management Service provider backing the Tink keyset (e.g. gcp-kms, aws-kms).
     *
     * @return the configured value of {@code tinkey.kms.serverType}
     */
    @Key("tinkey.kms.serverType")
    @DefaultValue("")
    String kmsServerType();

    /**
     * Path to the KMS provider credential file used to access the remote master key.
     *
     * @return the configured value of {@code tinkey.kms.credentialPath}
     */
    @Key("tinkey.kms.credentialPath")
    @DefaultValue("")
    String kmsCredentialPath();

    /**
     * URI of the remote KMS master key used to encrypt/decrypt the local Tink keyset.
     *
     * @return the configured value of {@code tinkey.kms.masterKeyUri}
     */
    @Key("tinkey.kms.masterKeyUri")
    @DefaultValue("")
    String kmsMasterKeyUri();

    /**
     * Starts a fluent, thread-local override of these properties for the current test thread.
     *
     * @return a new {@link SetProperty} builder
     */
    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        /**
         * Overrides the {@code tinkey.keysetFilename} property at runtime. Path to the Google Tink keyset
         * file used to encrypt/decrypt sensitive test data.
         *
         * @param value the new value of {@code tinkey.keysetFilename}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty keysetFilename(String value) {
            setProperty("tinkey.keysetFilename", value);
            return this;
        }

        /**
         * Overrides the {@code tinkey.kms.serverType} property at runtime. Key Management Service provider
         * backing the Tink keyset (e.g.
         *
         * @param value the new value of {@code tinkey.kms.serverType}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty kmsServerType(String value) {
            setProperty("tinkey.kms.serverType", value);
            return this;
        }

        /**
         * Overrides the {@code tinkey.kms.credentialPath} property at runtime. Path to the KMS provider
         * credential file used to access the remote master key.
         *
         * @param value the new value of {@code tinkey.kms.credentialPath}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty kmsCredentialPath(String value) {
            setProperty("tinkey.kms.credentialPath", value);
            return this;
        }

        /**
         * Overrides the {@code tinkey.kms.masterKeyUri} property at runtime. URI of the remote KMS master
         * key used to encrypt/decrypt the local Tink keyset.
         *
         * @param value the new value of {@code tinkey.kms.masterKeyUri}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty kmsMasterKeyUri(String value) {
            setProperty("tinkey.kms.masterKeyUri", value);
            return this;
        }
    }
}
