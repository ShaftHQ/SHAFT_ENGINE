package com.shaft.intellij.settings;

import com.intellij.credentialStore.CredentialAttributes;
import com.intellij.credentialStore.Credentials;
import com.intellij.ide.passwordSafe.PasswordSafe;
import com.intellij.openapi.application.ApplicationManager;

import java.util.Arrays;

/**
 * Stores optional provider keys in IntelliJ Password Safe.
 */
public final class ShaftCredentialService {
    private static final String SERVICE_PREFIX = "SHAFT Autobot ";

    /**
     * Returns the application-level credential service.
     *
     * @return credential service
     */
    public static ShaftCredentialService getInstance() {
        return ApplicationManager.getApplication().getService(ShaftCredentialService.class);
    }

    /**
     * Stores or clears a provider API key.
     *
     * @param provider provider id
     * @param secret API key characters
     */
    public void setApiKey(String provider, char[] secret) {
        try {
            String value = secret == null ? "" : new String(secret);
            PasswordSafe.getInstance().set(attributes(provider),
                    value.isBlank() ? null : new Credentials(provider, value));
        } finally {
            if (secret != null) {
                Arrays.fill(secret, '\0');
            }
        }
    }

    /**
     * Reads a provider API key.
     *
     * @param provider provider id
     * @return API key or empty string
     */
    public String apiKey(String provider) {
        Credentials credentials = PasswordSafe.getInstance().get(attributes(provider));
        return credentials == null ? "" : credentials.getPasswordAsString();
    }

    /**
     * Indicates whether a provider API key is currently stored.
     *
     * @param provider provider id
     * @return true if a non-blank API key is stored for the provider
     */
    public boolean hasApiKey(String provider) {
        Credentials credentials = PasswordSafe.getInstance().get(attributes(provider));
        if (credentials == null) {
            return false;
        }
        String password = credentials.getPasswordAsString();
        return password != null && !password.isBlank();
    }

    private static CredentialAttributes attributes(String provider) {
        return new CredentialAttributes(SERVICE_PREFIX + provider);
    }
}
