package com.shaft.intellij.settings;

import com.intellij.credentialStore.CredentialAttributes;
import com.intellij.credentialStore.Credentials;
import com.intellij.ide.passwordSafe.PasswordSafe;
import com.intellij.openapi.application.ApplicationManager;

import java.util.Arrays;
import java.util.List;
import java.util.function.BiConsumer;

/**
 * Stores optional provider keys in IntelliJ Password Safe.
 */
public final class ShaftCredentialService {
    private static final String SERVICE_PREFIX = "SHAFT Autobot ";

    /**
     * Every provider id the plugin ever writes to Password Safe, enumerated in one place so a
     * factory reset (see {@code ShaftPluginResetService}) can clear all of them without missing one.
     */
    public static final List<String> KNOWN_PROVIDERS = List.of(
            "OPENAI_API_KEY",
            "ANTHROPIC_API_KEY",
            "GEMINI_API_KEY",
            "GITHUB_TOKEN");

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

    /**
     * Clears every provider key the plugin ever writes (see {@link #KNOWN_PROVIDERS}).
     */
    public void clearAll() {
        clearAll(this::setApiKey);
    }

    /**
     * Runs the clear-all algorithm against the given setter, decoupled from {@link #setApiKey} and
     * Password Safe so the loop over {@link #KNOWN_PROVIDERS} can be executed and verified by a
     * plain unit test (Password Safe requires a running IntelliJ Application).
     *
     * @param setter invoked with each known provider id and a {@code null} secret to clear it
     */
    static void clearAll(BiConsumer<String, char[]> setter) {
        for (String provider : KNOWN_PROVIDERS) {
            setter.accept(provider, null);
        }
    }

    private static CredentialAttributes attributes(String provider) {
        return new CredentialAttributes(SERVICE_PREFIX + provider);
    }
}
