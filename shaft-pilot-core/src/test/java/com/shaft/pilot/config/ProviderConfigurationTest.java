package com.shaft.pilot.config;

import com.shaft.pilot.ai.ProcessingLocation;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ProviderConfigurationTest {
    @Test
    void compactConstructorDefaultsLocationByProviderId() {
        ProviderConfiguration local = new ProviderConfiguration(
                "ollama", URI.create("http://localhost:11434"), " model ", "", Map.of("header", "value"));
        ProviderConfiguration remote = new ProviderConfiguration(
                "openai", URI.create("https://example.test"), null, null, null);

        assertEquals(ProcessingLocation.LOCAL, local.processingLocation());
        assertEquals("model", local.model());
        assertEquals("value", local.option("header"));
        assertEquals("", local.option("missing"));
        assertEquals(ProcessingLocation.REMOTE, remote.processingLocation());
        assertEquals("", remote.model());
        assertEquals("", remote.apiKeyEnvironmentVariable());
    }

    @Test
    void canonicalConstructorDefaultsNullLocationAndCopiesOptions() {
        Map<String, String> options = new HashMap<>();
        options.put("base", "one");

        ProviderConfiguration configuration = new ProviderConfiguration(
                "custom", URI.create("https://example.test"), "model", " API_KEY ",
                null, options);
        options.put("base", "two");

        assertEquals(ProcessingLocation.NONE, configuration.processingLocation());
        assertEquals("API_KEY", configuration.apiKeyEnvironmentVariable());
        assertEquals("one", configuration.option("base"));
        assertThrows(UnsupportedOperationException.class, () -> configuration.options().put("x", "y"));
    }

    @Test
    void requiredFieldsRejectNulls() {
        assertThrows(NullPointerException.class,
                () -> new ProviderConfiguration(null, URI.create("https://example.test"), "", "", Map.of()));
        assertThrows(NullPointerException.class,
                () -> new ProviderConfiguration("id", null, "", "", Map.of()));
    }
}
