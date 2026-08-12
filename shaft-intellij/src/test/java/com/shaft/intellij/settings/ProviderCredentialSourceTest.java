package com.shaft.intellij.settings;

import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ProviderCredentialSourceTest {
    @Test
    void detectsProviderStandardVariablesByDocumentedPrecedenceWithoutExposingValues() {
        Map<String, String> environment = Map.of(
                "GEMINI_API_KEY", "legacy-secret",
                "GOOGLE_API_KEY", "preferred-secret");

        ProviderCredentialSource.Source source = ProviderCredentialSource.detect("gemini", environment).orElseThrow();

        assertEquals("GOOGLE_API_KEY", source.variableName());
        assertEquals("Use configured GOOGLE_API_KEY", source.label());
        assertEquals("Source[variableName=GOOGLE_API_KEY]", source.toString());
        assertTrue(ProviderCredentialSource.variables("openai").contains("OPENAI_API_KEY"));
        assertTrue(ProviderCredentialSource.variables("github").contains("GH_TOKEN"));
    }

    @Test
    void blankVariablesAreAbsent() {
        assertTrue(ProviderCredentialSource.detect("anthropic", Map.of("ANTHROPIC_API_KEY", " ")).isEmpty());
    }
}
