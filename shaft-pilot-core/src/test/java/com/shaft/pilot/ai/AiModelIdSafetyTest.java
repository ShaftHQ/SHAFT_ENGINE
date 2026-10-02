package com.shaft.pilot.ai;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #6375: provider model identifiers are exposed only when they cannot leak secrets or endpoints.
 */
class AiModelIdSafetyTest {
    @Test
    void acceptsOrdinaryModelIdentifiers() {
        assertTrue(AiModelDiscovery.isSafeModelId("gpt-4o-mini"));
        assertTrue(AiModelDiscovery.isSafeModelId("llama3.1:8b"));
    }

    @Test
    void rejectsPaddedUrlAndCredentialShapedIdentifiers() {
        assertFalse(AiModelDiscovery.isSafeModelId(null));
        assertFalse(AiModelDiscovery.isSafeModelId(" gpt-4o "));
        assertFalse(AiModelDiscovery.isSafeModelId("https://evil.test/model"));
        assertFalse(AiModelDiscovery.isSafeModelId("AKIAABCDEFGHIJKLMNOP"));
        assertFalse(AiModelDiscovery.isSafeModelId("abcdefgh.ijklmnop.qrstuvwx"));
    }
}
