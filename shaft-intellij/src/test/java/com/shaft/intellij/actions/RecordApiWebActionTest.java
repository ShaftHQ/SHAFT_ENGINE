package com.shaft.intellij.actions;

import com.google.gson.JsonObject;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers the default-mode (advancedUiEnabled=false) Assistant prompt {@code RecordApiWebAction}
 * builds instead of the old dead-end warning that discarded the typed URL (issue #3552), and the
 * {@code networkOptions} key-validation contract hardening (issue #3548 item 2).
 */
class RecordApiWebActionTest {
    @Test
    void recordApiPromptCarriesTheTypedUrl() {
        String prompt = RecordApiWebAction.recordApiPrompt("https://example.com/checkout");

        assertEquals("Record API traffic on https://example.com/checkout", prompt);
    }

    @Test
    void validNetworkCaptureOptionsKeysReportNoUnknowns() {
        JsonObject networkOptions = new JsonObject();
        networkOptions.addProperty("enabled", true);
        networkOptions.addProperty("excludeAssets", true);
        networkOptions.addProperty("excludePattern", "");
        networkOptions.addProperty("includePattern", "");
        networkOptions.addProperty("captureRequestBodies", true);
        networkOptions.addProperty("captureResponseBodies", true);

        assertEquals(List.of(), RecordApiWebAction.unknownNetworkCaptureOptionKeys(networkOptions));
    }

    @Test
    void mistypedNetworkCaptureOptionsKeyIsReportedAsUnknown() {
        JsonObject networkOptions = new JsonObject();
        networkOptions.addProperty("enabled", true);
        // Typo: the real field is captureResponseBodies -- this silently defaults response body
        // capture off instead of failing without item 2's validation.
        networkOptions.addProperty("captureResponsBodies", true);

        List<String> unknown = RecordApiWebAction.unknownNetworkCaptureOptionKeys(networkOptions);

        assertEquals(List.of("captureResponsBodies"), unknown);
    }

    @Test
    void networkCaptureOptionsFieldSetMatchesTheSixBoundFields() {
        assertTrue(RecordApiWebAction.NETWORK_CAPTURE_OPTIONS_FIELDS.containsAll(List.of(
                "enabled", "excludeAssets", "excludePattern", "includePattern",
                "captureResponseBodies", "captureRequestBodies")));
        assertEquals(6, RecordApiWebAction.NETWORK_CAPTURE_OPTIONS_FIELDS.size());
    }
}
