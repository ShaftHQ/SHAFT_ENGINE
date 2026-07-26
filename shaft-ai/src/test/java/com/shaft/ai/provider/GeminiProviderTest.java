package com.shaft.ai.provider;

import tools.jackson.core.JacksonException;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.pilot.ai.AiBudget;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.config.ProviderConfiguration;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.net.URI;
import java.net.http.HttpClient;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pure payload-parsing coverage for {@link GeminiProvider}, no live credentials required.
 * <p>
 * Reproduces the nightly {@code IntelliJ live Gemini E2E} failure (issue #4072): Gemini's
 * thinking-enabled models can emit a leading {@code "thought": true} part before the real
 * answer part. {@code parseStructuredPayload} used to read {@code parts[0]} unconditionally,
 * so a thought-first response was misread as malformed JSON even though a valid answer part
 * followed it.
 */
class GeminiProviderTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    private final GeminiProvider provider = new GeminiProvider(HttpClient.newHttpClient(), ignored -> "test-credential");

    @Test
    void parseStructuredPayloadSkipsLeadingThoughtPartAndReturnsAnswerPart() {
        JsonNode response = geminiResponse(
                thoughtPart("Let me plan the DuckDuckGo test case before writing it out."),
                textPart("{\"answer\":\"ok\"}"));

        JsonNode payload = provider.parseStructuredPayload(response);

        assertEquals("ok", payload.path("answer").asText());
    }

    @Test
    void parseStructuredPayloadReportsFinishReasonWhenNoUsableTextPartRemains() {
        JsonNode response = geminiResponse("MAX_TOKENS",
                thoughtPart("Still reasoning about the test case when the token budget ran out."));

        JacksonException exception = assertThrows(JacksonException.class, () -> provider.parseStructuredPayload(response));

        assertTrue(exception.getMessage().contains("MAX_TOKENS"), exception.getMessage());
    }

    @Test
    void parseStructuredPayloadReportsTruncationWhenTextPartIsCutOffMidJsonAndFinishReasonIsMaxTokens() {
        // Reproduces issue #4107: the model emits a real (non-thought) answer part, but the
        // output-token budget runs out mid-generation, so the text is valid up to the cut and then
        // stops -- e.g. mid property name, matching the live nightly's Jackson message "Unexpected
        // end-of-input in property name". Unlike the no-usable-text-part case above, a text part
        // *does* exist here, so the old code took the JSON.readTree(...) path and let Jackson's raw
        // parse-error message escape instead of recognizing the truncation.
        JsonNode response = geminiResponse("MAX_TOKENS", textPart("{\"answer\":\"partial value cut off mid-prop"));

        JacksonException exception = assertThrows(JacksonException.class, () -> provider.parseStructuredPayload(response));

        assertTrue(exception.getMessage().contains("MAX_TOKENS"), exception.getMessage());
        assertTrue(exception.getMessage().toLowerCase(java.util.Locale.ROOT).contains("truncat"), exception.getMessage());
    }

    @Test
    void parseStructuredPayloadReportsBothParseErrorAndObservedFinishReasonWhenNotMaxTokens() {
        // A genuinely malformed payload (not a truncation) must not be relabeled as a truncation
        // just because it fails to parse -- the two are different defects (issue #4107). But the
        // finishReason Gemini actually reported must still surface: the parse-error message alone
        // cannot tell a future reader whether this was truncation under a different finishReason
        // value or a real malformed payload, so the observed finishReason (here STOP) must appear
        // in the message even though it is not MAX_TOKENS.
        JsonNode response = geminiResponse("STOP", textPart("{not valid json at all"));

        JacksonException exception = assertThrows(JacksonException.class, () -> provider.parseStructuredPayload(response));

        String message = exception.getMessage().toLowerCase(java.util.Locale.ROOT);
        assertTrue(message.contains("not valid json") || message.contains("unexpected"), exception.getMessage());
        assertTrue(exception.getMessage().contains("STOP"), exception.getMessage());
    }

    @Test
    void parseStructuredPayloadReportsParseErrorAndNoneWhenFinishReasonIsAbsent() {
        // When Gemini's candidate omits finishReason entirely, the diagnostic must say so
        // explicitly rather than silently omitting it -- an absent value is itself a fact worth
        // surfacing, not something to swallow (issue #4107).
        JsonNode response = geminiResponse("", textPart("{not valid json at all"));

        JacksonException exception = assertThrows(JacksonException.class, () -> provider.parseStructuredPayload(response));

        assertTrue(exception.getMessage().contains("<none>"), exception.getMessage());
    }

    @Test
    void buildPayloadHonorsTheRequestedOutputBudgetInsteadOfTheGlobalDefaultCeiling() {
        // AutobotService.java's autobot_provider_chat already asks for an 8,000-token budget
        // (raised from 2k by #3371 specifically to stop reasoning-model truncation), but
        // outputTokenLimit() clamps every request to min(requested, PilotConfiguration
        // .current().maxOutputTokens()) -- and nothing overrides that global ceiling away from
        // its Pilot.java default. With no override in effect here (the live nightly E2E sets
        // none either), the 8,000-token request must still reach the wire; a lower value proves
        // the global default is silently eating the request the way it has since #3371 shipped.
        ObjectNode payload = provider.buildPayload(request(8_000), configuration("gemini-3.5-flash"));

        assertEquals(8_000, payload.path("generationConfig").path("maxOutputTokens").asLong());
    }

    @Test
    void buildPayloadCapsThinkingLevelForGemini3ModelsToPreserveAnswerTokenBudget() {
        // Gemini 3.x models spend thinking tokens from the same output budget as the answer
        // (AutobotService.java:164-166); thinkingConfig.thinkingLevel bounds that spend so a
        // fixed output budget leaves real headroom for the answer JSON (issue #4113's targeted
        // "partition" fix).
        ObjectNode payload = provider.buildPayload(request(8_000), configuration("gemini-3.5-flash"));

        assertEquals("low", payload.path("generationConfig").path("thinkingConfig").path("thinkingLevel").asText());
    }

    @Test
    void buildPayloadLeavesThinkingConfigUntouchedForNonGemini3Models() {
        // thinkingLevel is a Gemini 3.x-only field (2.5-series models use thinkingBudget
        // instead); sending it to a 2.5-series model risks an unsupported-parameter rejection,
        // so models outside the 3.x family must get no thinkingConfig at all.
        ObjectNode payload = provider.buildPayload(request(8_000), configuration("gemini-2.5-flash"));

        assertFalse(payload.path("generationConfig").has("thinkingConfig"),
                payload.path("generationConfig").toString());
    }

    private static AiRequest request(long maxOutputTokens) {
        JsonNode schema = JSON.createObjectNode().put("type", "object");
        return AiRequest.builder("gemini-provider-test", schema)
                .text("prompt")
                .budget(new AiBudget(0, maxOutputTokens, BigDecimal.ZERO))
                .build();
    }

    private static ProviderConfiguration configuration(String model) {
        return new ProviderConfiguration("gemini", URI.create("https://example.test/v1beta/models"),
                model, "GEMINI_API_KEY", Map.of());
    }

    private static ObjectNode thoughtPart(String text) {
        ObjectNode part = JSON.createObjectNode();
        part.put("thought", true);
        part.put("text", text);
        return part;
    }

    private static ObjectNode textPart(String text) {
        return JSON.createObjectNode().put("text", text);
    }

    private static JsonNode geminiResponse(ObjectNode... parts) {
        return geminiResponse("STOP", parts);
    }

    private static JsonNode geminiResponse(String finishReason, ObjectNode... parts) {
        ObjectNode root = JSON.createObjectNode();
        ObjectNode candidate = root.putArray("candidates").addObject();
        candidate.put("finishReason", finishReason);
        ArrayNode partsArray = candidate.putObject("content").putArray("parts");
        for (ObjectNode part : parts) {
            partsArray.add(part);
        }
        return root;
    }
}
