package com.shaft.ai.provider;

import tools.jackson.core.JacksonException;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
import org.junit.jupiter.api.Test;

import java.net.http.HttpClient;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
