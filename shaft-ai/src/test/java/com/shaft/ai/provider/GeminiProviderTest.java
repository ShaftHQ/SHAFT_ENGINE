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
