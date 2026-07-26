package com.shaft.ai.provider;

import tools.jackson.core.JacksonException;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.pilot.ai.AiCapabilities;
import com.shaft.pilot.ai.AiImage;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.ProcessingLocation;
import com.shaft.pilot.config.PilotConfiguration;
import com.shaft.pilot.config.ProviderConfiguration;

import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Map;
import java.util.function.Function;

/**
 * Direct Google Gemini generateContent adapter.
 */
public final class GeminiProvider extends AbstractHttpAiProvider {
    /**
     * Creates the service-loadable provider.
     */
    public GeminiProvider() {
        super();
    }

    GeminiProvider(HttpClient client, Function<String, String> environment) {
        super(client, environment);
    }

    @Override
    public String id() {
        return "gemini";
    }

    @Override
    public AiCapabilities capabilities() {
        return new AiCapabilities(true, true, false, 0,
                processingLocation(ProcessingLocation.REMOTE));
    }

    @Override
    protected URI endpoint(ProviderConfiguration configuration) {
        String base = configuration.endpoint().toString().replaceAll("/+$", "");
        String model = URLEncoder.encode(configuration.model(), StandardCharsets.UTF_8);
        return URI.create(base + "/" + model + ":generateContent");
    }

    @Override
    protected ObjectNode buildPayload(AiRequest request, ProviderConfiguration configuration) {
        ObjectNode root = JSON.createObjectNode();
        ArrayNode parts = root.putArray("contents").addObject().putArray("parts");
        parts.addObject().put("text", prompt(request));
        for (AiImage image : request.images()) {
            ObjectNode inlineData = parts.addObject().putObject("inlineData");
            inlineData.put("mimeType", image.mediaType());
            inlineData.put("data", Base64.getEncoder().encodeToString(image.data()));
        }
        ObjectNode generationConfig = root.putObject("generationConfig");
        generationConfig.put("maxOutputTokens", outputTokenLimit(request, PilotConfiguration.current()));
        generationConfig.put("responseMimeType", "application/json");
        generationConfig.set("responseSchema", request.desiredResponseSchema());
        return root;
    }

    @Override
    protected Map<String, String> headers(ProviderConfiguration configuration) {
        return Map.of("x-goog-api-key", credential(configuration));
    }

    @Override
    protected JsonNode parseStructuredPayload(JsonNode response) throws JacksonException {
        // Thinking-enabled Gemini models (e.g. gemini-3.5-flash) can emit a leading
        // {"thought": true, ...} part ahead of the real answer part, or exhaust the output
        // budget on thinking before ever emitting an answer part. Reading parts[0]
        // unconditionally misreads the former as malformed JSON and gives no signal for the
        // latter (issue #4072). Skip thought parts and use the first part with real text;
        // when none qualifies, report the candidate's finishReason so truncation, a
        // thought-only response, and a genuinely malformed payload are distinguishable.
        //
        // A text part can also exist but be incomplete: the output-token budget runs out
        // mid-generation, so the part is valid up to the cut and then simply stops (issue
        // #4107, e.g. Jackson's "Unexpected end-of-input in property name"). That is a
        // truncation, not a malformed payload, and must not share its message: only report
        // truncation when the candidate's own finishReason says so (MAX_TOKENS), otherwise
        // let the genuine parse error propagate unchanged.
        JsonNode candidate = response.path("candidates").path(0);
        String finishReason = candidate.path("finishReason").asText("");
        for (JsonNode part : candidate.path("content").path("parts")) {
            if (part.path("thought").asBoolean(false)) {
                continue;
            }
            JsonNode text = part.path("text");
            if (text.isTextual() && !text.asText().isBlank()) {
                try {
                    return JSON.readTree(text.asText());
                } catch (JacksonException exception) {
                    if ("MAX_TOKENS".equals(finishReason)) {
                        throw new JacksonException("Gemini response was truncated at the output-token budget"
                                + " (finishReason=MAX_TOKENS) before the answer JSON completed") {
                        };
                    }
                    throw exception;
                }
            }
        }
        String detail = finishReason.isBlank() ? "" : " (finishReason=" + finishReason + ")";
        throw new JacksonException("Gemini response contained no usable answer text" + detail) {
        };
    }

    @Override
    protected AiUsage usage(JsonNode response) {
        JsonNode usage = response.path("usageMetadata");
        return new AiUsage(usage.path("promptTokenCount").asLong(),
                usage.path("candidatesTokenCount").asLong(), null);
    }

    @Override
    protected String responseModel(JsonNode response, ProviderConfiguration configuration) {
        return response.path("modelVersion").asText(configuration.model());
    }
}
