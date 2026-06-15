package com.shaft.ai.provider;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
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
        ObjectNode text = generationConfig.putObject("responseFormat").putObject("text");
        text.put("mimeType", "application/json");
        text.set("schema", request.desiredResponseSchema());
        return root;
    }

    @Override
    protected Map<String, String> headers(ProviderConfiguration configuration) {
        return Map.of("x-goog-api-key", credential(configuration));
    }

    @Override
    protected JsonNode parseStructuredPayload(JsonNode response) throws JsonProcessingException {
        JsonNode text = response.path("candidates").path(0).path("content").path("parts").path(0).path("text");
        if (!text.isTextual()) {
            throw new JsonProcessingException("Missing candidate text") {
            };
        }
        return JSON.readTree(text.asText());
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
