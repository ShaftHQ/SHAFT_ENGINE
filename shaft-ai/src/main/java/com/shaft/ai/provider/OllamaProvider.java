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

import java.net.http.HttpClient;
import java.util.Base64;
import java.util.Map;
import java.util.function.Function;

/**
 * Direct local Ollama chat adapter.
 */
public final class OllamaProvider extends AbstractHttpAiProvider {
    /**
     * Creates the service-loadable provider.
     */
    public OllamaProvider() {
        super();
    }

    OllamaProvider(HttpClient client, Function<String, String> environment) {
        super(client, environment);
    }

    @Override
    public String id() {
        return "ollama";
    }

    @Override
    public AiCapabilities capabilities() {
        return new AiCapabilities(true, true, false, 0,
                processingLocation(ProcessingLocation.LOCAL));
    }

    @Override
    protected boolean requiresCredential(ProviderConfiguration configuration) {
        return !configuration.apiKeyEnvironmentVariable().isBlank();
    }

    @Override
    protected Map<String, String> headers(ProviderConfiguration configuration) {
        if (!requiresCredential(configuration)) {
            return Map.of();
        }
        String header = configuration.option("api-key-header");
        String prefix = configuration.option("api-key-prefix");
        return Map.of(
                header.isBlank() ? "Authorization" : header,
                prefix + credential(configuration));
    }

    @Override
    protected ObjectNode buildPayload(AiRequest request, ProviderConfiguration configuration) {
        ObjectNode root = JSON.createObjectNode();
        root.put("model", configuration.model());
        root.put("stream", false);
        root.set("format", request.desiredResponseSchema());
        ObjectNode message = root.putArray("messages").addObject();
        message.put("role", "user");
        message.put("content", prompt(request));
        if (!request.images().isEmpty()) {
            ArrayNode images = message.putArray("images");
            for (AiImage image : request.images()) {
                images.add(Base64.getEncoder().encodeToString(image.data()));
            }
        }
        root.putObject("options").put("num_predict",
                outputTokenLimit(request, PilotConfiguration.current()));
        return root;
    }

    @Override
    protected JsonNode parseStructuredPayload(JsonNode response) throws JsonProcessingException {
        JsonNode text = response.path("message").path("content");
        if (!text.isTextual()) {
            throw new JsonProcessingException("Missing message content") {
            };
        }
        return JSON.readTree(text.asText());
    }

    @Override
    protected AiUsage usage(JsonNode response) {
        return new AiUsage(response.path("prompt_eval_count").asLong(),
                response.path("eval_count").asLong(), null);
    }
}
