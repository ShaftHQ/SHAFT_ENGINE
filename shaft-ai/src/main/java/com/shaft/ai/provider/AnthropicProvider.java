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
 * Direct Anthropic Messages API adapter.
 */
public final class AnthropicProvider extends AbstractHttpAiProvider {
    /**
     * Creates the service-loadable provider.
     */
    public AnthropicProvider() {
        super();
    }

    AnthropicProvider(HttpClient client, Function<String, String> environment) {
        super(client, environment);
    }

    @Override
    public String id() {
        return "anthropic";
    }

    @Override
    public AiCapabilities capabilities() {
        return new AiCapabilities(true, true, true, 0, ProcessingLocation.REMOTE);
    }

    @Override
    protected ObjectNode buildPayload(AiRequest request, ProviderConfiguration configuration) {
        ObjectNode root = JSON.createObjectNode();
        root.put("model", configuration.model());
        root.put("max_tokens", outputTokenLimit(request, PilotConfiguration.current()));
        ArrayNode messages = root.putArray("messages");
        ObjectNode message = messages.addObject();
        message.put("role", "user");
        ArrayNode content = message.putArray("content");
        content.addObject().put("type", "text").put("text", prompt(request));
        for (AiImage image : request.images()) {
            ObjectNode source = content.addObject().put("type", "image").putObject("source");
            source.put("type", "base64");
            source.put("media_type", image.mediaType());
            source.put("data", Base64.getEncoder().encodeToString(image.data()));
        }
        ObjectNode format = root.putObject("output_config").putObject("format");
        format.put("type", "json_schema");
        format.set("schema", request.desiredResponseSchema());
        return root;
    }

    @Override
    protected Map<String, String> headers(ProviderConfiguration configuration) {
        return Map.of(
                "x-api-key", credential(configuration),
                "anthropic-version", configuration.option("api-version"));
    }

    @Override
    protected JsonNode parseStructuredPayload(JsonNode response) throws JsonProcessingException {
        for (JsonNode content : response.path("content")) {
            if ("text".equals(content.path("type").asText())) {
                return JSON.readTree(content.path("text").asText());
            }
        }
        throw new JsonProcessingException("Missing content text") {
        };
    }

    @Override
    protected AiUsage usage(JsonNode response) {
        JsonNode usage = response.path("usage");
        return new AiUsage(usage.path("input_tokens").asLong(), usage.path("output_tokens").asLong(), null);
    }
}
