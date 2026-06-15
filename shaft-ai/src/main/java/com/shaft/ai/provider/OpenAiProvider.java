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
 * Direct OpenAI Responses API adapter.
 */
public final class OpenAiProvider extends AbstractHttpAiProvider {
    /**
     * Creates the service-loadable provider.
     */
    public OpenAiProvider() {
        super();
    }

    OpenAiProvider(HttpClient client, Function<String, String> environment) {
        super(client, environment);
    }

    @Override
    public String id() {
        return "openai";
    }

    @Override
    public AiCapabilities capabilities() {
        return new AiCapabilities(true, true, true, 0,
                processingLocation(ProcessingLocation.REMOTE));
    }

    @Override
    protected ObjectNode buildPayload(AiRequest request, ProviderConfiguration configuration) {
        ObjectNode root = JSON.createObjectNode();
        root.put("model", configuration.model());
        root.put("store", false);
        root.put("max_output_tokens", outputTokenLimit(request, PilotConfiguration.current()));
        ArrayNode input = root.putArray("input");
        ObjectNode message = input.addObject();
        message.put("role", "user");
        ArrayNode content = message.putArray("content");
        content.addObject().put("type", "input_text").put("text", prompt(request));
        for (AiImage image : request.images()) {
            content.addObject()
                    .put("type", "input_image")
                    .put("image_url", "data:" + image.mediaType() + ";base64,"
                            + Base64.getEncoder().encodeToString(image.data()));
        }
        ObjectNode format = root.putObject("text").putObject("format");
        format.put("type", "json_schema");
        format.put("name", "shaft_pilot_response");
        format.put("strict", true);
        format.set("schema", request.desiredResponseSchema());
        return root;
    }

    @Override
    protected Map<String, String> headers(ProviderConfiguration configuration) {
        return Map.of("Authorization", "Bearer " + credential(configuration));
    }

    @Override
    protected JsonNode parseStructuredPayload(JsonNode response) throws JsonProcessingException {
        for (JsonNode output : response.path("output")) {
            for (JsonNode content : output.path("content")) {
                if ("output_text".equals(content.path("type").asText())) {
                    return JSON.readTree(content.path("text").asText());
                }
            }
        }
        throw new JsonProcessingException("Missing output text") {
        };
    }

    @Override
    protected AiUsage usage(JsonNode response) {
        JsonNode usage = response.path("usage");
        return new AiUsage(usage.path("input_tokens").asLong(), usage.path("output_tokens").asLong(), null);
    }
}
