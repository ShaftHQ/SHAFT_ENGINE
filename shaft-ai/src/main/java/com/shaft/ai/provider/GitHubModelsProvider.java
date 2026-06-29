package com.shaft.ai.provider;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.pilot.ai.AiCapabilities;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.ProcessingLocation;
import com.shaft.pilot.config.PilotConfiguration;
import com.shaft.pilot.config.ProviderConfiguration;

import java.net.http.HttpClient;
import java.util.Map;
import java.util.function.Function;

/**
 * Direct GitHub Models chat-completions adapter.
 */
public final class GitHubModelsProvider extends AbstractHttpAiProvider {
    /**
     * Creates the service-loadable provider.
     */
    public GitHubModelsProvider() {
        super();
    }

    GitHubModelsProvider(HttpClient client, Function<String, String> environment) {
        super(client, environment);
    }

    @Override
    public String id() {
        return "github";
    }

    @Override
    public AiCapabilities capabilities() {
        return new AiCapabilities(true, false, false, 0,
                processingLocation(ProcessingLocation.REMOTE));
    }

    @Override
    protected ObjectNode buildPayload(AiRequest request, ProviderConfiguration configuration) {
        ObjectNode root = JSON.createObjectNode();
        root.put("model", configuration.model());
        root.put("max_tokens", outputTokenLimit(request, PilotConfiguration.current()));
        root.putObject("response_format").put("type", "json_object");
        ObjectNode message = root.putArray("messages").addObject();
        message.put("role", "user");
        message.put("content", prompt(request) + "\n\nReturn only JSON matching this schema:\n"
                + request.desiredResponseSchema());
        return root;
    }

    @Override
    protected Map<String, String> headers(ProviderConfiguration configuration) {
        return Map.of(
                "Authorization", "Bearer " + credential(configuration),
                "X-GitHub-Api-Version", "2022-11-28");
    }

    @Override
    protected JsonNode parseStructuredPayload(JsonNode response) throws JsonProcessingException {
        JsonNode content = response.path("choices").path(0).path("message").path("content");
        if (!content.isTextual()) {
            throw new JsonProcessingException("Missing message content") {
            };
        }
        return JSON.readTree(content.asText());
    }

    @Override
    protected AiUsage usage(JsonNode response) {
        JsonNode usage = response.path("usage");
        return new AiUsage(usage.path("prompt_tokens").asLong(), usage.path("completion_tokens").asLong(), null);
    }
}
