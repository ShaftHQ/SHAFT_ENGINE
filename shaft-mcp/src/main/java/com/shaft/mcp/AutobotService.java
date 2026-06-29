package com.shaft.mcp;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.pilot.ai.AiBudget;
import com.shaft.pilot.ai.AiExecutionService;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceCategory;
import com.shaft.pilot.agent.LocalAgentClient;
import com.shaft.pilot.agent.LocalAgentMode;
import com.shaft.pilot.agent.LocalAgentRequest;
import com.shaft.pilot.agent.LocalAgentResponse;
import com.shaft.pilot.agent.LocalAgentService;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.nio.file.Path;
import java.time.Duration;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

/**
 * MCP adapter for SHAFT Autobot local agent routing.
 */
@Service
public class AutobotService {
    private static final int DEFAULT_TIMEOUT_SECONDS = 300;
    static final String CLOUD_AGENT_MODE_WARNING =
            "Cloud provider chat supports Ask and Plan only; use a local CLI runtime for Agent edits.";
    private static final ObjectMapper JSON = new ObjectMapper();

    private final McpWorkspacePolicy workspacePolicy;
    private final LocalAgentService localAgentService;
    private final Function<AiRequest, AiResponse> aiExecutor;

    /**
     * Creates the default Autobot MCP adapter.
     */
    public AutobotService() {
        this(McpWorkspacePolicy.current(), new LocalAgentService(), new AiExecutionService()::execute);
    }

    AutobotService(McpWorkspacePolicy workspacePolicy, LocalAgentService localAgentService) {
        this(workspacePolicy, localAgentService, new AiExecutionService()::execute);
    }

    AutobotService(
            McpWorkspacePolicy workspacePolicy,
            LocalAgentService localAgentService,
            Function<AiRequest, AiResponse> aiExecutor) {
        this.workspacePolicy = Objects.requireNonNull(workspacePolicy, "workspacePolicy");
        this.localAgentService = Objects.requireNonNull(localAgentService, "localAgentService");
        this.aiExecutor = Objects.requireNonNull(aiExecutor, "aiExecutor");
    }

    /**
     * Lists local agent CLI routes available to SHAFT Autobot.
     *
     * @return supported local clients
     */
    @Tool(name = "autobot_local_agent_clients",
            description = "lists local SHAFT Autobot CLI clients that do not require SHAFT cloud API keys")
    public List<AutobotLocalAgentClient> localAgentClients() {
        return java.util.Arrays.stream(LocalAgentClient.values())
                .map(client -> new AutobotLocalAgentClient(client.name(), client.displayName(),
                        client.executableName(), false))
                .toList();
    }

    /**
     * Runs an Ask, Plan, or explicitly approved Agent prompt through a local CLI agent.
     *
     * @param client local agent client: CODEX, CLAUDE_CODE, or COPILOT_CLI
     * @param mode Autobot mode: ASK, PLAN, or AGENT
     * @param prompt prompt written to the local agent
     * @param workingDirectory optional workspace-relative working directory
     * @param command optional custom command and arguments
     * @param environment optional process environment variables; never echoed in the response
     * @param timeoutSeconds process timeout in seconds; defaults to 300 when non-positive
     * @param allowSourceMutation explicit approval required for AGENT mode
     * @return safe local agent response
     */
    @Tool(name = "autobot_local_agent_run",
            description = "routes Ask, Plan, or approved Agent requests to Codex, Claude Code, or Copilot CLI")
    public LocalAgentResponse runLocalAgent(
            String client,
            String mode,
            String prompt,
            String workingDirectory,
            List<String> command,
            Map<String, String> environment,
            int timeoutSeconds,
            boolean allowSourceMutation) {
        Path directory = workingDirectory == null || workingDirectory.isBlank()
                ? workspacePolicy.root()
                : workspacePolicy.existing(workingDirectory, "Autobot working directory");
        LocalAgentRequest request = LocalAgentRequest.builder(LocalAgentClient.from(client), LocalAgentMode.from(mode),
                        prompt)
                .workingDirectory(directory)
                .command(command)
                .environment(environment)
                .timeout(Duration.ofSeconds(timeoutSeconds > 0 ? timeoutSeconds : DEFAULT_TIMEOUT_SECONDS))
                .allowSourceMutation(allowSourceMutation)
                .build();
        return localAgentService.execute(request);
    }

    /**
     * Runs an Ask or Plan prompt through the configured SHAFT cloud provider.
     *
     * @param provider provider identifier: openai, anthropic, gemini, or github
     * @param model model identifier
     * @param mode assistant mode: ASK or PLAN
     * @param prompt prompt sent to the provider
     * @param workingDirectory optional workspace-relative working directory
     * @param timeoutSeconds provider timeout in seconds
     * @param allowSourceMutation ignored for cloud providers; source mutation is never allowed
     * @return safe provider response
     */
    @Tool(name = "autobot_provider_chat",
            description = "routes Ask or Plan requests to configured SHAFT cloud providers without source mutation")
    public AutobotProviderChatResponse runProviderChat(
            String provider,
            String model,
            String mode,
            String prompt,
            String workingDirectory,
            int timeoutSeconds,
            boolean allowSourceMutation) {
        String normalizedMode = normalizeMode(mode);
        if ("AGENT".equals(normalizedMode) || allowSourceMutation) {
            return new AutobotProviderChatResponse("REJECTED", normalizeProvider(provider), model, normalizedMode,
                    "", List.of(CLOUD_AGENT_MODE_WARNING), Duration.ZERO, "");
        }
        if (workingDirectory != null && !workingDirectory.isBlank()) {
            workspacePolicy.existing(workingDirectory, "Autobot working directory");
        }
        String normalizedProvider = normalizeProvider(provider);
        try {
            configureProvider(normalizedProvider, model);
            AiRequest request = AiRequest.builder("autobot-provider-chat", answerSchema())
                    .text(prompt == null ? "" : prompt)
                    .approvalPolicy(new ApprovalPolicy(false, true, EnumSet.of(EvidenceCategory.TEXT)))
                    .budget(new AiBudget(8_000, 1_000, BigDecimal.ZERO))
                    .timeout(Duration.ofSeconds(timeoutSeconds > 0 ? timeoutSeconds : DEFAULT_TIMEOUT_SECONDS))
                    .deterministicFallback(JSON.createObjectNode().put("answer", ""))
                    .build();
            AiResponse response = aiExecutor.apply(request);
            return new AutobotProviderChatResponse(
                    response.status().name(),
                    response.provider().isBlank() ? normalizedProvider : response.provider(),
                    response.model().isBlank() ? model : response.model(),
                    normalizedMode,
                    response.structuredPayload().path("answer").asText(""),
                    response.warnings(),
                    response.duration(),
                    response.fallbackReason());
        } finally {
            SHAFT.Properties.clearForCurrentThread();
        }
    }

    private static JsonNode answerSchema() {
        var schema = JSON.createObjectNode().put("type", "object");
        schema.set("properties", JSON.createObjectNode()
                .set("answer", JSON.createObjectNode().put("type", "string")));
        schema.putArray("required").add("answer");
        return schema;
    }

    private static void configureProvider(String provider, String model) {
        var properties = SHAFT.Properties.pilot.set()
                .enabled(true)
                .provider(provider)
                .localConsent(false)
                .remoteConsent(true)
                .allowedEvidenceCategories("TEXT");
        if (model == null || model.isBlank()) {
            return;
        }
        switch (provider) {
            case "openai" -> properties.openAiModel(model);
            case "anthropic" -> properties.anthropicModel(model);
            case "gemini" -> properties.geminiModel(model);
            case "github" -> properties.githubModel(model);
            default -> {
            }
        }
    }

    private static String normalizeMode(String mode) {
        String normalized = mode == null || mode.isBlank() ? "ASK" : mode.trim();
        return normalized.toUpperCase(Locale.ROOT).replace('-', '_').replace(' ', '_');
    }

    private static String normalizeProvider(String provider) {
        return provider == null || provider.isBlank() ? "openai" : provider.trim().toLowerCase(Locale.ROOT);
    }
}
