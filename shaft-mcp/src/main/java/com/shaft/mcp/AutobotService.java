package com.shaft.mcp;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
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
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
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
    private final Function<String, String> environmentReader;

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
        this(workspacePolicy, localAgentService, aiExecutor, System::getenv);
    }

    AutobotService(
            McpWorkspacePolicy workspacePolicy,
            LocalAgentService localAgentService,
            Function<AiRequest, AiResponse> aiExecutor,
            Function<String, String> environmentReader) {
        this.workspacePolicy = Objects.requireNonNull(workspacePolicy, "workspacePolicy");
        this.localAgentService = Objects.requireNonNull(localAgentService, "localAgentService");
        this.aiExecutor = Objects.requireNonNull(aiExecutor, "aiExecutor");
        this.environmentReader = Objects.requireNonNull(environmentReader, "environmentReader");
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
            AiRequest request = AiRequest.builder("autobot-provider-chat", codegenSchema())
                    .text(prompt == null ? "" : prompt)
                    .approvalPolicy(new ApprovalPolicy(false, true, EnumSet.of(EvidenceCategory.TEXT)))
                    // Reasoning models spend thinking tokens from the same output budget; 2k
                    // routinely truncated a full generated test class into malformed JSON
                    // (live Gemini evidence in ShaftHQ/SHAFT_ENGINE#3369).
                    .budget(new AiBudget(8_000, 8_000, BigDecimal.ZERO))
                    .timeout(Duration.ofSeconds(timeoutSeconds > 0 ? timeoutSeconds : DEFAULT_TIMEOUT_SECONDS))
                    .deterministicFallback(JSON.createObjectNode().put("answer", ""))
                    .build();
            AiResponse response = aiExecutor.apply(request);
            JsonNode payload = response.structuredPayload();
            List<AutobotCodeBlock> codeBlocks = parseCodeBlocks(payload.path("codeBlocks"));
            return new AutobotProviderChatResponse(
                    response.status().name(),
                    response.provider().isBlank() ? normalizedProvider : response.provider(),
                    response.model().isBlank() ? model : response.model(),
                    normalizedMode,
                    payload.path("answer").asText(""),
                    payload.path("summary").asText(""),
                    codeBlocks,
                    stringList(payload.path("citedGuideUrls")),
                    stringList(payload.path("locatorAssumptions")),
                    guardrailStatus(codeBlocks),
                    response.warnings(),
                    response.duration(),
                    response.fallbackReason());
        } finally {
            SHAFT.Properties.clearForCurrentThread();
        }
    }

    /**
     * Reports the configured SHAFT cloud provider readiness for the IntelliJ status view.
     *
     * <p>Never exposes the API key value; only whether the provider key is present in the environment.</p>
     *
     * @param provider provider identifier: openai, anthropic, gemini, or github
     * @param model configured model, or blank when none is set
     * @return read-only provider status
     */
    @Tool(name = "autobot_provider_status",
            description = "reports the configured SHAFT cloud provider, model, API-key presence (never the value),"
                    + " and structured-output support for the IntelliJ readiness view")
    public AutobotProviderStatus providerStatus(String provider, String model) {
        String normalizedProvider = normalizeProvider(provider);
        String environmentVariable = keyEnvironmentVariable(normalizedProvider);
        boolean keyPresent = !environmentVariable.isBlank()
                && !text(environmentReader.apply(environmentVariable)).isBlank();
        boolean structuredOutput = structuredOutputSupported(normalizedProvider);
        String effectiveModel = text(model);
        List<String> warnings = new ArrayList<>();
        if (environmentVariable.isBlank()) {
            warnings.add("Unknown provider '" + normalizedProvider
                    + "'; expected openai, anthropic, gemini, or github.");
        } else if (!keyPresent) {
            warnings.add("No API key detected in " + environmentVariable + "; store the " + normalizedProvider
                    + " key in SHAFT settings and enable passing provider keys to MCP.");
        }
        if (effectiveModel.isBlank()) {
            warnings.add("No model configured; set a " + normalizedProvider + " model in SHAFT settings.");
        }
        if (!structuredOutput && !environmentVariable.isBlank()) {
            warnings.add("Provider does not advertise JSON-schema structured output.");
        }
        return new AutobotProviderStatus(
                "1.0",
                normalizedProvider,
                effectiveModel,
                keyPresent,
                environmentVariable,
                structuredOutput,
                "ASK, PLAN",
                List.copyOf(warnings));
    }

    private static JsonNode codegenSchema() {
        var schema = JSON.createObjectNode().put("type", "object");
        var properties = JSON.createObjectNode();
        properties.set("answer", stringSchema("Concise natural-language answer or summary of the change."));
        properties.set("summary", stringSchema("One-line summary of what the generated SHAFT code does."));
        var codeBlock = JSON.createObjectNode().put("type", "object");
        var blockProperties = JSON.createObjectNode();
        blockProperties.set("language", stringSchema("Code language, usually java."));
        blockProperties.set("path", stringSchema("Repository-relative target file path, or empty."));
        blockProperties.set("insertionAnchor",
                stringSchema("Existing method or textual anchor to insert after, or empty."));
        blockProperties.set("code", stringSchema("SHAFT-syntax code without Markdown fences."));
        codeBlock.set("properties", blockProperties);
        codeBlock.putArray("required").add("code");
        var codeBlocks = JSON.createObjectNode().put("type", "array");
        codeBlocks.put("description", "Reviewed SHAFT-syntax code blocks ready for preview and apply.");
        codeBlocks.set("items", codeBlock);
        properties.set("codeBlocks", codeBlocks);
        properties.set("citedGuideUrls",
                stringArraySchema("Official SHAFT guide URLs that back the generated APIs."));
        properties.set("locatorAssumptions",
                stringArraySchema("Locator assumptions that were not verified in a live browser."));
        schema.set("properties", properties);
        schema.putArray("required").add("answer");
        return schema;
    }

    private static JsonNode stringSchema(String description) {
        return JSON.createObjectNode().put("type", "string").put("description", description);
    }

    private static JsonNode stringArraySchema(String description) {
        var array = JSON.createObjectNode().put("type", "array").put("description", description);
        array.set("items", JSON.createObjectNode().put("type", "string"));
        return array;
    }

    private static List<AutobotCodeBlock> parseCodeBlocks(JsonNode node) {
        if (node == null || !node.isArray()) {
            return List.of();
        }
        List<AutobotCodeBlock> blocks = new ArrayList<>();
        for (JsonNode block : node) {
            String code = block.path("code").asText("");
            if (code.isBlank()) {
                continue;
            }
            blocks.add(new AutobotCodeBlock(
                    block.path("language").asText("java"),
                    block.path("path").asText(""),
                    block.path("insertionAnchor").asText(""),
                    code));
        }
        return List.copyOf(blocks);
    }

    private static List<String> stringList(JsonNode node) {
        if (node == null || !node.isArray()) {
            return List.of();
        }
        List<String> values = new ArrayList<>();
        for (JsonNode item : node) {
            String value = item.asText("");
            if (!value.isBlank()) {
                values.add(value.trim());
            }
        }
        return List.copyOf(values);
    }

    private static String guardrailStatus(List<AutobotCodeBlock> blocks) {
        List<String> codes = blocks.stream()
                .map(AutobotCodeBlock::code)
                .filter(value -> !value.isBlank())
                .toList();
        if (codes.isEmpty()) {
            return "NOT_CHECKED";
        }
        McpCodeGuardrailResult result = new TestAutomationService().checkGeneratedCode("java", String.join("\n", codes));
        if (result.passed()) {
            return "PASSED";
        }
        long errors = result.violations().stream()
                .filter(violation -> "ERROR".equals(violation.severity()))
                .count();
        return "VIOLATIONS: " + errors + " error(s)";
    }

    private static String keyEnvironmentVariable(String provider) {
        return switch (provider) {
            case "openai" -> "OPENAI_API_KEY";
            case "anthropic" -> "ANTHROPIC_API_KEY";
            case "gemini" -> "GEMINI_API_KEY";
            case "github" -> "GITHUB_TOKEN";
            default -> "";
        };
    }

    private static boolean structuredOutputSupported(String provider) {
        return Set.of("openai", "anthropic", "gemini", "github").contains(provider);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
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
        return provider == null || provider.isBlank() ? "gemini" : provider.trim().toLowerCase(Locale.ROOT);
    }
}
