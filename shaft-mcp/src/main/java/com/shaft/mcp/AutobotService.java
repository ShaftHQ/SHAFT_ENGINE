package com.shaft.mcp;

import com.shaft.pilot.agent.LocalAgentClient;
import com.shaft.pilot.agent.LocalAgentMode;
import com.shaft.pilot.agent.LocalAgentRequest;
import com.shaft.pilot.agent.LocalAgentResponse;
import com.shaft.pilot.agent.LocalAgentService;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * MCP adapter for SHAFT Autobot local agent routing.
 */
@Service
public class AutobotService {
    private static final int DEFAULT_TIMEOUT_SECONDS = 300;

    private final McpWorkspacePolicy workspacePolicy;
    private final LocalAgentService localAgentService;

    /**
     * Creates the default Autobot MCP adapter.
     */
    public AutobotService() {
        this(McpWorkspacePolicy.current(), new LocalAgentService());
    }

    AutobotService(McpWorkspacePolicy workspacePolicy, LocalAgentService localAgentService) {
        this.workspacePolicy = Objects.requireNonNull(workspacePolicy, "workspacePolicy");
        this.localAgentService = Objects.requireNonNull(localAgentService, "localAgentService");
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
}
