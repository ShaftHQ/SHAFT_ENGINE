package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.shaft.intellij.mcp.ShaftCommandLine;

/**
 * Builds SHAFT Assistant MCP invocations from prompt text.
 */
final class AssistantCommand {
    private static final int DEFAULT_TIMEOUT_SECONDS = 300;
    private static final String HELP = """
            Slash commands:
            /guide <query> - Search the SHAFT guide.
            /scenarios <intent> - Find automation scenarios.
            /guardrails <code> - Check generated Java code.
            /clients - List local assistant clients.
            /help - Show this help.
            """.stripIndent().trim();

    private AssistantCommand() {
        throw new IllegalStateException("Utility class");
    }

    static Invocation fromPrompt(
            String prompt,
            String client,
            String mode,
            String workingDirectory,
            String customCommand,
            boolean allowSourceMutation) {
        String text = prompt == null ? "" : prompt.trim();
        if (text.isEmpty()) {
            return Invocation.local("Enter a prompt or slash command.");
        }
        if (text.startsWith("/")) {
            return slash(text);
        }
        JsonObject arguments = new JsonObject();
        arguments.addProperty("client", client);
        arguments.addProperty("mode", mode);
        arguments.addProperty("prompt", text);
        arguments.addProperty("workingDirectory", workingDirectory == null ? "" : workingDirectory);
        arguments.add("command", commandArray(customCommand));
        arguments.add("environment", new JsonObject());
        arguments.addProperty("timeoutSeconds", DEFAULT_TIMEOUT_SECONDS);
        arguments.addProperty("allowSourceMutation", "AGENT".equals(mode) && allowSourceMutation);
        return Invocation.tool("autobot_local_agent_run", arguments);
    }

    private static Invocation slash(String text) {
        String[] parts = text.split("\\s+", 2);
        String command = parts[0].toLowerCase();
        String rest = parts.length == 2 ? parts[1].trim() : "";
        return switch (command) {
            case "/guide" -> Invocation.tool("shaft_guide_search", guide(rest));
            case "/scenarios" -> Invocation.tool("test_automation_scenarios", scenarios(rest));
            case "/guardrails" -> Invocation.tool("test_code_guardrails_check", guardrails(rest));
            case "/clients" -> Invocation.tool("autobot_local_agent_clients", new JsonObject());
            case "/help" -> Invocation.local(HELP);
            default -> Invocation.local("Unknown command. Use /help for available SHAFT Assistant commands.");
        };
    }

    private static JsonObject guide(String query) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("query", query.isBlank() ? "page objects locators" : query);
        arguments.addProperty("maxResults", 5);
        return arguments;
    }

    private static JsonObject scenarios(String intent) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("area", "all");
        arguments.addProperty("intent", intent);
        arguments.addProperty("maxResults", 20);
        return arguments;
    }

    private static JsonObject guardrails(String code) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("language", "java");
        arguments.addProperty("code", code);
        return arguments;
    }

    private static JsonArray commandArray(String value) {
        JsonArray array = new JsonArray();
        if (value == null || value.isBlank()) {
            return array;
        }
        for (String token : ShaftCommandLine.parse(value)) {
            array.add(token);
        }
        return array;
    }

    record Invocation(String toolName, JsonObject arguments, String localResponse) {
        static Invocation tool(String toolName, JsonObject arguments) {
            return new Invocation(toolName, arguments, null);
        }

        static Invocation local(String response) {
            return new Invocation("", new JsonObject(), response);
        }

        boolean isLocal() {
            return localResponse != null;
        }
    }
}
