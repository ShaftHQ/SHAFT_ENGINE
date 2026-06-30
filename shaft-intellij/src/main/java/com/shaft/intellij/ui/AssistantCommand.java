package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.shaft.intellij.mcp.ShaftCommandLine;

import java.util.Locale;

/**
 * Builds SHAFT Assistant MCP invocations from prompt text.
 */
final class AssistantCommand {
    private static final int DEFAULT_TIMEOUT_SECONDS = 300;
    private static final int DEFAULT_BROWSER_MAX_CHARACTERS = 12_000;
    private static final int DEFAULT_BROWSER_MAX_ELEMENTS = 10;
    private static final String SHAFT_MCP_USAGE_HINT =
            "If this request requires interacting with a browser, page element, or mobile app, use shaft-mcp.";
    private static final String HELP = """
            Slash commands:
            /guide <query> - Search the SHAFT guide.
            /scenarios <intent> - Find automation scenarios.
            /record [playwright] - Start a WebDriver or Playwright recording session.
            /inspect [url] [intent] - Open intent-driven inspection context.
            /locator [url] [intent] - Alias for /inspect.
            /guardrails <code> - Check generated Java code.
            /triage - Analyze failed Allure evidence.
            /fixTestFailure - Analyze failed Allure evidence and propose repair options.
            /clients - List local assistant clients.
            /generateTest [intent|recordingPath] - Suggest scenarios, or generate code from recording.
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
        return fromPrompt(prompt, Selection.fromClient(client), mode, workingDirectory, customCommand, allowSourceMutation);
    }

    static Invocation fromPrompt(
            String prompt,
            Selection selection,
            String mode,
            String workingDirectory,
            String customCommand,
            boolean allowSourceMutation) {
        String text = prompt == null ? "" : prompt.trim();
        if (text.isEmpty()) {
            return Invocation.local("Enter a prompt or slash command.");
        }
        if (text.startsWith("/")) {
            return slash(text, workingDirectory);
        }
        if (selection.cloud()) {
            return cloud(text, selection, mode, workingDirectory);
        }
        if (!"CLI".equals(selection.runtime())) {
            return Invocation.local("SHAFT is configured for " + selection.displayName()
                    + ". Configure SHAFT MCP for that runtime, then send this prompt from its native chat panel.");
        }
        JsonObject arguments = new JsonObject();
        arguments.addProperty("client", selection.client());
        arguments.addProperty("mode", mode);
        arguments.addProperty("prompt", localAgentPrompt(text));
        arguments.addProperty("workingDirectory", workingDirectory == null ? "" : workingDirectory);
        arguments.add("command", commandArray(customCommand));
        arguments.add("environment", new JsonObject());
        arguments.addProperty("timeoutSeconds", DEFAULT_TIMEOUT_SECONDS);
        arguments.addProperty("allowSourceMutation", "AGENT".equals(mode) && allowSourceMutation);
        return Invocation.tool("autobot_local_agent_run", arguments);
    }

    private static Invocation cloud(String text, Selection selection, String mode, String workingDirectory) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("provider", selection.cloudProvider());
        arguments.addProperty("model", selection.cloudModel());
        arguments.addProperty("mode", mode);
        arguments.addProperty("prompt", text);
        arguments.addProperty("workingDirectory", workingDirectory == null ? "" : workingDirectory);
        arguments.addProperty("timeoutSeconds", DEFAULT_TIMEOUT_SECONDS);
        arguments.addProperty("allowSourceMutation", false);
        return Invocation.tool("autobot_provider_chat", arguments);
    }

    private static Invocation slash(String text, String workingDirectory) {
        String[] parts = text.split("\\s+", 2);
        String command = parts[0].toLowerCase(Locale.ROOT);
        String rest = parts.length == 2 ? parts[1].trim() : "";
        return switch (command) {
            case "/guide" -> Invocation.tool("shaft_guide_search", guide(rest));
            case "/scenarios" -> Invocation.tool("test_automation_scenarios", scenarios(rest));
            case "/record" -> record(rest);
            case "/inspect", "/locator" -> Invocation.tool("browser_open_intent", inspect(rest));
            case "/guardrails" -> Invocation.tool("test_code_guardrails_check", guardrails(rest));
            case "/triage", "/fixtestfailure" -> Invocation.tool(
                    "doctor_analyze_failed_allure", triage(workingDirectory));
            case "/clients" -> Invocation.tool("autobot_local_agent_clients", new JsonObject());
            case "/generatetest" -> generateTest(rest);
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

    private static JsonObject inspect(String rest) {
        String[] targetAndIntent = splitTargetUrlAndIntent(rest);
        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", targetAndIntent[0]);
        arguments.addProperty("userIntent", targetAndIntent[1]);
        arguments.addProperty("maxCharacters", DEFAULT_BROWSER_MAX_CHARACTERS);
        arguments.addProperty("maxElements", DEFAULT_BROWSER_MAX_ELEMENTS);
        return arguments;
    }

    private static JsonObject guardrails(String code) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("language", "java");
        arguments.addProperty("code", code);
        return arguments;
    }

    private static Invocation record(String rest) {
        boolean playwright = rest.toLowerCase(Locale.ROOT).contains("playwright");
        return Invocation.tool(playwright ? "playwright_record_start" : "capture_start",
                playwright ? playwrightRecordStart() : captureStart(rest));
    }

    private static JsonObject triage(String workingDirectory) {
        JsonObject arguments = new JsonObject();
        JsonArray allureResultPaths = new JsonArray();
        allureResultPaths.add("allure-results");
        arguments.add("allureResultPaths", allureResultPaths);
        arguments.add("historicalBundlePaths", new JsonArray());
        arguments.addProperty("outputDirectory", "target/shaft-doctor");
        arguments.addProperty("includeScreenshots", true);
        arguments.addProperty("includePageSnapshots", true);
        arguments.addProperty("minimumAllureResults", 1);
        arguments.addProperty("repositoryRoot", workingDirectory == null ? "" : workingDirectory);
        arguments.add("allowedSourcePaths", new JsonArray());
        arguments.addProperty("useAi", false);
        arguments.addProperty("allowLocalAi", false);
        arguments.addProperty("allowRemoteAi", false);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    private static Invocation generateTest(String rest) {
        String recordingPath = firstJsonLikePath(rest);
        if (recordingPath.isBlank()) {
            return Invocation.tool("test_automation_scenarios", scenarios(rest));
        }
        if (recordingPath.toLowerCase(Locale.ROOT).contains("playwright")) {
            return Invocation.tool("playwright_recording_code_blocks", generatePlaywrightCodeFromRecording(recordingPath));
        }
        return Invocation.tool("capture_code_blocks", generateCaptureCodeFromRecording(recordingPath));
    }

    private static JsonObject captureStart(String rest) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", parseLeadingUrl(rest));
        arguments.addProperty("browser", "Chrome");
        arguments.addProperty("outputPath", "recordings/intellij-capture.json");
        arguments.addProperty("headless", false);
        return arguments;
    }

    private static JsonObject playwrightRecordStart() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputPath", "recordings/playwright-recording.json");
        arguments.addProperty("mode", "default");
        arguments.addProperty("includeSensitiveValues", false);
        return arguments;
    }

    private static JsonObject generateCaptureCodeFromRecording(String recordingPath) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("sessionPath", recordingPath);
        arguments.addProperty("outputDirectory", ".");
        arguments.addProperty("packageName", "tests.generated");
        arguments.addProperty("className", "RecordedFlowTest");
        arguments.addProperty("overwrite", false);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    private static JsonObject generatePlaywrightCodeFromRecording(String recordingPath) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("recordingPath", recordingPath);
        arguments.addProperty("driverVariableName", "driver");
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

    private static String localAgentPrompt(String text) {
        String lower = text.toLowerCase(Locale.ROOT);
        if (lower.contains("shaft-mcp")) {
            return text;
        }
        return SHAFT_MCP_USAGE_HINT + "\n\n" + text;
    }

    private static String[] splitTargetUrlAndIntent(String rest) {
        String trimmed = rest == null ? "" : rest.trim();
        if (trimmed.isBlank()) {
            return new String[]{"", ""};
        }
        int firstSpace = trimmed.indexOf(' ');
        if (firstSpace < 0) {
            if (isUrl(trimmed)) {
                return new String[]{trimmed, ""};
            }
            return new String[]{"", trimmed};
        }
        String firstToken = trimmed.substring(0, firstSpace);
        String restIntent = trimmed.substring(firstSpace + 1).trim();
        if (isUrl(firstToken)) {
            return new String[]{firstToken, restIntent};
        }
        return new String[]{"", trimmed};
    }

    private static String firstJsonLikePath(String rest) {
        if (rest == null || rest.isBlank()) {
            return "";
        }
        for (String token : rest.split("\\s+")) {
            if (token.toLowerCase(Locale.ROOT).endsWith(".json")) {
                return token;
            }
        }
        return "";
    }

    private static String parseLeadingUrl(String rest) {
        String[] parts = (rest == null ? "" : rest).trim().split("\\s+", 2);
        if (parts.length > 0 && isUrl(parts[0])) {
            return parts[0];
        }
        return "";
    }

    private static boolean isUrl(String value) {
        String candidate = value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
        return candidate.startsWith("http://") || candidate.startsWith("https://") || candidate.startsWith("file:");
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

    record Selection(boolean cloud, String family, String runtime, String cloudProvider, String cloudModel) {
        static Selection local(String family, String runtime) {
            return new Selection(false, normalize(family, "CODEX"), normalize(runtime, "CLI"), "", "");
        }

        static Selection cloud(String provider, String model) {
            return new Selection(true, "", "", normalize(provider, "openai").toLowerCase(Locale.ROOT),
                    model == null ? "" : model.trim());
        }

        static Selection fromClient(String client) {
            return switch (normalize(client, "CODEX")) {
                case "CLAUDE_CODE" -> local("CLAUDE", "CLI");
                case "COPILOT_CLI" -> local("COPILOT", "CLI");
                default -> local("CODEX", "CLI");
            };
        }

        String client() {
            return switch (family) {
                case "CLAUDE" -> "CLAUDE_CODE";
                case "COPILOT" -> "COPILOT_CLI";
                default -> "CODEX";
            };
        }

        String displayName() {
            String familyName = switch (family) {
                case "CLAUDE" -> "Claude";
                case "COPILOT" -> "GitHub Copilot";
                default -> "Codex";
            };
            String runtimeName = switch (runtime) {
                case "IDE_PLUGIN" -> "plugin";
                case "DESKTOP_APP" -> "desktop app";
                default -> "CLI";
            };
            return familyName + " " + runtimeName;
        }

        private static String normalize(String value, String fallback) {
            String normalized = value == null || value.isBlank() ? fallback : value.trim();
            return normalized.toUpperCase(Locale.ROOT).replace('-', '_').replace(' ', '_');
        }
    }
}
