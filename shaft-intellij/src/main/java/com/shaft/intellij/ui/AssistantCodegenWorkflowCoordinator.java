package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.mcp.ShaftMcpToolResult;

import java.io.IOException;
import java.io.Reader;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Project-scoped state and consent boundary for free-text AutoBot code generation. */
@Service(Service.Level.PROJECT)
public final class AssistantCodegenWorkflowCoordinator implements Disposable {
    static final String PROPOSAL_PREFIX = "SHAFT_CODEGEN_PROPOSAL ";
    static final String PROGRESS_PREFIX = "SHAFT_CODEGEN_PROGRESS ";
    static final String RESULT_PREFIX = "SHAFT_CODEGEN_RESULT ";
    private static final String URL_QUESTION = "What URL should AutoBot record?";
    private static final Pattern URL = Pattern.compile("(?i)\\b(?:https?://|file:/)[^\\s,;)}\\]]+");
    private static final Pattern HEAL_RETRY_OVERRIDE = Pattern.compile(
            "(?i)\\b(?:allow|use|try|perform|up\\s+to)\\s+(\\d{1,2})\\s+(?:heal|healing)\\s+retries\\b");
    private static final Pattern CLASS_NAME = Pattern.compile("[A-Za-z_$][A-Za-z0-9_.$]*");
    private static final String CUSTOM_PROPERTIES = "src/main/resources/properties/custom.properties";
    private static final String SHAFT_MCP_TOOL_PREFIX = "mcp__shaft-mcp__";
    private static final List<String> RECORD_MCP_TOOLS = List.of(
            "capture_checkpoint",
            "capture_pick_locator",
            "capture_set_mode",
            "capture_start",
            "capture_status",
            "capture_stop",
            "browser_accessibility_audit",
            "browser_aria_snapshot",
            "browser_get_current_url",
            "browser_get_page_dom",
            "browser_get_title",
            "browser_navigate",
            "browser_navigate_back",
            "browser_navigate_forward",
            "browser_network_requests",
            "browser_refresh",
            "browser_set_window_size",
            "browser_take_screenshot",
            "element_clear",
            "element_click",
            "element_drag_and_drop",
            "element_hover",
            "element_is_displayed",
            "element_is_enabled",
            "element_is_selected",
            "element_type",
            "element_upload_file");
    private final Map<String, Workflow> workflows = new ConcurrentHashMap<>();

    static AssistantCodegenWorkflowCoordinator getInstance(Project project) {
        if (project == null) {
            return new AssistantCodegenWorkflowCoordinator();
        }
        AssistantCodegenWorkflowCoordinator service = project.getService(AssistantCodegenWorkflowCoordinator.class);
        return service == null ? new AssistantCodegenWorkflowCoordinator() : service;
    }

    AssistantCommand.Invocation route(
            String sessionId,
            String userText,
            AssistantCommand.Invocation baseInvocation,
            String workingDirectory) {
        String key = sessionKey(sessionId);
        Workflow current = workflows.get(key);
        if (current != null) {
            AssistantCommand.Invocation currentRoute = routeCurrentWorkflow(key, current, userText, baseInvocation);
            if (currentRoute != null) {
                return currentRoute;
            }
        }
        if (!scenarioCodegen(baseInvocation, userText)) {
            return baseInvocation == null
                    ? AssistantCommand.Invocation.local("No AutoBot codegen workflow is awaiting input.")
                    : baseInvocation;
        }
        if (!supportsRecordToolSandbox(baseInvocation)) {
            return AssistantCommand.Invocation.local(
                    "AutoBot scenario codegen requires Codex CLI. The selected client cannot enforce "
                            + "the browser/capture-only RECORD tool boundary before edit consent.");
        }
        if (hasCustomCommand(baseInvocation)) {
            return AssistantCommand.Invocation.local(
                    "AutoBot scenario codegen requires a supported built-in Local / CLI client. "
                            + "Custom Agent commands cannot enforce the read-only RECORD boundary.");
        }
        Workflow workflow = new Workflow(scenario(userText), baseInvocation, workingDirectory);
        workflows.put(key, workflow);
        String scenarioUrl = firstUrl(workflow.scenario);
        String configuredUrl = configuredBaseUrl(workingDirectory);
        workflow.targetUrl = scenarioUrl.isBlank() ? configuredUrl : scenarioUrl;
        if (workflow.targetUrl.isBlank()) {
            workflow.phase = Phase.AWAITING_URL;
            return AssistantCommand.Invocation.local(URL_QUESTION);
        }
        return recordInvocation(workflow);
    }

    private AssistantCommand.Invocation routeCurrentWorkflow(
            String key,
            Workflow current,
            String userText,
            AssistantCommand.Invocation baseInvocation) {
        if (current.phase == Phase.AWAITING_URL) {
            return acceptUrl(current, userText);
        }
        if (current.phase == Phase.AWAITING_EDIT_CONFIRMATION) {
            return acceptConsent(current, userText);
        }
        if (!current.phase.terminal()) {
            return AssistantCommand.Invocation.local("AutoBot codegen is already running in phase "
                    + current.phase + ". Cancel it before starting another workflow.");
        }
        if (baseInvocation == null) {
            return AssistantCommand.Invocation.local("This AutoBot codegen workflow already ended with "
                    + current.phase.name().toLowerCase(Locale.ROOT) + ". Start a new `/codegen` request.");
        }
        workflows.remove(key);
        return null;
    }

    ShaftMcpToolResult complete(String sessionId, ShaftMcpToolResult result, String workingDirectory) {
        Workflow workflow = workflows.get(sessionKey(sessionId));
        if (workflow == null || workflow.phase.terminal()) {
            return result;
        }
        if (workflow.phase == Phase.RECORD) {
            return completeRecord(workflow, result, workingDirectory);
        }
        if (workflow.phase == Phase.GENERATE || workflow.phase == Phase.REPLAY || workflow.phase == Phase.HEAL) {
            return completeMutable(workflow, result, workingDirectory);
        }
        return result;
    }

    Phase progress(String sessionId, String line) {
        Workflow workflow = workflows.get(sessionKey(sessionId));
        if (workflow == null || workflow.phase.terminal()) {
            return workflow == null ? Phase.IDLE : workflow.phase;
        }
        JsonObject progress = structuredObject(line, PROGRESS_PREFIX);
        if (progress == null || !progress.has("phase")) {
            return workflow.phase;
        }
        Phase reported;
        try {
            reported = Phase.valueOf(progress.get("phase").getAsString().trim().toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException | UnsupportedOperationException error) {
            return workflow.phase;
        }
        if (allowedProgressTransition(workflow, reported)) {
            if (reported == Phase.HEAL && workflow.phase != Phase.HEAL) {
                workflow.healAttempts++;
            }
            workflow.phase = reported;
        }
        return workflow.phase;
    }

    private static boolean allowedProgressTransition(Workflow workflow, Phase reported) {
        if (!(reported == Phase.GENERATE || reported == Phase.REPLAY || reported == Phase.HEAL)) {
            return false;
        }
        Phase current = workflow.phase;
        if (reported == Phase.HEAL && current != Phase.HEAL) {
            return workflow.healAttempts < workflow.healRetryAllowance;
        }
        return current.mutableOrder() <= reported.mutableOrder()
                || (current == Phase.HEAL && reported == Phase.REPLAY);
    }

    Phase phase(String sessionId) {
        Workflow workflow = workflows.get(sessionKey(sessionId));
        return workflow == null ? Phase.IDLE : workflow.phase;
    }

    String status(String sessionId) {
        Phase phase = phase(sessionId);
        return switch (phase) {
            case AWAITING_URL -> "AutoBot awaiting target URL";
            case RECORD -> "AutoBot recording";
            case AWAITING_EDIT_CONFIRMATION -> "AutoBot awaiting edit confirmation";
            case GENERATE -> "AutoBot generating";
            case REPLAY -> "AutoBot replaying";
            case HEAL -> "AutoBot healing";
            case SUCCEEDED -> "AutoBot codegen passed";
            case DENIED -> "AutoBot codegen denied";
            case CANCELLED -> "AutoBot codegen cancelled";
            case DISPOSED -> "AutoBot codegen disposed";
            case FAILED -> "AutoBot codegen failed";
            case IDLE -> "";
        };
    }

    void cancel(String sessionId) {
        terminate(sessionId, Phase.CANCELLED);
    }

    void disposeSession(String sessionId) {
        terminate(sessionId, Phase.DISPOSED);
    }

    @Override
    public void dispose() {
        workflows.values().stream()
                .filter(workflow -> !workflow.phase.terminal())
                .forEach(workflow -> workflow.phase = Phase.DISPOSED);
    }

    private AssistantCommand.Invocation acceptUrl(Workflow workflow, String response) {
        String supplied = firstUrl(response);
        if (supplied.isBlank()) {
            workflow.phase = Phase.FAILED;
            return AssistantCommand.Invocation.local(
                    "A target URL was not supplied after the single URL question. AutoBot codegen cancelled.");
        }
        workflow.targetUrl = supplied;
        return recordInvocation(workflow);
    }

    private AssistantCommand.Invocation acceptConsent(Workflow workflow, String response) {
        String decision = normalizeDecision(response);
        if (explicitDenial(decision)) {
            workflow.phase = Phase.DENIED;
            return AssistantCommand.Invocation.local(
                    "AutoBot codegen stopped at the proposal. No mutable invocation was launched.");
        }
        if (!explicitApproval(decision)) {
            return AssistantCommand.Invocation.local(
                    workflow.proposalMarkdown
                            + "\n\nNo source files or replay have run. Send `approve edits` to continue or `deny` to stop.");
        }
        workflow.phase = Phase.GENERATE;
        JsonObject arguments = workflow.baseArguments.deepCopy();
        arguments.addProperty("mode", "AGENT");
        arguments.addProperty("allowSourceMutation", true);
        arguments.remove("readOnlyCodegenRecording");
        arguments.addProperty("healRetryAllowance", workflow.healRetryAllowance);
        arguments.addProperty("prompt", mutablePrompt(workflow));
        return AssistantCommand.Invocation.tool("autobot_local_agent_run", arguments)
                .routedVia(workflow.routedVia);
    }

    private static AssistantCommand.Invocation recordInvocation(Workflow workflow) {
        workflow.phase = Phase.RECORD;
        JsonObject arguments = workflow.baseArguments.deepCopy();
        arguments.addProperty("mode", "AGENT");
        arguments.addProperty("allowSourceMutation", false);
        arguments.addProperty("readOnlyCodegenRecording", true);
        arguments.remove("healRetryAllowance");
        arguments.addProperty("prompt", recordPrompt(workflow));
        return AssistantCommand.Invocation.tool("autobot_local_agent_run", arguments)
                .routedVia(workflow.routedVia);
    }

    static List<String> recordMcpTools() {
        return RECORD_MCP_TOOLS;
    }

    boolean allowsRecordMcpTool(String sessionId, String toolName) {
        Workflow workflow = workflows.get(sessionKey(sessionId));
        if (workflow == null || workflow.phase != Phase.RECORD || toolName == null) {
            return false;
        }
        String normalized = toolName.startsWith(SHAFT_MCP_TOOL_PREFIX)
                ? toolName.substring(SHAFT_MCP_TOOL_PREFIX.length())
                : toolName;
        return RECORD_MCP_TOOLS.contains(normalized);
    }

    private static ShaftMcpToolResult completeRecord(
            Workflow workflow, ShaftMcpToolResult result, String workingDirectory) {
        if (result == null || !result.success()) {
            workflow.phase = Phase.FAILED;
            return result == null ? ShaftMcpToolResult.failure("AutoBot recording returned no result.") : result;
        }
        JsonObject proposal = structuredObject(
                AssistantLocalAgentRunner.StructuredStreamParser.modelOutput(result.output()), PROPOSAL_PREFIX);
        if (!validProposal(proposal)) {
            workflow.phase = Phase.FAILED;
            return ShaftMcpToolResult.failure(
                    "AutoBot recording ended without a valid structured proposal. No source mutation was launched.");
        }
        String recordingPath = proposal.get("recordingPath").getAsString().trim();
        if (!AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH.equals(recordingPath)) {
            workflow.phase = Phase.FAILED;
            return ShaftMcpToolResult.failure("AutoBot returned recording path `" + recordingPath
                    + "`; expected unchanged route `" + AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH + "`.");
        }
        if (!realCanonicalRecording(workingDirectory)) {
            workflow.phase = Phase.FAILED;
            return ShaftMcpToolResult.failure(
                    "AutoBot did not save a regular recording file at the canonical project path `"
                            + AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH + "`.");
        }
        workflow.recordingPath = recordingPath;
        workflow.proposal = proposal.deepCopy();
        workflow.proposalMarkdown = proposal.get("proposalMarkdown").getAsString().trim();
        workflow.phase = Phase.AWAITING_EDIT_CONFIRMATION;
        return ShaftMcpToolResult.success("**AutoBot recording and reuse proposal**\n\n"
                + workflow.proposalMarkdown
                + "\n\nRecording: `" + workflow.recordingPath + "`"
                + "\n\nNo source files or replay have run. Send `approve edits` to continue or `deny` to stop.");
    }

    private static ShaftMcpToolResult completeMutable(
            Workflow workflow,
            ShaftMcpToolResult result,
            String workingDirectory) {
        if (result == null || !result.success()) {
            workflow.phase = Phase.FAILED;
            return result == null ? ShaftMcpToolResult.failure("AutoBot mutable run returned no result.") : result;
        }
        JsonObject terminal = structuredObject(
                AssistantLocalAgentRunner.StructuredStreamParser.modelOutput(result.output()), RESULT_PREFIX);
        if (!validTerminal(terminal)) {
            workflow.phase = Phase.FAILED;
            return ShaftMcpToolResult.failure(
                    "AutoBot mutable run ended without a valid structured terminal summary.");
        }
        String status = terminal.get("status").getAsString().trim().toLowerCase(Locale.ROOT);
        workflow.phase = "passed".equals(status) ? Phase.SUCCEEDED : Phase.FAILED;
        return "passed".equals(status)
                ? ShaftMcpToolResult.success(terminalMarkdown(terminal, workingDirectory))
                : ShaftMcpToolResult.failure(terminalMarkdown(terminal, workingDirectory));
    }

    private static String recordPrompt(Workflow workflow) {
        JsonObject state = new JsonObject();
        state.addProperty("scenario", workflow.scenario);
        state.addProperty("targetUrl", workflow.targetUrl);
        state.addProperty("recordingPath", AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH);
        return """
                AutoBot free-text codegen, read-only RECORD invocation. You own all browser and SHAFT MCP actions; the IntelliJ plugin coordinates state and presentation only.
                Load and follow `$shaft-test-recording`, `$shaft-locator-design`, and `$shaft-web-actions` plus the repository's SHAFT recording guidance. Use negotiated SHAFT MCP/CLI capabilities to call capture_start with the exact state below, record each requested assertion with capture_checkpoint, perform the requested browser journey, verify capture_status has no unresolved readiness warning, and call capture_stop/save. Then inspect existing test classes and page objects and propose which existing owners to reuse and which owners are genuinely missing. Do not edit source. Do not generate, replay, or heal. Preserve MCP timeout, cancellation, shutdown, and client lifecycle.

                Workflow state:
                %s

                End with exactly one parseable line:
                SHAFT_CODEGEN_PROPOSAL {"recordingPath":"recordings/intellij-capture.json","proposalMarkdown":"...","phaseOutcomes":{"RECORD":"passed"}}
                """.formatted(state).stripIndent().trim();
    }

    private static String mutablePrompt(Workflow workflow) {
        JsonObject state = new JsonObject();
        state.addProperty("scenario", workflow.scenario);
        state.addProperty("targetUrl", workflow.targetUrl);
        state.addProperty("recordingPath", workflow.recordingPath);
        state.add("proposal", workflow.proposal.deepCopy());
        state.addProperty("healRetryAllowance", workflow.healRetryAllowance);
        return """
                AutoBot free-text codegen, approved mutable invocation. Explicit edit consent was granted before this process launched. You own GENERATE, REPLAY, and HEAL through negotiated SHAFT MCP and local project tools; the IntelliJ plugin coordinates state and presentation only.
                Load and follow `$shaft-automated-test-authoring`, `$shaft-page-objects`, `$shaft-recording-codegen`, `$shaft-locator-design`, `$shaft-web-actions`, and `$shaft-change-verification` plus the repository's SHAFT test-generation guidance. Before any source edit, inspect existing test classes and page objects and call `shaft_coding_partner_plan` (or the negotiated equivalent owner/reuse planning capability). Reuse the approved proposal, saved recording, existing test classes, page objects, locator fields, and action methods. Create a new owner only when none fits.
                Follow the Page Object Model and the project's existing package and class ownership. Keep locators and page actions in page objects, not test methods. Locator priority is stable author-written IDs first, then existing project locator conventions, then accessible semantic locators, with XPath only as a last fallback. Use SHAFT syntax and fluent action chaining.
                Make the smallest source change, reporting exact changed and created class names. Replay once. On failure, heal and replay no more than the exact healRetryAllowance in state; never inherit another default and never weaken assertions. Preserve MCP timeout, cancellation, shutdown, and client lifecycle.
                Emit a parseable progress line when each phase starts: SHAFT_CODEGEN_PROGRESS {"phase":"GENERATE|REPLAY|HEAL"}

                Workflow state:
                %s

                End with exactly one parseable line:
                SHAFT_CODEGEN_RESULT {"status":"passed|failed","phaseOutcomes":{"RECORD":"...","GENERATE":"...","REPLAY":"...","HEAL":"..."},"changedClasses":[],"createdClasses":[],"reportPath":"allure-report/AllureReport.html or blank"}
                """.formatted(state).stripIndent().trim();
    }

    private static String terminalMarkdown(JsonObject terminal, String workingDirectory) {
        String status = terminal.get("status").getAsString().trim().toLowerCase(Locale.ROOT);
        StringBuilder markdown = new StringBuilder("**AutoBot codegen terminal summary**\n\nStatus: **")
                .append(status).append("**\n\nPhase outcomes:\n");
        for (Map.Entry<String, JsonElement> entry : terminal.getAsJsonObject("phaseOutcomes").entrySet()) {
            markdown.append("- ").append(entry.getKey()).append(": ")
                    .append(entry.getValue().getAsString()).append('\n');
        }
        markdown.append("\nChanged classes: ").append(classList(terminal.getAsJsonArray("changedClasses")))
                .append("\n\nCreated classes: ").append(classList(terminal.getAsJsonArray("createdClasses")));
        if (realAllureReport(terminal, workingDirectory)) {
            markdown.append("\n\n[Allure report](allure-report/AllureReport.html)");
        }
        return markdown.toString();
    }

    private static boolean realAllureReport(JsonObject terminal, String workingDirectory) {
        if (!terminal.has("reportPath")
                || !"allure-report/AllureReport.html".equals(terminal.get("reportPath").getAsString().trim())
                || workingDirectory == null
                || workingDirectory.isBlank()) {
            return false;
        }
        Path root = Path.of(workingDirectory).toAbsolutePath().normalize();
        Path report = root.resolve("allure-report/AllureReport.html").normalize();
        return report.startsWith(root) && Files.isRegularFile(report);
    }

    private static String classList(JsonArray values) {
        List<String> names = new ArrayList<>();
        values.forEach(value -> names.add("`" + value.getAsString() + "`"));
        return names.isEmpty() ? "none" : String.join(", ", names);
    }

    private static boolean validProposal(JsonObject proposal) {
        if (proposal == null) {
            return false;
        }
        return nonBlankString(proposal, "recordingPath")
                && nonBlankString(proposal, "proposalMarkdown")
                && proposal.has("phaseOutcomes")
                && proposal.get("phaseOutcomes").isJsonObject()
                && nonBlankOutcome(proposal.getAsJsonObject("phaseOutcomes"), "RECORD");
    }

    private static boolean validTerminal(JsonObject terminal) {
        if (!validTerminalShape(terminal)) {
            return false;
        }
        String status = terminal.get("status").getAsString().trim().toLowerCase(Locale.ROOT);
        JsonArray changedClasses = terminal.getAsJsonArray("changedClasses");
        JsonArray createdClasses = terminal.getAsJsonArray("createdClasses");
        JsonObject phaseOutcomes = terminal.getAsJsonObject("phaseOutcomes");
        if (!validTerminalValues(status, changedClasses, createdClasses, phaseOutcomes)) {
            return false;
        }
        return !"passed".equals(status) || validPassedTerminal(changedClasses, createdClasses, phaseOutcomes);
    }

    private static boolean validTerminalShape(JsonObject terminal) {
        return terminal != null
                && nonBlankString(terminal, "status")
                && terminal.has("phaseOutcomes")
                && terminal.get("phaseOutcomes").isJsonObject()
                && terminal.has("changedClasses")
                && terminal.get("changedClasses").isJsonArray()
                && terminal.has("createdClasses")
                && terminal.get("createdClasses").isJsonArray()
                && optionalString(terminal, "reportPath");
    }

    private static boolean validTerminalValues(
            String status, JsonArray changedClasses, JsonArray createdClasses, JsonObject phaseOutcomes) {
        return ("passed".equals(status) || "failed".equals(status))
                && validClassNames(changedClasses)
                && validClassNames(createdClasses)
                && validPhaseOutcomes(phaseOutcomes);
    }

    private static boolean validPassedTerminal(
            JsonArray changedClasses, JsonArray createdClasses, JsonObject phaseOutcomes) {
        return nonBlankOutcome(phaseOutcomes, "GENERATE")
                && nonBlankOutcome(phaseOutcomes, "REPLAY")
                && nonBlankOutcome(phaseOutcomes, "HEAL")
                && (!changedClasses.isEmpty() || !createdClasses.isEmpty());
    }

    private static boolean validPhaseOutcomes(JsonObject outcomes) {
        if (outcomes.entrySet().isEmpty()) {
            return false;
        }
        for (Map.Entry<String, JsonElement> outcome : outcomes.entrySet()) {
            String phase = outcome.getKey();
            if (!("RECORD".equals(phase) || "GENERATE".equals(phase)
                    || "REPLAY".equals(phase) || "HEAL".equals(phase))
                    || !nonBlankOutcome(outcomes, phase)) {
                return false;
            }
        }
        return true;
    }

    private static boolean nonBlankOutcome(JsonObject outcomes, String phase) {
        return outcomes.has(phase)
                && outcomes.get(phase).isJsonPrimitive()
                && outcomes.get(phase).getAsJsonPrimitive().isString()
                && !outcomes.get(phase).getAsString().isBlank();
    }

    private static boolean realCanonicalRecording(String workingDirectory) {
        if (workingDirectory == null || workingDirectory.isBlank()) {
            return false;
        }
        Path root = Path.of(workingDirectory).toAbsolutePath().normalize();
        Path recording = root.resolve(AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH).normalize();
        if (!recording.startsWith(root)
                || !Files.isRegularFile(recording, LinkOption.NOFOLLOW_LINKS)
                || Files.isSymbolicLink(recording)) {
            return false;
        }
        try {
            return recording.toRealPath().startsWith(root.toRealPath());
        } catch (IOException | RuntimeException error) {
            return false;
        }
    }

    private static boolean validClassNames(JsonArray values) {
        for (JsonElement value : values) {
            if (!value.isJsonPrimitive()
                    || !value.getAsJsonPrimitive().isString()
                    || !CLASS_NAME.matcher(value.getAsString()).matches()) {
                return false;
            }
        }
        return true;
    }

    private static boolean nonBlankString(JsonObject object, String key) {
        return object.has(key)
                && object.get(key).isJsonPrimitive()
                && object.get(key).getAsJsonPrimitive().isString()
                && !object.get(key).getAsString().isBlank();
    }

    private static boolean optionalString(JsonObject object, String key) {
        return !object.has(key)
                || (object.get(key).isJsonPrimitive() && object.get(key).getAsJsonPrimitive().isString());
    }

    private static JsonObject structuredObject(String output, String prefix) {
        if (output == null || output.isBlank()) {
            return null;
        }
        String[] lines = output.split("\\R");
        for (int index = lines.length - 1; index >= 0; index--) {
            String line = lines[index].trim();
            if (line.isBlank()) {
                continue;
            }
            if (!line.startsWith(prefix)) {
                return null;
            }
            try {
                JsonElement parsed = JsonParser.parseString(line.substring(prefix.length()).trim());
                return parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
            } catch (RuntimeException error) {
                return null;
            }
        }
        return null;
    }

    private static String configuredBaseUrl(String workingDirectory) {
        if (workingDirectory == null || workingDirectory.isBlank()) {
            return "";
        }
        Path file = Path.of(workingDirectory).resolve(CUSTOM_PROPERTIES);
        if (!Files.isRegularFile(file)) {
            return "";
        }
        Properties properties = new Properties();
        try (Reader reader = Files.newBufferedReader(file, StandardCharsets.UTF_8)) {
            properties.load(reader);
        } catch (IOException | RuntimeException error) {
            return "";
        }
        String candidate = properties.getProperty("baseURL", "").trim();
        return validUrl(candidate) ? candidate : "";
    }

    private static String firstUrl(String text) {
        Matcher matcher = URL.matcher(text == null ? "" : text);
        if (!matcher.find()) {
            return "";
        }
        String candidate = matcher.group();
        return validUrl(candidate) ? candidate : "";
    }

    private static boolean validUrl(String candidate) {
        if (candidate == null || candidate.isBlank()) {
            return false;
        }
        try {
            URI uri = URI.create(candidate);
            return "file".equalsIgnoreCase(uri.getScheme())
                    ? uri.getPath() != null && !uri.getPath().isBlank()
                    : ("http".equalsIgnoreCase(uri.getScheme()) || "https".equalsIgnoreCase(uri.getScheme()))
                    && uri.getHost() != null;
        } catch (IllegalArgumentException error) {
            return false;
        }
    }

    private static int healRetryAllowance(String scenario) {
        Matcher matcher = HEAL_RETRY_OVERRIDE.matcher(scenario == null ? "" : scenario);
        if (!matcher.find()) {
            return 1;
        }
        int requested = Integer.parseInt(matcher.group(1));
        return requested > 1 ? requested : 1;
    }

    private static boolean scenarioCodegen(AssistantCommand.Invocation invocation, String userText) {
        if (invocation == null
                || !"autobot_local_agent_run".equals(invocation.toolName())
                || userText == null) {
            return false;
        }
        return userText.stripLeading().toLowerCase(Locale.ROOT).startsWith("/codegen");
    }

    private static boolean hasCustomCommand(AssistantCommand.Invocation invocation) {
        JsonElement command = invocation.arguments().get("command");
        return command != null && command.isJsonArray() && !command.getAsJsonArray().isEmpty();
    }

    private static boolean supportsRecordToolSandbox(AssistantCommand.Invocation invocation) {
        JsonElement client = invocation.arguments().get("client");
        if (client == null || !client.isJsonPrimitive() || !client.getAsJsonPrimitive().isString()) {
            return false;
        }
        String normalized = client.getAsString().trim().toUpperCase(Locale.ROOT);
        return "CODEX".equals(normalized);
    }

    private static String scenario(String userText) {
        String trimmed = userText == null ? "" : userText.trim();
        return trimmed.replaceFirst("(?is)^/codegen(?:\\s+|$)", "").trim();
    }

    private static String normalizeDecision(String response) {
        return response == null ? "" : response.trim().toLowerCase(Locale.ROOT)
                .replaceAll("[.!]+$", "")
                .trim();
    }

    private static boolean explicitApproval(String decision) {
        return decision.matches("approve(?: edits)?|approved|yes(?:,? proceed)?|proceed|continue");
    }

    private static boolean explicitDenial(String decision) {
        return decision.matches("deny|denied|no|cancel|stop");
    }

    private void terminate(String sessionId, Phase terminal) {
        Workflow workflow = workflows.get(sessionKey(sessionId));
        if (workflow != null && !workflow.phase.terminal()) {
            workflow.phase = terminal;
        }
    }

    private static String sessionKey(String sessionId) {
        return sessionId == null || sessionId.isBlank() ? "default" : sessionId;
    }

    enum Phase {
        IDLE,
        AWAITING_URL,
        RECORD,
        AWAITING_EDIT_CONFIRMATION,
        GENERATE,
        REPLAY,
        HEAL,
        SUCCEEDED,
        FAILED,
        DENIED,
        CANCELLED,
        DISPOSED;

        boolean terminal() {
            return this == SUCCEEDED || this == FAILED || this == DENIED || this == CANCELLED || this == DISPOSED;
        }

        int mutableOrder() {
            return switch (this) {
                case GENERATE -> 0;
                case REPLAY -> 1;
                case HEAL -> 2;
                default -> Integer.MAX_VALUE;
            };
        }
    }

    private static final class Workflow {
        private final String scenario;
        private final JsonObject baseArguments;
        private final String routedVia;
        private final int healRetryAllowance;
        private String targetUrl = "";
        private String recordingPath = "";
        private String proposalMarkdown = "";
        private JsonObject proposal = new JsonObject();
        private Phase phase = Phase.IDLE;
        private int healAttempts;

        private Workflow(
                String scenario,
                AssistantCommand.Invocation baseInvocation,
                String workingDirectory) {
            this.scenario = scenario;
            this.baseArguments = baseInvocation.arguments().deepCopy();
            this.baseArguments.addProperty("workingDirectory", workingDirectory == null ? "" : workingDirectory);
            this.routedVia = baseInvocation.routedVia();
            this.healRetryAllowance = healRetryAllowance(scenario);
        }
    }
}
