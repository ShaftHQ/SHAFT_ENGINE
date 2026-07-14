package com.shaft.intellij.ui;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;
import java.util.regex.Pattern;

/**
 * Converts Assistant and MCP payloads into Markdown suitable for display.
 */
final class AssistantMarkdown {
    private static final Gson PRETTY = new GsonBuilder().setPrettyPrinting().create();
    private static final Set<String> KNOWN_TOOLS = Set.of(
            "autobot_local_agent_clients",
            "autobot_local_agent_run",
            "autobot_provider_chat",
            "autobot_provider_status",
            "shaft_coding_partner_diff",
            "verify_run_focused");
    private static final Pattern EXCEPTION_CLASS_PREFIX = Pattern.compile(
            "^(?:[a-zA-Z_$][\\w$]*\\.)+[A-Za-z_$][\\w$]*(?:Exception|Error)\\s*:\\s*");

    private AssistantMarkdown() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Turns a failed tool/agent invocation's exception into a plain-language message.
     *
     * <p>Unwraps common async-completion wrappers, strips a leading fully-qualified exception
     * class name when the underlying message already reads as plain text, and always returns a
     * non-blank result so callers never need to null-check before rendering it.</p>
     *
     * @param error the failure, or null
     * @return a safe, human-readable message
     */
    static String humanizeError(Throwable error) {
        if (error == null) {
            return "";
        }
        Throwable cause = rootCause(error);
        String message = cause.getMessage();
        if (message == null || message.isBlank()) {
            return "SHAFT Assistant hit an unexpected " + simpleName(cause)
                    + ". Check the SHAFT MCP connection in Settings, then try again.";
        }
        String cleaned = EXCEPTION_CLASS_PREFIX.matcher(message.strip()).replaceFirst("");
        return cleaned.isBlank() ? message.strip() : cleaned;
    }

    /**
     * Renders a failed tool result with a consistent "short headline + details + one next action"
     * shape (issue #3513 S3). A body that already leads with its own bold headline (a structured
     * failure narrative such as the Capture codegen report) is returned unchanged so it is not
     * double-headlined; a plain-text failure (a humanized error) gets the headline and a single
     * next-action line.
     *
     * @param toolName the MCP tool that failed
     * @param body the already-rendered result body (may be blank)
     * @return the failure markdown
     */
    static String toolFailureMarkdown(String toolName, String body) {
        String trimmed = body == null ? "" : body.strip();
        if (trimmed.startsWith("**")) {
            return trimmed;
        }
        List<String> sections = new ArrayList<>();
        sections.add("**" + ShaftStatusPresentation.ERROR_ICON + " `"
                + (toolName == null ? "" : toolName.trim()) + "` couldn't finish**");
        if (!trimmed.isBlank()) {
            sections.add(trimmed);
        }
        sections.add("_Next: review the details above and try again — or check the SHAFT MCP "
                + "connection in Settings if the tool can't reach SHAFT._");
        return joinSections(sections);
    }

    private static Throwable rootCause(Throwable error) {
        Throwable current = error;
        while ((current instanceof CompletionException || current instanceof ExecutionException)
                && current.getCause() != null) {
            current = current.getCause();
        }
        return current;
    }

    private static String simpleName(Throwable error) {
        String name = error.getClass().getSimpleName();
        return name.isBlank() ? "error" : name;
    }

    static String fromMcpOutput(String output) {
        return normalizeMarkdown(unwrapMcpText(output));
    }

    static String fromMcpOutput(String toolName, String output) {
        String unwrapped = unwrapMcpText(output);
        JsonElement parsed = parse(unwrapped);
        if (parsed != null) {
            String known = knownToolMarkdown(toolName, parsed);
            if (!known.isBlank()) {
                return known;
            }
            String generic = genericMarkdown(parsed);
            if (!generic.isBlank()) {
                return generic;
            }
        }
        return normalizeMarkdown(unwrapped);
    }

    static JsonObject jsonObjectFromMcpOutput(String output) {
        JsonElement parsed = parse(unwrapMcpText(output));
        return parsed != null && parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
    }

    static boolean shouldFormatWithAgent(String toolName, String output) {
        if (KNOWN_TOOLS.contains(toolName)) {
            return false;
        }
        JsonElement parsed = parse(unwrapMcpText(output));
        return parsed != null && (parsed.isJsonObject() || parsed.isJsonArray()) && genericMarkdown(parsed).isBlank();
    }

    static String formatterPrompt(String toolName, String output) {
        return """
                Convert this SHAFT MCP tool response into concise, user-facing Markdown.
                Preserve facts and code exactly. Do not invent missing information.
                Use headings, bullets, tables, or fenced code blocks only where useful.

                Tool: %s

                Response:
                ```json
                %s
                ```
                """.formatted(toolName == null ? "" : toolName, clip(unwrapMcpText(output), 12_000)).strip();
    }

    static String normalizeMarkdown(String text) {
        if (text == null || text.isBlank()) {
            return "_No response returned._";
        }
        String trimmed = text.trim();
        if (containsCodeFence(trimmed)) {
            return trimmed;
        }
        String prettyJson = prettyJson(trimmed);
        if (!prettyJson.isBlank()) {
            return fence("json", prettyJson);
        }
        if (looksLikeJava(trimmed)) {
            return fence("java", trimmed);
        }
        return trimmed;
    }

    private static String knownToolMarkdown(String toolName, JsonElement parsed) {
        return switch (toolName == null ? "" : toolName) {
            case "autobot_local_agent_clients" -> clientsMarkdown(parsed);
            case "autobot_local_agent_run" -> localAgentMarkdown(parsed);
            case "autobot_provider_chat" -> providerChatMarkdown(parsed);
            case "autobot_provider_status" -> providerStatusMarkdown(parsed);
            case "shaft_coding_partner_diff" -> codingPartnerDiffMarkdown(parsed);
            case "verify_run_focused" -> verifyMarkdown(parsed);
            case "healer_run_failed_test", "playwright_healer_run_failed_test" -> healerMarkdown(parsed);
            default -> "";
        };
    }

    private static String clientsMarkdown(JsonElement parsed) {
        if (!parsed.isJsonArray()) {
            return "";
        }
        JsonArray clients = parsed.getAsJsonArray();
        if (clients.isEmpty()) {
            return "_No local assistant clients returned._";
        }
        StringBuilder markdown = new StringBuilder("""
                | Client | Command | SHAFT API key |
                | --- | --- | --- |
                """);
        for (JsonElement item : clients) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject client = item.getAsJsonObject();
            markdown.append("| ")
                    .append(table(string(client, "displayName", string(client, "id", "Unknown"))))
                    .append(" | `")
                    .append(string(client, "executableName", ""))
                    .append("` | ")
                    .append(booleanValue(client, "requiresCloudApiKey") ? "Required" : "Not required")
                    .append(" |\n");
        }
        return markdown.toString().trim();
    }

    private static String localAgentMarkdown(JsonElement parsed) {
        if (!parsed.isJsonObject()) {
            return "";
        }
        JsonObject response = parsed.getAsJsonObject();
        if (!response.has("stdout") && !response.has("stderr") && !response.has("status")) {
            return "";
        }
        String stdout = string(response, "stdout", "");
        String stderr = string(response, "stderr", "");
        List<String> sections = new ArrayList<>();
        if (!stdout.isBlank()) {
            sections.add(normalizeMarkdown(stdout));
        }
        sections.add(metadataLine(
                "Status", string(response, "status", ""),
                "Client", string(response, "client", ""),
                "Mode", string(response, "mode", ""),
                "Exit", string(response, "exitCode", "")));
        String warnings = warnings(response);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        if (!stderr.isBlank() && (!"SUCCESS".equals(string(response, "status", "")) || stdout.isBlank())) {
            sections.add("**stderr**\n\n" + fence("text", stderr));
        }
        return joinSections(sections);
    }

    private static String providerChatMarkdown(JsonElement parsed) {
        if (!parsed.isJsonObject()) {
            return "";
        }
        JsonObject response = parsed.getAsJsonObject();
        if (!response.has("answer") && !response.has("provider") && !response.has("fallbackReason")) {
            return "";
        }
        List<String> sections = new ArrayList<>();
        String answer = string(response, "answer", "");
        String summary = string(response, "summary", "");
        boolean hasCodeBlocks = hasNonEmptyCodeBlocks(response);
        appendProviderChatContent(sections, response, answer, summary, hasCodeBlocks);
        sections.add(metadataLine(
                "Status", string(response, "status", ""),
                "Provider", string(response, "provider", ""),
                "Model", string(response, "model", ""),
                "Mode", string(response, "mode", "")));
        String warnings = warnings(response);
        String fallback = string(response, "fallbackReason", "");
        appendProviderChatFooter(sections, warnings, fallback);
        if (isEmptyProviderChatResponse(answer, summary, warnings, fallback, hasCodeBlocks)) {
            sections.add("_No answer returned._");
        }
        return joinSections(sections);
    }

    private static void appendProviderChatContent(
            List<String> sections, JsonObject response, String answer, String summary, boolean hasCodeBlocks) {
        if (!answer.isBlank()) {
            sections.add(normalizeMarkdown(answer));
        }
        if (!summary.isBlank()) {
            sections.add("**Summary:** " + summary);
        }
        if (hasCodeBlocks) {
            appendNonBlank(sections, providerCodeBlocksMarkdown(response.getAsJsonArray("codeBlocks")));
        }
        appendNonBlank(sections, bulletList("Cited SHAFT guides", response, "citedGuideUrls"));
        appendNonBlank(sections, bulletList("Unverified locator assumptions", response, "locatorAssumptions"));
        String guardrailStatus = string(response, "guardrailStatus", "");
        if (!guardrailStatus.isBlank() && !"NOT_CHECKED".equals(guardrailStatus)) {
            sections.add("**Guardrails:** " + guardrailStatus);
        }
    }

    private static void appendProviderChatFooter(List<String> sections, String warnings, String fallback) {
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        if (!fallback.isBlank()) {
            sections.add("**Fallback reason:** " + fallback);
        }
    }

    private static boolean hasNonEmptyCodeBlocks(JsonObject response) {
        return response.has("codeBlocks") && response.get("codeBlocks").isJsonArray()
                && !response.getAsJsonArray("codeBlocks").isEmpty();
    }

    private static boolean isEmptyProviderChatResponse(
            String answer, String summary, String warnings, String fallback, boolean hasCodeBlocks) {
        return answer.isBlank() && summary.isBlank() && warnings.isBlank() && fallback.isBlank() && !hasCodeBlocks;
    }

    private static String providerCodeBlocksMarkdown(JsonArray blocks) {
        List<String> sections = new ArrayList<>();
        for (JsonElement item : blocks) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject block = item.getAsJsonObject();
            String code = string(block, "code", "");
            if (code.isBlank()) {
                continue;
            }
            String path = string(block, "path", "");
            String anchor = string(block, "insertionAnchor", "");
            StringBuilder header = new StringBuilder();
            if (!path.isBlank()) {
                header.append('`').append(path).append('`');
            }
            if (!anchor.isBlank()) {
                header.append(header.length() > 0 ? " after `" : "After `").append(anchor).append('`');
            }
            if (header.length() > 0) {
                sections.add("**" + header + "**");
            }
            sections.add(fence(string(block, "language", "java"), code));
        }
        return joinSections(sections);
    }

    private static String providerStatusMarkdown(JsonElement parsed) {
        if (!parsed.isJsonObject()) {
            return "";
        }
        JsonObject object = parsed.getAsJsonObject();
        if (!object.has("provider") || !object.has("apiKeyPresent")) {
            return "";
        }
        List<String> sections = new ArrayList<>();
        sections.add(metadataLine(
                "Provider", string(object, "provider", ""),
                "Model", string(object, "model", "")));
        sections.add(metadataLine(
                "API key", booleanValue(object, "apiKeyPresent") ? "present" : "missing",
                "Structured output", booleanValue(object, "structuredOutputSupported") ? "yes" : "no",
                "Modes", string(object, "supportedModes", "")));
        appendNonBlank(sections, warnings(object));
        return joinSections(sections);
    }

    private static String codingPartnerDiffMarkdown(JsonElement parsed) {
        if (!parsed.isJsonObject()) {
            return "";
        }
        JsonObject object = parsed.getAsJsonObject();
        if (!object.has("unifiedDiff") || !object.has("targetSourcePath")) {
            return "";
        }
        List<String> sections = new ArrayList<>();
        String anchor = string(object, "insertionAnchor", "");
        sections.add(metadataLine(
                "Patch preview", "`" + string(object, "targetSourcePath", "") + "`",
                "Anchor", anchor.isBlank() ? "class end" : "`" + anchor + "`",
                "Adds", string(object, "insertedLineCount", "0") + " line(s)"));
        String diff = string(object, "unifiedDiff", "");
        if (!diff.isBlank()) {
            sections.add(fence("diff", clip(diff, 8_000)));
        }
        appendNonBlank(sections, warnings(object));
        sections.add("_Review the diff, then apply it in IntelliJ under approval and run `/verify`._");
        return joinSections(sections);
    }

    private static String verifyMarkdown(JsonElement parsed) {
        if (!parsed.isJsonObject()) {
            return "";
        }
        JsonObject object = parsed.getAsJsonObject();
        if (!object.has("status") || !object.has("command")) {
            return "";
        }
        List<String> sections = new ArrayList<>();
        String status = string(object, "status", "");
        sections.add(metadataLine(
                "Verification", verifyStatusIcon(status) + " " + status,
                "Exit", string(object, "exitCode", "")));
        appendNonBlank(sections, verifyCommandLine(object));
        String output = string(object, "outputSummary", "");
        if (!output.isBlank()) {
            sections.add(fence("text", clip(output, 4_000)));
        }
        appendNonBlank(sections, warnings(object));
        return joinSections(sections);
    }

    private static String verifyStatusIcon(String status) {
        return switch (status) {
            case "PASSED" -> ShaftStatusPresentation.SUCCESS_ICON;
            case "TIMED_OUT" -> ShaftStatusPresentation.PENDING_ICON;
            default -> ShaftStatusPresentation.ERROR_ICON;
        };
    }

    private static String verifyCommandLine(JsonObject object) {
        if (!object.has("command") || !object.get("command").isJsonArray()) {
            return "";
        }
        StringBuilder command = new StringBuilder();
        for (JsonElement token : object.getAsJsonArray("command")) {
            if (token.isJsonPrimitive()) {
                command.append(token.getAsString()).append(' ');
            }
        }
        return command.length() == 0 ? "" : "**Command:** `" + command.toString().trim() + "`";
    }

    private static String genericMarkdown(JsonElement parsed) {
        if (parsed.isJsonObject()) {
            JsonObject object = parsed.getAsJsonObject();
            if (object.has("state") && object.has("outputPath") && object.has("processId")) {
                return captureStatusMarkdown(object);
            }
            String mobile = mobileMarkdown(object);
            if (!mobile.isBlank()) {
                return mobile;
            }
            String screenshot = screenshotMarkdown(object);
            if (!screenshot.isBlank()) {
                return screenshot;
            }
            String doctor = doctorMarkdown(object);
            if (!doctor.isBlank()) {
                return doctor;
            }
            String codingPartner = codingPartnerMarkdown(object);
            if (!codingPartner.isBlank()) {
                return codingPartner;
            }
            if (object.has("scenarios") && object.get("scenarios").isJsonArray()) {
                return scenariosMarkdown(object);
            }
            if (object.has("tools") && object.get("tools").isJsonArray()) {
                return toolsMarkdown(object.getAsJsonArray("tools"));
            }
            String captureReplay = captureReplayMarkdown(object);
            if (!captureReplay.isBlank()) {
                return captureReplay;
            }
            String backendComparison = backendComparisonMarkdown(object);
            if (!backendComparison.isBlank()) {
                return backendComparison;
            }
            String evidencePack = evidencePackMarkdown(object);
            if (!evidencePack.isBlank()) {
                return evidencePack;
            }
            if (object.has("codeBlocks") && object.get("codeBlocks").isJsonArray()) {
                return codeBlocksMarkdown(object.getAsJsonArray("codeBlocks"));
            }
            if (object.has("warnings") && object.get("warnings").isJsonArray()) {
                String warnings = warnings(object);
                if (!warnings.isBlank()) {
                    return warnings + "\n\n" + fence("json", PRETTY.toJson(object));
                }
            }
        }
        return "";
    }

    /**
     * Renders a Capture generation/replay result ({@code capture_generate_replay},
     * {@code capture_code_blocks}, and their Playwright/mobile siblings) as a full plain-language
     * story: what was generated where, whether it compiled, whether the replay actually passed (and
     * why a browser window opened at all), what went wrong, and what to do next. Before this, a
     * replay result rendered as its bare code blocks — or, with no blocks, as an unexplained
     * near-empty answer — which read as a silent "Done" (issue #3426 B4).
     */
    private static String captureReplayMarkdown(JsonObject object) {
        if (!object.has("successful") || !object.has("report") || !object.has("codeBlocks")) {
            return "";
        }
        JsonObject report = object.get("report") != null && object.get("report").isJsonObject()
                ? object.getAsJsonObject("report")
                : new JsonObject();
        boolean successful = booleanValue(object, "successful");
        JsonObject compilation = validationObject(report, "compilation");
        JsonObject replay = validationObject(report, "replay");
        String replayStatus = string(replay, "status", "SKIPPED");
        List<String> sections = new ArrayList<>();
        sections.add(successful
                ? "**" + ShaftStatusPresentation.SUCCESS_ICON + " Test generated, compiled, and verified**"
                : "**" + ShaftStatusPresentation.WARNING_ICON + " Test generation finished with problems — details below**");
        sections.add(captureReplayStory(object, report, compilation, replay));
        appendNonBlank(sections, validationDetails("Compilation", compilation));
        appendNonBlank(sections, validationDetails("Replay", replay));
        appendNonBlank(sections, bulletList("Inputs you must provide before running this test", report, "requiredUserInputs"));
        appendNonBlank(sections, bulletList("Potentially flaky steps", report, "flakySteps"));
        appendNonBlank(sections, bulletList("Events that could not be converted to code", report, "unsupportedEvents"));
        appendNonBlank(sections, warnings(object));
        appendNonBlank(sections, warnings(report));
        if (object.has("codeBlocks") && object.get("codeBlocks").isJsonArray()
                && !object.getAsJsonArray("codeBlocks").isEmpty()) {
            sections.add("**Generated code** (copy it into `src/test/java` in your project):");
            appendNonBlank(sections, codeBlocksMarkdown(object.getAsJsonArray("codeBlocks")));
        } else {
            sections.add(ShaftStatusPresentation.ERROR_ICON
                    + " **No usable code was produced.** The compilation/replay details above explain why; "
                    + "fix the reported problem (or re-record the flow) and ask again for a test generated "
                    + "from the recording.");
        }
        appendNonBlank(sections, captureReplayArtifacts(object));
        if ("PASSED".equals(replayStatus)) {
            sections.add("_Next: paste the class into your project and run `mvn -q test-compile` "
                    + "or your normal test run._");
        }
        return joinSections(sections);
    }

    /**
     * The plain-language "what actually happened, in order" narrative for a Capture codegen run.
     */
    private static String captureReplayStory(
            JsonObject object, JsonObject report, JsonObject compilation, JsonObject replay) {
        List<String> steps = new ArrayList<>();
        String sourcePath = firstTextProperty(object, "sourcePath").isBlank()
                ? string(report, "sourcePath", "")
                : firstTextProperty(object, "sourcePath");
        steps.add(sourcePath.isBlank()
                ? "1. SHAFT read your recording and tried to generate a Java TestNG class, but no source file was produced."
                : "1. SHAFT read your recording and generated the Java TestNG class `" + sourcePath + "`.");
        String testDataPath = firstTextProperty(object, "testDataPath");
        if (!testDataPath.isBlank()) {
            steps.add("2. Typed values and expected texts were externalized to `" + testDataPath
                    + "` so the test never hard-codes data.");
        }
        steps.add((testDataPath.isBlank() ? "2. " : "3. ") + validationStorySentence("compile", compilation));
        String replayStatus = string(replay, "status", "SKIPPED");
        String replaySentence = switch (replayStatus) {
            case "PASSED" -> "The generated test was then replayed end-to-end in a real browser — that is the "
                    + "browser window you may have seen open — and every step passed against the live site.";
            case "FAILED" -> "The generated test was then replayed in a real browser — that is the browser window "
                    + "you may have seen open (it starts on `about:blank` before the test navigates). The replay "
                    + "FAILED, so the code below is generated-but-unverified; the replay details explain the failing step.";
            default -> "Replay was skipped" + firstDiagnosticSuffix(replay) + ", so the code below compiled but was "
                    + "not re-executed against the live site.";
        };
        steps.add((testDataPath.isBlank() ? "3. " : "4. ") + replaySentence);
        return "**What happened**\n" + String.join("\n", steps);
    }

    private static String validationStorySentence(String verb, JsonObject validation) {
        return switch (string(validation, "status", "SKIPPED")) {
            case "PASSED" -> "The generated class was compiled against SHAFT to prove it is valid Java — it compiled cleanly.";
            case "FAILED" -> "The generated class FAILED to " + verb + "; the compilation details below list the errors.";
            default -> "Compilation was skipped" + firstDiagnosticSuffix(validation) + ".";
        };
    }

    private static String firstDiagnosticSuffix(JsonObject validation) {
        JsonElement diagnostics = validation.get("diagnostics");
        if (diagnostics == null || !diagnostics.isJsonArray() || diagnostics.getAsJsonArray().isEmpty()) {
            return "";
        }
        JsonElement first = diagnostics.getAsJsonArray().get(0);
        return first.isJsonPrimitive() ? " (" + first.getAsString() + ")" : "";
    }

    private static JsonObject validationObject(JsonObject report, String key) {
        JsonElement value = report.get(key);
        return value != null && value.isJsonObject() ? value.getAsJsonObject() : new JsonObject();
    }

    private static String validationDetails(String label, JsonObject validation) {
        if (!"FAILED".equals(string(validation, "status", ""))) {
            return "";
        }
        StringBuilder markdown = new StringBuilder("**").append(ShaftStatusPresentation.ERROR_ICON)
                .append(' ').append(label).append(" failed**");
        JsonElement diagnostics = validation.get("diagnostics");
        if (diagnostics != null && diagnostics.isJsonArray()) {
            for (JsonElement diagnostic : diagnostics.getAsJsonArray()) {
                if (diagnostic.isJsonPrimitive()) {
                    markdown.append("\n- ").append(diagnostic.getAsString());
                }
            }
        }
        return markdown.toString();
    }

    private static String captureReplayArtifacts(JsonObject object) {
        return metadataLine(
                "Full report", codePath(firstTextProperty(object, "reportPath")),
                "Review", codePath(firstTextProperty(object, "reviewPath")));
    }

    private static String codePath(String path) {
        return path == null || path.isBlank() ? "" : "`" + path + "`";
    }

    /**
     * Renders a {@code capture_backend_comparison} result (issue #3425 C2): one card per backend
     * with its generation outcome and source path, so a user can judge WebDriver vs Playwright
     * differentiation on their own recorded flow.
     */
    private static String backendComparisonMarkdown(JsonObject object) {
        if (!object.has("backends") || !object.get("backends").isJsonArray()) {
            return "";
        }
        List<String> sections = new ArrayList<>();
        sections.add("**Backend comparison** — the same recording generated for each SHAFT backend:");
        StringBuilder table = new StringBuilder("""
                | Backend | Generation | Generated source | Code blocks |
                | --- | --- | --- | --- |
                """);
        for (JsonElement item : object.getAsJsonArray("backends")) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject backend = item.getAsJsonObject();
            int blockCount = backend.has("blockIds") && backend.get("blockIds").isJsonArray()
                    ? backend.getAsJsonArray("blockIds").size()
                    : 0;
            table.append("| ").append(table(string(backend, "backend", "?")))
                    .append(" | ").append(booleanValue(backend, "successful")
                            ? ShaftStatusPresentation.SUCCESS_ICON + " succeeded"
                            : ShaftStatusPresentation.ERROR_ICON + " failed")
                    .append(" | ").append(table(codePath(string(backend, "sourcePath", ""))))
                    .append(" | ").append(blockCount)
                    .append(" |\n");
        }
        sections.add(table.toString().trim());
        appendNonBlank(sections, warnings(object));
        sections.add("_Both classes compile against SHAFT; open the generated sources side by side to compare "
                + "the WebDriver and Playwright styles for your flow._");
        return joinSections(sections);
    }

    /**
     * Renders a {@code capture_evidence_pack} manifest (issue #3425 B6) as a shareable checklist.
     */
    private static String evidencePackMarkdown(JsonObject object) {
        if (!object.has("artifactPaths") || !object.get("artifactPaths").isJsonArray()
                || !object.has("validationCommands")) {
            return "";
        }
        List<String> sections = new ArrayList<>();
        sections.add("**Evidence pack** — everything a reviewer needs, in one list:");
        appendNonBlank(sections, bulletList("Artifacts", object, "artifactPaths"));
        appendNonBlank(sections, bulletList("Validation commands", object, "validationCommands"));
        appendNonBlank(sections, warnings(object));
        sections.add("_Zip the artifact paths above (or attach them to your PR) to share the full recording "
                + "evidence with your team._");
        return joinSections(sections);
    }

    private static String codingPartnerMarkdown(JsonObject object) {
        if (!object.has("workingSetSummary") || !object.has("reuseMatches") || !object.has("missingCodeItems")) {
            return "";
        }
        List<String> sections = new ArrayList<>();
        sections.add(metadataLine(
                "Coding partner plan", string(object, "schemaVersion", ""),
                "Backend", string(object, "backend", "")));
        appendCodingPartnerSummary(sections, object);
        appendNonBlank(sections, codingPartnerStepsMarkdown(object));
        appendNonBlank(sections, codingPartnerReuseMarkdown(object));
        appendNonBlank(sections, bulletList("Missing code", object, "missingCodeItems"));
        appendNonBlank(sections, bulletList("Suggested MCP calls", object, "suggestedMcpCalls"));
        appendNonBlank(sections, codingPartnerNextActionsMarkdown(object));
        appendCodingPartnerVerification(sections, object);
        appendNonBlank(sections, bulletList("Evidence paths", object, "evidencePaths"));
        appendNonBlank(sections, warnings(object));
        return joinSections(sections);
    }

    private static void appendCodingPartnerSummary(List<String> sections, JsonObject object) {
        String summary = string(object, "workingSetSummary", "");
        if (!summary.isBlank()) {
            sections.add("**Working set:** " + summary);
        }
        String target = string(object, "recommendedTargetSourcePath", "");
        String anchor = string(object, "recommendedInsertionAnchor", "");
        if (!target.isBlank() || !anchor.isBlank()) {
            sections.add(metadataLine(
                    "Recommended target", target.isBlank() ? "" : "`" + target + "`",
                    "Insertion anchor", anchor.isBlank() ? "" : "`" + anchor + "`"));
        }
    }

    private static void appendCodingPartnerVerification(List<String> sections, JsonObject object) {
        String verification = string(object, "verificationCommand", "");
        if (!verification.isBlank()) {
            sections.add("**Verification:** `" + verification + "`");
        }
    }

    private static String codingPartnerStepsMarkdown(JsonObject object) {
        JsonElement value = object.get("stepPlan");
        if (value == null || !value.isJsonArray() || value.getAsJsonArray().isEmpty()) {
            return "";
        }
        StringBuilder markdown = new StringBuilder("**Plan steps**");
        for (JsonElement item : value.getAsJsonArray()) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject step = item.getAsJsonObject();
            String index = string(step, "index", "");
            String instruction = string(step, "instruction", "Step");
            markdown.append("\n").append(index.isBlank() ? "- " : index + ". ").append(instruction);
            String reuse = string(step, "reuseHint", "");
            if (!reuse.isBlank()) {
                markdown.append("\n  Reuse: ").append(reuse);
            }
            String proof = string(step, "proofTool", "");
            if (!proof.isBlank()) {
                markdown.append("\n  Proof: `").append(proof).append("`");
            }
        }
        return markdown.toString();
    }

    private static String codingPartnerReuseMarkdown(JsonObject object) {
        JsonElement value = object.get("reuseMatches");
        if (value == null || !value.isJsonArray() || value.getAsJsonArray().isEmpty()) {
            return "";
        }
        StringBuilder markdown = new StringBuilder("**Reuse matches**");
        for (JsonElement item : value.getAsJsonArray()) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject match = item.getAsJsonObject();
            String sourcePath = string(match, "sourcePath", "");
            String className = string(match, "className", "");
            markdown.append("\n- ");
            if (!sourcePath.isBlank()) {
                markdown.append("`").append(sourcePath).append("`");
            }
            if (!className.isBlank()) {
                markdown.append(sourcePath.isBlank() ? "" : " ").append("(").append(className).append(")");
            }
            String score = string(match, "score", "");
            if (!score.isBlank()) {
                markdown.append(" score ").append(score);
            }
            appendInlineList(markdown, "Anchors", match, "insertionAnchors");
            appendInlineList(markdown, "Locators", match, "locatorSummaries");
            appendInlineList(markdown, "Actions", match, "actionSummaries");
        }
        return markdown.toString();
    }

    private static String codingPartnerNextActionsMarkdown(JsonObject object) {
        JsonElement value = object.get("nextActions");
        if (value == null || !value.isJsonArray() || value.getAsJsonArray().isEmpty()) {
            return "";
        }
        StringBuilder markdown = new StringBuilder("**Next actions**");
        for (JsonElement item : value.getAsJsonArray()) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject action = item.getAsJsonObject();
            String label = string(action, "label", "Run next action");
            String tool = string(action, "toolName", "");
            markdown.append("\n- ").append(label);
            if (!tool.isBlank()) {
                markdown.append(" (`").append(tool).append("`)");
            }
            if (booleanValue(action, "requiresConfirmation")) {
                markdown.append(" - confirm context first");
            }
            appendInlineList(markdown, "Why", action, "rationale");
        }
        return markdown.toString();
    }

    private static void appendInlineList(StringBuilder markdown, String label, JsonObject object, String key) {
        JsonElement value = object.get(key);
        if (value == null || !value.isJsonArray() || value.getAsJsonArray().isEmpty()) {
            return;
        }
        List<String> items = new ArrayList<>();
        for (JsonElement item : value.getAsJsonArray()) {
            if (item.isJsonPrimitive()) {
                items.add("`" + item.getAsString() + "`");
            }
        }
        if (!items.isEmpty()) {
            markdown.append("\n  ").append(label).append(": ").append(String.join(", ", items));
        }
    }

    private static String scenariosMarkdown(JsonObject object) {
        JsonArray scenarios = object.getAsJsonArray("scenarios");
        if (scenarios.isEmpty()) {
            return "_No automation scenarios matched._";
        }
        List<String> sections = new ArrayList<>();
        sections.add(metadataLine(
                "Automation scenarios", "",
                "Intent", string(object, "intent", ""),
                "Area", string(object, "area", "")));
        StringBuilder list = new StringBuilder("**Automation scenarios**");
        for (JsonElement item : scenarios) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject scenario = item.getAsJsonObject();
            String title = string(scenario, "title", string(scenario, "id", "Scenario"));
            String id = string(scenario, "id", "");
            String summary = string(scenario, "summary", "");
            list.append("\n- **").append(title).append("**");
            if (!id.isBlank()) {
                list.append(" (`").append(id).append("`)");
            }
            if (!summary.isBlank()) {
                list.append(": ").append(summary);
            }
            String tools = scenarioToolsMarkdown(scenario);
            if (!tools.isBlank()) {
                list.append("\n  Tools: ").append(tools);
            }
        }
        sections.add(list.toString());
        return joinSections(sections);
    }

    private static String scenarioToolsMarkdown(JsonObject scenario) {
        JsonElement tools = scenario.get("tools");
        if (tools == null || !tools.isJsonArray() || tools.getAsJsonArray().isEmpty()) {
            return "";
        }
        List<String> names = new ArrayList<>();
        for (JsonElement tool : tools.getAsJsonArray()) {
            if (tool.isJsonPrimitive()) {
                names.add("`" + tool.getAsString() + "`");
            }
        }
        return String.join(", ", names);
    }

    private static String doctorMarkdown(JsonObject object) {
        if (!object.has("schemaVersion") || !object.has("primaryCause") || !object.has("codeBlocks")) {
            return "";
        }
        List<String> sections = new ArrayList<>();
        sections.add(metadataLine(
                "Doctor", string(object, "status", ""),
                "Primary cause", string(object, "primaryCause", ""),
                "Confidence", string(object, "confidence", ""),
                "Bundle", string(object, "bundleId", "")));
        String summary = string(object, "summary", "");
        if (!summary.isBlank()) {
            sections.add("**Diagnosis:** " + summary);
        }
        String reports = metadataLine(
                "JSON report", string(object, "jsonReportPath", ""),
                "Markdown report", string(object, "markdownReportPath", ""),
                "Evidence bundle", string(object, "bundlePath", ""));
        if (!reports.isBlank()) {
            sections.add(reports);
        }
        if (object.has("actions") && object.get("actions").isJsonArray()) {
            String actions = doctorActionsMarkdown(object.getAsJsonArray("actions"));
            if (!actions.isBlank()) {
                sections.add(actions);
            }
        }
        String blocks = codeBlocksMarkdown(object.getAsJsonArray("codeBlocks"));
        if (!blocks.isBlank()) {
            sections.add("**Fix snippets**\n\n" + blocks);
        }
        String warnings = warnings(object);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        if (object.has("providerFallback") && object.get("providerFallback").isJsonObject()) {
            JsonObject fallback = object.getAsJsonObject("providerFallback");
            String reason = string(fallback, "reason", "");
            if (!reason.isBlank()) {
                sections.add("**AI advisory:** " + (booleanValue(fallback, "used") ? "used" : "off") + " — " + reason);
            }
        }
        return joinSections(sections);
    }

    private static String doctorActionsMarkdown(JsonArray actions) {
        return titledActionsMarkdown("Recommended actions", actions);
    }

    private static String titledActionsMarkdown(String title, JsonArray actions) {
        if (actions.isEmpty()) {
            return "";
        }
        StringBuilder markdown = new StringBuilder("**").append(title).append("**");
        for (JsonElement item : actions) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject action = item.getAsJsonObject();
            String actionTitle = string(action, "title", "Action");
            String text = string(action, "action", "");
            String status = string(action, "status", "");
            markdown.append("\n- **").append(actionTitle).append("**");
            if (!status.isBlank()) {
                markdown.append(" (`").append(status).append("`)");
            }
            if (!text.isBlank()) {
                markdown.append(": ").append(text);
            }
        }
        return markdown.toString();
    }

    /**
     * Renders an {@code McpHealerRunResult} ({@code healer_run_failed_test},
     * {@code playwright_healer_run_failed_test}) as a readable card: the overall status, each
     * guarded rerun attempt, the nested Doctor {@code analysis} (delegated to {@link
     * #doctorMarkdown(JsonObject)} so a manual doctor run and a healer's embedded diagnosis read
     * identically — issue #3552), and the top-level review-only actions/code blocks. The top-level
     * {@code actions}/{@code codeBlocks} are a superset of {@code analysis}'s own (the healer merges
     * the doctor's remediation into its own list plus an agent-handoff action/snippet — see
     * {@code HealerService}), so they are labelled "Healer actions"/"Handoff snippets" rather than
     * reusing the Doctor card's "Recommended actions"/"Fix snippets" headers, to avoid two
     * identically-labelled sections that would read as a plain duplicate.
     */
    private static String healerMarkdown(JsonElement parsed) {
        if (!parsed.isJsonObject()) {
            return "";
        }
        JsonObject object = parsed.getAsJsonObject();
        if (!object.has("schemaVersion") || !object.has("attempts") || !object.has("analysis")) {
            return "";
        }
        List<String> sections = new ArrayList<>();
        String status = string(object, "status", "");
        sections.add("**" + healerStatusIcon(status) + " Healer:** " + status);
        if (object.get("attempts").isJsonArray()) {
            appendNonBlank(sections, healerAttemptsMarkdown(object.getAsJsonArray("attempts")));
        }
        JsonElement analysis = object.get("analysis");
        if (analysis != null && analysis.isJsonObject()) {
            appendNonBlank(sections, doctorMarkdown(analysis.getAsJsonObject()));
        }
        if (object.has("actions") && object.get("actions").isJsonArray()) {
            appendNonBlank(sections, titledActionsMarkdown("Healer actions", object.getAsJsonArray("actions")));
        }
        if (object.has("codeBlocks") && object.get("codeBlocks").isJsonArray()) {
            String blocks = codeBlocksMarkdown(object.getAsJsonArray("codeBlocks"));
            if (!blocks.isBlank()) {
                sections.add("**Handoff snippets**\n\n" + blocks);
            }
        }
        appendNonBlank(sections, warnings(object));
        return joinSections(sections);
    }

    private static String healerAttemptsMarkdown(JsonArray attempts) {
        if (attempts.isEmpty()) {
            return "";
        }
        StringBuilder markdown = new StringBuilder("**Attempts**");
        for (JsonElement item : attempts) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject attempt = item.getAsJsonObject();
            markdown.append("\n- Attempt ").append(string(attempt, "attemptNumber", "?")).append(": ")
                    .append(healerAttemptOutcome(attempt));
            String exitCode = string(attempt, "exitCode", "");
            if (!exitCode.isBlank()) {
                markdown.append(" (exit ").append(exitCode).append(")");
            }
            String diagnostics = string(attempt, "diagnostics", "");
            if (!diagnostics.isBlank()) {
                markdown.append(" — ").append(diagnostics);
            }
        }
        return markdown.toString();
    }

    private static String healerAttemptOutcome(JsonObject attempt) {
        if (booleanValue(attempt, "passed")) {
            return ShaftStatusPresentation.SUCCESS_ICON + " passed";
        }
        if (booleanValue(attempt, "timedOut")) {
            return ShaftStatusPresentation.WARNING_ICON + " timed out";
        }
        return ShaftStatusPresentation.ERROR_ICON + " failed";
    }

    private static String healerStatusIcon(String status) {
        return switch (status) {
            case "PASSED" -> ShaftStatusPresentation.SUCCESS_ICON;
            case "FAILED_WITH_SUGGESTIONS", "PRODUCT_BUG_SUSPECTED" -> ShaftStatusPresentation.WARNING_ICON;
            default -> ShaftStatusPresentation.ERROR_ICON;
        };
    }

    private static String mobileMarkdown(JsonObject object) {
        if (object.has("nodeAvailable") && object.has("appiumAvailable") && object.has("missingDependencies")) {
            return mobileToolchainMarkdown(object);
        }
        if (object.has("mode") && object.has("active") && object.has("deviceName")) {
            return mobileSessionMarkdown(object);
        }
        if (object.has("currentContext") && object.has("contexts") && object.has("pageSource")) {
            return mobileContextsMarkdown(object);
        }
        if (object.has("currentContext") && object.has("source") && object.has("characterCount")) {
            return mobileAccessibilityMarkdown(object);
        }
        if (object.has("confirmationToken") && object.has("readyToStart") && object.has("confirmationRequired")) {
            return mobileInspectorPlanMarkdown(object);
        }
        if (object.has("inspectorUrl") && object.has("appiumServerUrl") && object.has("actionCount")) {
            return mobileInspectorRecordingStatusMarkdown(object);
        }
        if (object.has("active") && object.has("outputPath") && object.has("actionCount")) {
            return mobileRecordingStatusMarkdown(object);
        }
        if (object.has("proxyPort") && object.has("caCertificatePem") && object.has("transactionCount")) {
            return mobileApiCaptureStatusMarkdown(object);
        }
        return "";
    }

    private static String mobileApiCaptureStatusMarkdown(JsonObject object) {
        List<String> sections = new ArrayList<>();
        boolean active = booleanValue(object, "active");
        String readiness = string(object, "readiness", "");
        // The atomic unit of an API capture is a transaction (request/response), not a GUI step.
        sections.add(metadataLine(
                "API capture", active ? "recording" : "stopped",
                "Transactions", string(object, "transactionCount", "0"),
                "Readiness", (readinessIcon(readiness) + " " + readiness).trim(),
                "Proxy port", string(object, "proxyPort", "")));
        String certificate = string(object, "caCertificatePem", "");
        if (!certificate.isBlank()) {
            sections.add("_Install the returned CA certificate as a trusted CA on the device before"
                    + " HTTPS traffic can be captured._");
        }
        String outputPath = string(object, "outputPath", "");
        if (!outputPath.isBlank()) {
            sections.add("**Session:** `" + outputPath + "`");
            boolean hasTransactions = !string(object, "transactionCount", "0").equals("0");
            if (!active && hasTransactions) {
                sections.add("Review code next — send:\n\n"
                        + fence("text", "Generate a SHAFT API test from " + outputPath));
            }
        }
        String warnings = warnings(object);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        return joinSections(sections);
    }

    private static String mobileRecordingStatusMarkdown(JsonObject object) {
        List<String> sections = new ArrayList<>();
        boolean active = booleanValue(object, "active");
        String readiness = string(object, "readiness", "");
        sections.add(metadataLine(
                "Recording", active ? "active" : "stopped",
                "Mode", string(object, "mode", ""),
                "Steps", string(object, "actionCount", ""),
                "Readiness", (readinessIcon(readiness) + " " + readiness).trim(),
                "Sensitive values", booleanValue(object, "includeSensitiveValues") ? "included" : "excluded"));
        String outputPath = string(object, "outputPath", "");
        if (!outputPath.isBlank()) {
            sections.add("**Output:** `" + outputPath + "`");
            boolean hasSteps = !string(object, "actionCount", "0").equals("0");
            if (!active && hasSteps) {
                sections.add("Review code next — send:\n\n"
                        + fence("text", "Generate a SHAFT test from " + outputPath));
            }
        }
        String warnings = warnings(object);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        return joinSections(sections);
    }

    private static String mobileInspectorPlanMarkdown(JsonObject object) {
        List<String> sections = new ArrayList<>();
        sections.add(metadataLine(
                "Inspector plan", booleanValue(object, "readyToStart") ? "ready" : "not ready",
                "Platform", string(object, "platformName", ""),
                "Confirmation", booleanValue(object, "confirmationRequired") ? "required" : "not required",
                "Sensitive values", booleanValue(object, "includeSensitiveValues") ? "included" : "excluded"));
        String token = string(object, "confirmationToken", "");
        if (!token.isBlank()) {
            sections.add("**Confirmation token:** `" + token + "`");
        }
        sections.add(metadataLine(
                "Device", string(object, "selectedDeviceId", ""),
                "AVD", string(object, "selectedAndroidAvdName", ""),
                "Provision emulator", booleanValue(object, "willProvisionAndroidEmulator") ? "yes" : "no",
                "Real device", booleanValue(object, "realDeviceAvailable") ? "available" : "not detected"));
        String outputPath = string(object, "outputPath", "");
        if (!outputPath.isBlank()) {
            sections.add("**Recording output:** `" + outputPath + "`");
        }
        String nextSteps = bulletList("Next steps", object, "nextSteps");
        if (!nextSteps.isBlank()) {
            sections.add(nextSteps);
        }
        if (object.has("codeBlocks") && object.get("codeBlocks").isJsonArray()) {
            String blocks = codeBlocksMarkdown(object.getAsJsonArray("codeBlocks"));
            if (!blocks.isBlank()) {
                sections.add(blocks);
            }
        }
        String warnings = warnings(object);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        return joinSections(sections);
    }

    private static String mobileInspectorRecordingStatusMarkdown(JsonObject object) {
        List<String> sections = new ArrayList<>();
        sections.add(metadataLine(
                "Inspector recording", booleanValue(object, "active") ? "active" : "stopped",
                "Paused", booleanValue(object, "paused") ? "yes" : "no",
                "Platform", string(object, "platformName", ""),
                "Actions", string(object, "actionCount", "")));
        sections.add(metadataLine(
                "Device", string(object, "deviceId", ""),
                "AVD", string(object, "androidAvdName", ""),
                "Managed emulator", booleanValue(object, "managedEmulator") ? "yes" : "no"));
        String outputPath = string(object, "outputPath", "");
        if (!outputPath.isBlank()) {
            sections.add("**Output:** `" + outputPath + "`");
        }
        String inspectorUrl = string(object, "inspectorUrl", "");
        if (!inspectorUrl.isBlank()) {
            sections.add("**Inspector URL:** " + inspectorUrl);
        }
        String appiumServerUrl = string(object, "appiumServerUrl", "");
        if (!appiumServerUrl.isBlank()) {
            sections.add("**Appium server:** " + appiumServerUrl);
        }
        if (object.has("codeBlocks") && object.get("codeBlocks").isJsonArray()) {
            String blocks = codeBlocksMarkdown(object.getAsJsonArray("codeBlocks"));
            if (!blocks.isBlank()) {
                sections.add(blocks);
            }
        }
        String warnings = warnings(object);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        return joinSections(sections);
    }

    private static String mobileToolchainMarkdown(JsonObject object) {
        List<String> sections = new ArrayList<>();
        sections.add(metadataLine(
                "Platform", string(object, "platformName", ""),
                "Node", availability(object, "nodeAvailable"),
                "npm", availability(object, "npmAvailable"),
                "Appium", availability(object, "appiumAvailable"),
                "Inspector", availability(object, "appiumInspectorAvailable"),
                "adb", availability(object, "adbAvailable"),
                "emulator", availability(object, "emulatorAvailable")));
        String versions = metadataLine(
                "Appium version", string(object, "appiumVersion", ""),
                "Inspector plugin", string(object, "appiumInspectorPluginVersion", ""));
        if (!versions.isBlank()) {
            sections.add(versions);
        }
        String missing = bulletList("Missing dependencies", object, "missingDependencies");
        if (!missing.isBlank()) {
            sections.add(missing);
        }
        String devices = object.has("androidDevices") && object.get("androidDevices").isJsonArray()
                ? mobileDevicesMarkdown(object.getAsJsonArray("androidDevices"))
                : "";
        if (!devices.isBlank()) {
            sections.add(devices);
        }
        String emulators = bulletList("Cached Android emulators", object, "cachedAndroidEmulators");
        if (!emulators.isBlank()) {
            sections.add(emulators);
        }
        String warnings = warnings(object);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        return joinSections(sections);
    }

    private static String mobileSessionMarkdown(JsonObject object) {
        List<String> sections = new ArrayList<>();
        sections.add(metadataLine(
                "Mode", string(object, "mode", ""),
                "Platform", string(object, "platformName", ""),
                "Device", string(object, "deviceName", ""),
                "Browser", string(object, "browserName", ""),
                "Active", booleanValue(object, "active") ? "yes" : "no"));
        if (object.has("deviceProfile") && object.get("deviceProfile").isJsonObject()) {
            String profile = mobileDeviceProfileMarkdown(object.getAsJsonObject("deviceProfile"));
            if (!profile.isBlank()) {
                sections.add(profile);
            }
        }
        if (object.has("codeBlocks") && object.get("codeBlocks").isJsonArray()) {
            String blocks = codeBlocksMarkdown(object.getAsJsonArray("codeBlocks"));
            if (!blocks.isBlank()) {
                sections.add(blocks);
            }
        }
        String warnings = warnings(object);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        return joinSections(sections);
    }

    private static String mobileContextsMarkdown(JsonObject object) {
        List<String> sections = new ArrayList<>();
        sections.add(metadataLine(
                "Current context", string(object, "currentContext", ""),
                "Source characters", string(object, "sourceCharacterCount", ""),
                "Truncated", booleanValue(object, "truncated") ? "yes" : "no"));
        String contexts = bulletList("Contexts", object, "contexts");
        if (!contexts.isBlank()) {
            sections.add(contexts);
        }
        String pageSource = string(object, "pageSource", "");
        if (!pageSource.isBlank()) {
            sections.add("**Source preview**\n\n" + fence("xml", clip(pageSource, 4_000)));
        }
        String warnings = warnings(object);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        return joinSections(sections);
    }

    private static String mobileAccessibilityMarkdown(JsonObject object) {
        List<String> sections = new ArrayList<>();
        sections.add(metadataLine(
                "Current context", string(object, "currentContext", ""),
                "Characters", string(object, "characterCount", ""),
                "Truncated", booleanValue(object, "truncated") ? "yes" : "no"));
        String source = string(object, "source", "");
        if (!source.isBlank()) {
            sections.add("**Accessibility tree**\n\n" + fence("xml", clip(source, 4_000)));
        }
        String warnings = warnings(object);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        return joinSections(sections);
    }

    private static String screenshotMarkdown(JsonObject object) {
        if (!object.has("mediaType") || !object.has("byteLength") || !object.has("outputPath")) {
            return "";
        }
        List<String> sections = new ArrayList<>();
        sections.add(metadataLine(
                "Screenshot", string(object, "mediaType", ""),
                "Bytes", string(object, "byteLength", ""),
                "Output", string(object, "outputPath", "")));
        String warnings = warnings(object);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        return joinSections(sections);
    }

    private static String captureStatusMarkdown(JsonObject object) {
        List<String> sections = new ArrayList<>();
        String state = string(object, "state", "");
        String readiness = string(object, "readiness", "");
        sections.add(metadataLine(
                "State", state,
                "Browser", string(object, "browser", ""),
                "Readiness", readinessIcon(readiness) + " " + readiness,
                "Events", string(object, "eventCount", ""),
                "Process", string(object, "processId", "")));
        if ("INCOMPLETE".equalsIgnoreCase(state)) {
            sections.add("_This recording was interrupted before it was stopped, but every action captured so"
                    + " far was already saved to disk — nothing was lost. Review it, generate code from it,"
                    + " or start a new recording._");
        }
        String outputPath = string(object, "outputPath", "");
        if (!outputPath.isBlank()) {
            sections.add("**Output:** `" + outputPath + "`");
            if ("COMPLETED".equalsIgnoreCase(state)) {
                sections.add("Generate code next — send:\n\n"
                        + fence("text", "Generate a SHAFT test from " + outputPath));
            }
        }
        String currentUrl = string(object, "currentUrl", "");
        if (!currentUrl.isBlank()) {
            sections.add("**Current URL:** " + currentUrl);
        }
        String warnings = warnings(object);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        return joinSections(sections);
    }

    private static String readinessIcon(String readiness) {
        return switch (readiness == null ? "" : readiness.toUpperCase(Locale.ROOT)) {
            case "READY" -> ShaftStatusPresentation.SUCCESS_ICON;
            case "RISKY" -> ShaftStatusPresentation.WARNING_ICON;
            case "BLOCKED" -> "⛔";
            default -> "";
        };
    }

    private static String toolsMarkdown(JsonArray tools) {
        if (tools.isEmpty()) {
            return "_No tools returned._";
        }
        StringBuilder markdown = new StringBuilder("""
                | Tool | Description |
                | --- | --- |
                """);
        for (JsonElement item : tools) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject tool = item.getAsJsonObject();
            markdown.append("| `")
                    .append(string(tool, "name", ""))
                    .append("` | ")
                    .append(table(string(tool, "description", "")))
                    .append(" |\n");
        }
        return markdown.toString().trim();
    }

    private static String codeBlocksMarkdown(JsonArray blocks) {
        List<String> sections = new ArrayList<>();
        for (JsonElement item : blocks) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject block = item.getAsJsonObject();
            String language = string(block, "language", "java");
            String code = firstTextProperty(block, "code", "content", "text");
            if (!code.isBlank()) {
                if (isRejectedGeneratedJava(language, code)) {
                    sections.add(nativeSeleniumRejectionMarkdown());
                    continue;
                }
                sections.add(fence(language.isBlank() ? "text" : language, code));
            }
        }
        return joinSections(sections);
    }

    static boolean containsRejectedGeneratedJava(String output) {
        String unwrapped = unwrapMcpText(output);
        JsonElement parsed = parse(unwrapped);
        if (parsed != null && parsed.isJsonObject()) {
            JsonObject object = parsed.getAsJsonObject();
            if (object.has("codeBlocks") && object.get("codeBlocks").isJsonArray()) {
                for (JsonElement item : object.getAsJsonArray("codeBlocks")) {
                    if (!item.isJsonObject()) {
                        continue;
                    }
                    JsonObject block = item.getAsJsonObject();
                    String language = string(block, "language", "java");
                    String code = firstTextProperty(block, "code", "content", "text");
                    if (isRejectedGeneratedJava(language, code)) {
                        return true;
                    }
                }
            }
        }
        return containsRejectedJavaFence(unwrapped)
                || (!containsCodeFence(unwrapped) && looksLikeJava(unwrapped) && isRejectedGeneratedJava("java", unwrapped));
    }

    static String nativeSeleniumRejectionMarkdown() {
        return """
                **Generated code rejected**

                The assistant returned Java that uses native Selenium APIs or a rejected SHAFT locator fallback. SHAFT IntelliJ Assistant only accepts generated Java that uses SHAFT syntax.

                Ask the agent to regenerate the answer with SHAFT-only Java:
                - Call `shaft_guide_search` with a query for SHAFT GUI WebDriver, page objects, locators, `driver.browser()`, and `driver.element()`.
                - For broad test or page-object design, call `test_automation_scenarios` to learn the matching SHAFT coding pattern.
                - Call `test_code_guardrails_check` on the final Java snippet before returning it.
                - Use `SHAFT.GUI.WebDriver`, `driver.browser()`, `driver.element()`, `driver.element().touch()`, and `SHAFT.GUI.Locator`.
                - Do not use `SHAFT.GUI.Locator.xpath(...)`; use Smart Locators, the SHAFT locator builder, or `By.xpath(...)` only as a last fallback.
                - Do not return native navigation calls, direct element lookup calls, WebElement actions, browser-driver constructors, or other raw Selenium code.
                """.strip();
    }

    private static String unwrapMcpText(String output) {
        String candidate = output == null ? "" : output.trim();
        for (int depth = 0; depth < 5; depth++) {
            JsonElement parsed = parse(candidate);
            if (parsed == null) {
                return candidate;
            }
            if (parsed.isJsonPrimitive() && parsed.getAsJsonPrimitive().isString()) {
                candidate = parsed.getAsString().trim();
                continue;
            }
            if (parsed.isJsonArray()) {
                String contentText = contentText(parsed.getAsJsonArray());
                if (contentText.isBlank()) {
                    return PRETTY.toJson(parsed);
                }
                if (parse(contentText) != null) {
                    candidate = contentText;
                    continue;
                }
                return contentText;
            }
            if (!parsed.isJsonObject()) {
                return PRETTY.toJson(parsed);
            }
            JsonObject object = parsed.getAsJsonObject();
            String contentText = contentText(object);
            if (!contentText.isBlank()) {
                if (parse(contentText) != null) {
                    candidate = contentText;
                    continue;
                }
                return contentText;
            }
            String nested = firstTextProperty(object, "markdown", "text", "message", "response", "output");
            if (!nested.isBlank() && !nested.equals(candidate)) {
                candidate = nested.trim();
                continue;
            }
            JsonElement result = object.get("result");
            if (result != null && !result.isJsonNull()) {
                candidate = result.isJsonPrimitive() && result.getAsJsonPrimitive().isString()
                        ? result.getAsString().trim()
                        : PRETTY.toJson(result);
                continue;
            }
            return PRETTY.toJson(object);
        }
        return candidate;
    }

    private static String contentText(JsonObject object) {
        JsonElement content = object.get("content");
        return content != null && content.isJsonArray() ? contentText(content.getAsJsonArray()) : "";
    }

    private static String contentText(JsonArray array) {
        List<String> parts = new ArrayList<>();
        for (JsonElement item : array) {
            if (item == null || !item.isJsonObject()) {
                continue;
            }
            JsonObject object = item.getAsJsonObject();
            JsonElement text = object.get("text");
            if (text != null && text.isJsonPrimitive() && text.getAsJsonPrimitive().isString()) {
                parts.add(text.getAsString().trim());
            }
        }
        return String.join("\n\n", parts).trim();
    }

    private static String firstTextProperty(JsonObject object, String... names) {
        for (String name : names) {
            JsonElement value = object.get(name);
            if (value != null && value.isJsonPrimitive() && value.getAsJsonPrimitive().isString()) {
                return value.getAsString();
            }
        }
        return "";
    }

    private static String availability(JsonObject object, String key) {
        return booleanValue(object, key) ? "available" : "missing";
    }

    private static String bulletList(String title, JsonObject object, String key) {
        JsonElement value = object.get(key);
        if (value == null || !value.isJsonArray() || value.getAsJsonArray().isEmpty()) {
            return "";
        }
        StringBuilder markdown = new StringBuilder("**").append(title).append("**");
        for (JsonElement item : value.getAsJsonArray()) {
            if (item.isJsonPrimitive()) {
                markdown.append("\n- ").append(item.getAsString());
            }
        }
        return markdown.toString();
    }

    private static String mobileDevicesMarkdown(JsonArray devices) {
        if (devices.isEmpty()) {
            return "";
        }
        StringBuilder markdown = new StringBuilder("""
                | Device | State | Type |
                | --- | --- | --- |
                """);
        for (JsonElement item : devices) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject device = item.getAsJsonObject();
            markdown.append("| `")
                    .append(table(string(device, "id", "")))
                    .append("` | ")
                    .append(table(string(device, "state", "")))
                    .append(" | ")
                    .append(table(string(device, "type", "")))
                    .append(" |\n");
        }
        return markdown.toString().trim();
    }

    private static String mobileDeviceProfileMarkdown(JsonObject profile) {
        if (profile.isEmpty()) {
            return "";
        }
        StringBuilder markdown = new StringBuilder("**Device profile**");
        for (String key : profile.keySet().stream().sorted().toList()) {
            markdown.append("\n- ").append(key).append(": `").append(string(profile, key, "")).append("`");
        }
        return markdown.toString();
    }

    private static String warnings(JsonObject object) {
        JsonElement warnings = object.get("warnings");
        if (warnings == null || !warnings.isJsonArray() || warnings.getAsJsonArray().isEmpty()) {
            return "";
        }
        StringBuilder markdown = new StringBuilder("**").append(ShaftStatusPresentation.WARNING_ICON).append(" Warnings**");
        for (JsonElement warning : warnings.getAsJsonArray()) {
            if (warning.isJsonPrimitive()) {
                markdown.append("\n- ").append(warning.getAsString());
            }
        }
        return markdown.toString();
    }

    private static String metadataLine(String... pairs) {
        List<String> parts = new ArrayList<>();
        for (int index = 0; index + 1 < pairs.length; index += 2) {
            String value = pairs[index + 1];
            if (value != null && !value.isBlank()) {
                parts.add("**" + pairs[index] + ":** " + value);
            }
        }
        return String.join(" · ", parts);
    }

    private static String joinSections(List<String> sections) {
        return String.join("\n\n", sections.stream()
                .filter(section -> section != null && !section.isBlank())
                .toList());
    }

    private static void appendNonBlank(List<String> sections, String section) {
        if (section != null && !section.isBlank()) {
            sections.add(section);
        }
    }

    private static String string(JsonObject object, String key, String fallback) {
        JsonElement value = object.get(key);
        if (value == null || value.isJsonNull()) {
            return fallback;
        }
        if (value.isJsonPrimitive()) {
            return value.getAsString();
        }
        return PRETTY.toJson(value);
    }

    private static boolean booleanValue(JsonObject object, String key) {
        JsonElement value = object.get(key);
        return value != null
                && value.isJsonPrimitive()
                && value.getAsJsonPrimitive().isBoolean()
                && value.getAsBoolean();
    }

    private static String table(String value) {
        return value.replace("|", "\\|").replace("\n", "<br>");
    }

    private static JsonElement parse(String text) {
        if (text == null || text.isBlank()) {
            return null;
        }
        try {
            return JsonParser.parseString(text);
        } catch (RuntimeException exception) {
            return null;
        }
    }

    private static String prettyJson(String text) {
        JsonElement parsed = parse(text);
        return parsed == null ? "" : PRETTY.toJson(parsed);
    }

    private static boolean containsCodeFence(String text) {
        return text.contains("```");
    }

    /**
     * Returns whether {@code markdown} contains a fenced code block. Exposed so callers can tell a
     * genuine code-generation result apart from a terse, non-code fallback answer (for example a
     * bare {@code "Done"} value with no {@code codeBlocks}).
     */
    static boolean hasCodeFence(String markdown) {
        return markdown != null && containsCodeFence(markdown);
    }

    private static boolean looksLikeJava(String text) {
        String lower = text.toLowerCase(Locale.ROOT);
        return lower.contains("public class ")
                || lower.contains("class ")
                && (lower.contains("void ") || lower.contains("extends ") || lower.contains("implements "))
                || lower.startsWith("package ")
                || lower.startsWith("import ")
                || lower.contains("@test")
                || lower.contains("webDriver.element()");
    }

    private static boolean isJava(String language) {
        return language == null || language.isBlank() || "java".equalsIgnoreCase(language.trim());
    }

    private static boolean isRejectedGeneratedJava(String language, String code) {
        String snippet = code == null ? "" : code;
        return isJava(language) && !snippet.isBlank() && looksLikeNativeSelenium(snippet);
    }

    private static boolean containsRejectedJavaFence(String markdown) {
        if (markdown == null || markdown.isBlank()) {
            return false;
        }
        int offset = 0;
        while (offset < markdown.length()) {
            int fenceStart = markdown.indexOf("```", offset);
            if (fenceStart < 0) {
                return false;
            }
            int languageStart = fenceStart + 3;
            int firstLineEnd = markdown.indexOf('\n', languageStart);
            if (firstLineEnd < 0) {
                return false;
            }
            String language = markdown.substring(languageStart, firstLineEnd).trim();
            int fenceEnd = markdown.indexOf("```", firstLineEnd + 1);
            if (fenceEnd < 0) {
                return false;
            }
            String code = markdown.substring(firstLineEnd + 1, fenceEnd);
            if (isRejectedGeneratedJava(language, code)) {
                return true;
            }
            offset = fenceEnd + 3;
        }
        return false;
    }

    /**
     * Whether pasted/typed code reads as native Selenium (package-private so the composer can
     * proactively offer "convert to SHAFT + guardrails" on paste — issue #3425 B7).
     */
    static boolean looksLikeNativeSelenium(String code) {
        return code.contains("org.openqa.selenium.WebDriver")
                || code.contains("SHAFT.GUI.Locator.xpath(")
                || code.contains("new ChromeDriver(")
                || code.contains("new FirefoxDriver(")
                || code.contains("new EdgeDriver(")
                || code.contains("driver.get(")
                || code.contains("driver.findElement(")
                || code.contains("driver.findElements(")
                || code.contains("WebElement ");
    }

    private static String fence(String language, String text) {
        return "```" + language + "\n" + text.stripTrailing() + "\n```";
    }

    private static String clip(String text, int maxCharacters) {
        if (text == null || text.length() <= maxCharacters) {
            return text == null ? "" : text;
        }
        return text.substring(0, maxCharacters) + "\n... truncated ...";
    }
}
