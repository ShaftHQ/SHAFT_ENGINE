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

/**
 * Converts Assistant and MCP payloads into Markdown suitable for display.
 */
final class AssistantMarkdown {
    private static final Gson PRETTY = new GsonBuilder().setPrettyPrinting().create();
    private static final Set<String> KNOWN_TOOLS = Set.of(
            "autobot_local_agent_clients",
            "autobot_local_agent_run",
            "autobot_provider_chat");

    private AssistantMarkdown() {
        throw new IllegalStateException("Utility class");
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
        if (!answer.isBlank()) {
            sections.add(normalizeMarkdown(answer));
        }
        sections.add(metadataLine(
                "Status", string(response, "status", ""),
                "Provider", string(response, "provider", ""),
                "Model", string(response, "model", ""),
                "Mode", string(response, "mode", "")));
        String warnings = warnings(response);
        if (!warnings.isBlank()) {
            sections.add(warnings);
        }
        String fallback = string(response, "fallbackReason", "");
        if (!fallback.isBlank()) {
            sections.add("**Fallback reason:** " + fallback);
        }
        if (answer.isBlank() && warnings.isBlank() && fallback.isBlank()) {
            sections.add("_No answer returned._");
        }
        return joinSections(sections);
    }

    private static String genericMarkdown(JsonElement parsed) {
        if (parsed.isJsonObject()) {
            JsonObject object = parsed.getAsJsonObject();
            if (object.has("state") && object.has("outputPath") && object.has("processId")) {
                return captureStatusMarkdown(object);
            }
            if (object.has("tools") && object.get("tools").isJsonArray()) {
                return toolsMarkdown(object.getAsJsonArray("tools"));
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

    private static String captureStatusMarkdown(JsonObject object) {
        List<String> sections = new ArrayList<>();
        sections.add(metadataLine(
                "State", string(object, "state", ""),
                "Browser", string(object, "browser", ""),
                "Readiness", string(object, "readiness", ""),
                "Events", string(object, "eventCount", ""),
                "Process", string(object, "processId", "")));
        String outputPath = string(object, "outputPath", "");
        if (!outputPath.isBlank()) {
            sections.add("**Output:** `" + outputPath + "`");
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
                sections.add(fence(language.isBlank() ? "text" : language, code));
            }
        }
        return joinSections(sections);
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

    private static String warnings(JsonObject object) {
        JsonElement warnings = object.get("warnings");
        if (warnings == null || !warnings.isJsonArray() || warnings.getAsJsonArray().isEmpty()) {
            return "";
        }
        StringBuilder markdown = new StringBuilder("**Warnings**");
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
