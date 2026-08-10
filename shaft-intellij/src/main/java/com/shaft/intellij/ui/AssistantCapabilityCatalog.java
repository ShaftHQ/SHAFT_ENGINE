package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.shaft.intellij.mcp.ShaftCliCommandIndex;
import com.shaft.intellij.mcp.ToolCatalogIndex;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Predicate;

/** Formats the complete Assistant, SHAFT MCP, and SHAFT CLI capability surface. */
final class AssistantCapabilityCatalog {

    private AssistantCapabilityCatalog() {
    }

    static String bundled(String topic) {
        return render(topic, bundledTools(), false,
                "Live MCP discovery is unavailable; showing the bundled catalog.");
    }

    static String live(String topic, String toolsListOutput) {
        Map<String, ToolCatalogIndex.ToolMetadata> bundledByName = new LinkedHashMap<>();
        for (ToolCatalogIndex.ToolMetadata tool : ToolCatalogIndex.tools()) {
            bundledByName.put(tool.name(), tool);
        }
        List<DiscoveredTool> discoveredTools = ToolCatalog.parseToolsList(toolsListOutput);
        List<ToolEntry> tools = new ArrayList<>();
        for (DiscoveredTool discovered : discoveredTools) {
            ToolCatalogIndex.ToolMetadata bundled = bundledByName.get(discovered.name());
            tools.add(new ToolEntry(
                    discovered.name(),
                    bundled == null ? "Other discovered tools" : bundled.service(),
                    discovered.description().isBlank() && bundled != null
                            ? bundled.description()
                            : discovered.description(),
                    bundled == null ? "" : bundled.slashAlias()));
        }
        if (!validToolsEnvelope(toolsListOutput, !discoveredTools.isEmpty())) {
            return render(topic, bundledTools(), false,
                    "Live discovery returned no usable MCP tools; showing the bundled catalog.");
        }
        return render(topic, tools, true, "Live SHAFT MCP discovery succeeded.");
    }

    private static boolean validToolsEnvelope(String payload, boolean hasUsableTools) {
        try {
            JsonElement root = JsonParser.parseString(payload == null ? "" : payload);
            JsonElement tools = root.isJsonObject() ? root.getAsJsonObject().get("tools") : null;
            return tools != null && tools.isJsonArray()
                    && (tools.getAsJsonArray().isEmpty() || hasUsableTools);
        } catch (RuntimeException invalidJson) {
            return false;
        }
    }

    private static List<ToolEntry> bundledTools() {
        return ToolCatalogIndex.tools().stream()
                .map(tool -> new ToolEntry(tool.name(), tool.service(), tool.description(), tool.slashAlias()))
                .toList();
    }

    private static String render(String topic, List<ToolEntry> tools, boolean live, String sourceNote) {
        String normalizedTopic = normalize(topic);
        List<String> topicTokens = List.of(normalizedTopic.split(" ")).stream()
                .filter(token -> !token.isBlank())
                .toList();
        Predicate<String> matches = value -> {
            String normalizedValue = normalize(value);
            return topicTokens.isEmpty() || topicTokens.stream().allMatch(normalizedValue::contains);
        };
        StringBuilder output = new StringBuilder("# SHAFT Assistant capabilities\n\n")
                .append(sourceNote);
        int matchesBefore = output.length();

        List<AssistantCommand.CommandHint> commands = AssistantCommand.registeredCommandHints().stream()
                .filter(command -> matches.test(command.canonical() + " " + command.summary() + " "
                        + String.join(" ", command.synonyms())))
                .sorted(Comparator.comparing(AssistantCommand.CommandHint::canonical))
                .toList();
        if (!commands.isEmpty()) {
            output.append("\n\n## SHAFT Assistant commands");
            for (AssistantCommand.CommandHint command : commands) {
                output.append("\n- **").append(command.canonical()).append("** — ")
                        .append(command.summary());
                if (!command.synonyms().isEmpty()) {
                    output.append(" (aliases: ").append(String.join(", ", command.synonyms())).append(')');
                }
                output.append("\n  Example: `").append(command.example()).append('`');
            }
        }

        List<ToolEntry> matchingTools = tools.stream()
                .filter(tool -> matches.test(tool.name() + " " + tool.service() + " "
                        + tool.description() + " " + tool.slashAlias()))
                .sorted(Comparator.comparing(ToolEntry::service).thenComparing(ToolEntry::name))
                .toList();
        if (!matchingTools.isEmpty()) {
            output.append("\n\n## SHAFT MCP tools")
                    .append(live ? " (available now)" : " (bundled fallback)");
            String service = null;
            for (ToolEntry tool : matchingTools) {
                String nextService = friendlyService(tool.service());
                if (!nextService.equals(service)) {
                    service = nextService;
                    output.append("\n\n### ").append(service);
                }
                output.append("\n- `").append(tool.name()).append("` — ")
                        .append(tool.description().isBlank() ? "SHAFT MCP tool" : tool.description());
                if (!tool.slashAlias().isBlank()) {
                    output.append(" (alias: `/mcp ").append(tool.slashAlias()).append("`)");
                }
            }
        }

        List<ShaftCliCommandIndex.CommandMetadata> cliCommands = ShaftCliCommandIndex.commands().stream()
                .filter(command -> matches.test(command.name() + " " + command.description()))
                .toList();
        if (!cliCommands.isEmpty()) {
            output.append("\n\n## SHAFT CLI commands");
            for (ShaftCliCommandIndex.CommandMetadata command : cliCommands) {
                output.append("\n- `shaft-cli ").append(command.name()).append("` — ")
                        .append(command.description());
            }
            if (normalizedTopic.isBlank() || "call".contains(normalizedTopic)
                    || normalizedTopic.contains("tool")) {
                output.append("\n\nUse `shaft-cli call <tool> [json]` to invoke any SHAFT MCP tool by name.");
            }
        }

        if (output.length() == matchesBefore) {
            output.append("\n\nNo capability matched `").append(topic == null ? "" : topic.trim())
                    .append("`. Use `/help` to show the complete catalog.");
        } else if (!normalizedTopic.isBlank()) {
            output.append("\n\n_Filtered by: `").append(topic.trim()).append("`. Use `/help` for everything._");
        }
        return output.toString();
    }

    private static String friendlyService(String service) {
        String value = service == null || service.isBlank() ? "Other discovered tools" : service;
        return value.replace("Service", "")
                .replaceAll("([a-z])([A-Z])", "$1 $2")
                .trim();
    }

    private static String normalize(String value) {
        return value == null ? "" : value.trim().toLowerCase(Locale.ROOT)
                .replace('_', ' ')
                .replace('-', ' ')
                .replaceAll("\\s+", " ");
    }

    private record ToolEntry(String name, String service, String description, String slashAlias) {
    }
}
