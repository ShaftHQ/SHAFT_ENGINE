package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Builds MCP tool catalogs from discovered tools/list style payloads.
 */
final class ToolCatalog {
    private static final String MCP_CATEGORY_LABEL = "MCP";

    private ToolCatalog() {
        throw new IllegalStateException("Utility class");
    }

    static List<DiscoveredTool> parseToolsList(String toolsListOutput) {
        if (toolsListOutput == null || toolsListOutput.isBlank()) {
            return List.of();
        }

        JsonElement root;
        try {
            root = JsonParser.parseString(toolsListOutput);
        } catch (RuntimeException ignored) {
            return List.of();
        }

        JsonElement tools = extractToolsNode(root);
        if (tools == null || !tools.isJsonArray()) {
            return List.of();
        }

        Map<String, DiscoveredTool> discoveredByName = new LinkedHashMap<>();
        for (JsonElement node : tools.getAsJsonArray()) {
            if (!node.isJsonObject()) {
                continue;
            }

            JsonObject toolJson = node.getAsJsonObject();
            String name = string(toolJson, "name");
            if (name.isBlank()) {
                continue;
            }

            discoveredByName.putIfAbsent(name, new DiscoveredTool(name, string(toolJson, "description")));
        }

        return List.copyOf(discoveredByName.values());
    }

    static List<ToolCategory> mergeDiscoveredTools(List<ToolCategory> curatedCategories,
                                                  List<DiscoveredTool> discoveredTools) {
        if (curatedCategories == null || curatedCategories.isEmpty()) {
            return List.of();
        }
        if (discoveredTools == null || discoveredTools.isEmpty()) {
            return List.copyOf(curatedCategories);
        }

        Map<String, DiscoveredTool> discoveredByName = discoveredTools.stream()
                .filter(tool -> tool != null && !tool.name().isBlank())
                .collect(Collectors.toMap(
                        DiscoveredTool::name,
                        tool -> tool,
                        (first, second) -> first,
                        LinkedHashMap::new));

        Set<String> matchedNames = new HashSet<>();
        List<ToolCategory> mergedCategories = new ArrayList<>(curatedCategories.size());
        for (ToolCategory category : curatedCategories) {
            mergedCategories.add(mergeCategory(category, discoveredByName, matchedNames));
        }

        return mergedCategories;
    }

    private static ToolCategory mergeCategory(
            ToolCategory category,
            Map<String, DiscoveredTool> discoveredByName,
            Set<String> matchedNames) {
        List<ToolTemplate> templates = new ArrayList<>(category.templates().size());

        for (ToolTemplate template : category.templates()) {
            DiscoveredTool discovered = discoveredByName.get(template.toolName());
            if (discovered != null) {
                matchedNames.add(template.toolName());
            }
            templates.add(mergedTemplate(template, discovered));
        }

        if (MCP_CATEGORY_LABEL.equals(category.label())) {
            for (DiscoveredTool discovered : discoveredByName.values()) {
                if (!matchedNames.contains(discovered.name())) {
                    templates.add(fallbackTemplate(discovered));
                }
            }
        }

        return new ToolCategory(category.label(), templates);
    }

    private static ToolTemplate mergedTemplate(ToolTemplate template, DiscoveredTool discovered) {
        if (discovered == null || discovered.description().isBlank()) {
            return template;
        }
        return new ToolTemplate(
                template.label(),
                template.toolName(),
                template.arguments(),
                discovered.description(),
                template.confirmationRequired());
    }

    private static ToolTemplate fallbackTemplate(DiscoveredTool tool) {
        return new ToolTemplate(tool.name(), tool.name(), "{}", tool.description(), false);
    }

    private static JsonElement extractToolsNode(JsonElement root) {
        if (root == null || !root.isJsonObject()) {
            return root != null && root.isJsonArray() ? root : null;
        }

        JsonObject rootObject = root.getAsJsonObject();

        if (rootObject.has("tools") && rootObject.get("tools").isJsonArray()) {
            return rootObject.get("tools");
        }

        if (rootObject.has("result")
                && rootObject.get("result").isJsonObject()
                && rootObject.getAsJsonObject("result").has("tools")
                && rootObject.getAsJsonObject("result").get("tools").isJsonArray()) {
            return rootObject.getAsJsonObject("result").get("tools");
        }

        return null;
    }

    private static String string(JsonObject obj, String key) {
        if (!obj.has(key) || !obj.get(key).isJsonPrimitive()) {
            return "";
        }

        if (!obj.get(key).getAsJsonPrimitive().isString()) {
            return "";
        }

        return obj.get(key).getAsString();
    }
}
