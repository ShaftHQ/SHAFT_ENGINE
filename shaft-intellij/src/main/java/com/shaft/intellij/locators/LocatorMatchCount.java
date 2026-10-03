package com.shaft.intellij.locators;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.shaft.intellij.mcp.ShaftMcpToolResult;

import java.util.Map;

/**
 * Pure contract for validating a {@code By.*} locator against the live SHAFT session through the
 * {@code element_count} MCP tool (issue #6421).
 */
public final class LocatorMatchCount {
    public static final String TOOL_NAME = "element_count";
    public static final String START_SESSION = "No live SHAFT session. Start a session from the SHAFT "
            + "tool window (Capture or Locator Playground), open the page, then check the locator again.";
    private static final Map<String, String> STRATEGIES = Map.of(
            "id", "ID", "cssSelector", "CSSSELECTOR", "xpath", "XPATH", "name", "NAME",
            "tagName", "TAGNAME", "className", "CLASSNAME");

    private LocatorMatchCount() {
    }

    /** Whether {@code By.<factory>} can be checked. */
    public static boolean supports(String factory) {
        return STRATEGIES.containsKey(factory);
    }

    /** Tool arguments for a {@code Locator.hasTagName(..)...build()} chain given as {@code [name, args...]} steps. */
    public static JsonObject chainArguments(java.util.List<java.util.List<String>> steps) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("locatorStrategy", "SHAFT_LOCATOR");
        arguments.addProperty("locatorValue", new com.google.gson.Gson().toJson(steps));
        return arguments;
    }

    /** Tool arguments for {@code By.<factory>(value)}, or null when the factory is unsupported. */
    public static JsonObject arguments(String factory, String value) {
        String strategy = STRATEGIES.get(factory);
        if (strategy == null) {
            return null;
        }
        JsonObject arguments = new JsonObject();
        arguments.addProperty("locatorStrategy", strategy);
        arguments.addProperty("locatorValue", value);
        return arguments;
    }

    /** {@code N matches}, or the guided start-a-session message when there is no live session. */
    public static String message(ShaftMcpToolResult result) {
        Integer count = result == null || !result.success() ? null : count(result.output());
        if (count == null) {
            String detail = result == null || result.output() == null ? "" : result.output().strip();
            return detail.isEmpty() ? START_SESSION : START_SESSION + " (" + detail + ")";
        }
        return count + (count == 1 ? " match" : " matches");
    }

    private static Integer count(String output) {
        JsonObject payload = parse(output);
        if (payload == null) {
            return null;
        }
        if (payload.has("count") && payload.get("count").isJsonPrimitive()) {
            return payload.get("count").getAsInt();
        }
        if (payload.has("content") && payload.get("content").isJsonArray()) {
            for (JsonElement entry : payload.getAsJsonArray("content")) {
                if (entry.isJsonObject() && entry.getAsJsonObject().has("text")) {
                    Integer nested = count(entry.getAsJsonObject().get("text").getAsString());
                    if (nested != null) {
                        return nested;
                    }
                }
            }
        }
        return null;
    }

    private static JsonObject parse(String text) {
        try {
            JsonElement parsed = text == null ? null : JsonParser.parseString(text);
            return parsed != null && parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
        } catch (RuntimeException malformed) {
            return null;
        }
    }
}
