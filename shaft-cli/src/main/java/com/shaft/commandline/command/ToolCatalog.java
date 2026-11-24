package com.shaft.commandline.command;

import com.shaft.commandline.mcp.McpClient;
import com.shaft.commandline.mcp.Tool;
import com.shaft.commandline.util.Json;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

/**
 * Offline tool-index rendering for {@code shaft-cli tools}. Group matching treats a token that
 * contains {@code _} as a tool-name prefix ({@code report_}). Short names such as {@code report}
 * select a service instead: {@code report} is {@code TestAutomationService}, while the
 * {@code report_*} tools live on {@code TraceService}.
 */
final class ToolCatalog {

    /**
     * Short {@code --group} names. Exact service names such as {@code CaptureService} match on their own.
     */
    private static final Map<String, String> SERVICE_ALIASES = Map.ofEntries(
            Map.entry("autobot", "AutobotService"),
            Map.entry("browser", "BrowserService"),
            Map.entry("capture", "CaptureService"),
            Map.entry("coding-partner", "CodingPartnerService"),
            Map.entry("design", "DesignService"),
            Map.entry("doctor", "DoctorService"),
            Map.entry("element", "ElementService"),
            Map.entry("engine", "EngineService"),
            Map.entry("guide", "GuideService"),
            Map.entry("heal", "HealerService"),
            Map.entry("healer", "HealerService"),
            Map.entry("infrastructure", "InfrastructureMcpService"),
            Map.entry("mobile", "MobileService"),
            Map.entry("planner", "PlannerService"),
            Map.entry("project", "ShaftProjectService"),
            Map.entry("report", "TestAutomationService"),
            Map.entry("setup", "InfrastructureMcpService"),
            Map.entry("test-automation", "TestAutomationService"),
            Map.entry("trace", "TraceService"));

    private ToolCatalog() {
        throw new IllegalStateException("Utility class");
    }

    static Optional<JsonNode> readCached(PrintWriter err) {
        try (InputStream stream = ToolsCommand.class.getResourceAsStream(ToolsCommand.CACHED_INDEX_RESOURCE)) {
            if (stream == null) {
                err.println("Cached tool index not found on the classpath: " + ToolsCommand.CACHED_INDEX_RESOURCE);
                err.flush();
                return Optional.empty();
            }
            return Optional.of(Json.MAPPER.readTree(stream));
        } catch (IOException exception) {
            err.println("Cached tool index could not be read: " + exception.getMessage());
            err.flush();
            return Optional.empty();
        } catch (RuntimeException exception) {
            err.println("Cached tool index is not valid JSON: " + exception.getMessage());
            err.flush();
            return Optional.empty();
        }
    }

    static int renderCached(JsonNode index, PrintWriter out, PrintWriter err,
                            boolean json, boolean compact, String group) {
        if (json && group.isBlank()) {
            out.println(pretty(index));
            out.flush();
            return 0;
        }
        if (!json && !compact && group.isBlank()) {
            writeLegacyText(index, out);
            return 0;
        }
        return writeFiltered(index, out, err, json, compact, group);
    }

    static int renderLive(McpClient client, PrintWriter out, PrintWriter err,
                          boolean json, boolean compact, String group) {
        Map<String, String> services = servicesByName();
        if (json) {
            return writeLiveJson(client.listToolsRaw(), services, out, err, group);
        }
        return writeLiveText(client.listTools(), services, out, err, compact, group);
    }

    static int describe(PrintWriter out, PrintWriter err, String toolName, boolean json) {
        Optional<JsonNode> loaded = readCached(err);
        if (loaded.isEmpty()) {
            return 1;
        }
        JsonNode index = loaded.get();
        Optional<JsonNode> tool = find(index, toolName);
        if (tool.isEmpty()) {
            printUnknownTool(err, index, toolName);
            return 2;
        }
        if (json) {
            out.println(pretty(tool.get()));
            out.flush();
            return 0;
        }
        writeDescribed(out, tool.get());
        out.flush();
        return 0;
    }

    private static void writeLegacyText(JsonNode index, PrintWriter out) {
        for (JsonNode tool : index.path("tools")) {
            out.println(tool.path("name").asText("") + " — " + ToolsCommand.firstSentence(tool.path("description").asText("")));
        }
        out.flush();
    }

    private static int writeFiltered(JsonNode index, PrintWriter out, PrintWriter err,
                                     boolean json, boolean compact, String group) {
        List<JsonNode> matched = select(index, group);
        if (!group.isBlank() && matched.isEmpty()) {
            printUnknownGroup(err, group, index);
            return 2;
        }
        if (json) {
            out.println(pretty(filteredIndex(index, matched)));
            out.flush();
            return 0;
        }
        for (JsonNode tool : matched) {
            out.println(compact ? compactLine(tool) : textLine(tool));
        }
        out.flush();
        return 0;
    }

    private static int writeLiveText(List<Tool> tools, Map<String, String> services, PrintWriter out, PrintWriter err,
                                     boolean compact, String group) {
        List<JsonNode> matched = new ArrayList<>();
        for (Tool tool : tools) {
            String service = serviceOf(tool.name(), "", services);
            if (group.isBlank() || matches(tool.name(), service, group)) {
                matched.add(liveNode(tool, service));
            }
        }
        if (!group.isBlank() && matched.isEmpty()) {
            printUnknownGroup(err, group, indexForGroups());
            return 2;
        }
        for (JsonNode tool : matched) {
            out.println(compact ? compactLine(tool) : textLine(tool));
        }
        out.flush();
        return 0;
    }

    private static int writeLiveJson(JsonNode raw, Map<String, String> services, PrintWriter out, PrintWriter err,
                                     String group) {
        if (!(raw instanceof ObjectNode object)) {
            err.println("Live tools/list result is not a JSON object.");
            err.flush();
            return 1;
        }
        ArrayNode kept = Json.newObject().putArray("tools");
        JsonNode tools = object.path("tools");
        if (tools.isArray()) {
            collectLiveMatches(tools, services, group, kept);
        }
        if (!group.isBlank() && kept.isEmpty()) {
            printUnknownGroup(err, group, indexForGroups());
            return 2;
        }
        ObjectNode copy = object.deepCopy();
        copy.set("tools", kept);
        out.println(pretty(copy));
        out.flush();
        return 0;
    }

    private static void collectLiveMatches(JsonNode tools, Map<String, String> services, String group, ArrayNode kept) {
        for (JsonNode tool : tools) {
            String name = tool.path("name").asText("");
            String service = serviceOf(name, tool.path("service").asText(""), services);
            if (matches(name, service, group)) {
                kept.add(tool);
            }
        }
    }

    private static List<JsonNode> select(JsonNode index, String group) {
        List<JsonNode> matched = new ArrayList<>();
        JsonNode tools = index.path("tools");
        if (!tools.isArray()) {
            return matched;
        }
        for (JsonNode tool : tools) {
            if (group.isBlank() || matches(tool.path("name").asText(""), tool.path("service").asText(""), group)) {
                matched.add(tool);
            }
        }
        return matched;
    }

    static boolean matches(String name, String service, String group) {
        String key = group.trim().toLowerCase(Locale.ROOT);
        if (key.isEmpty()) {
            return true;
        }
        if (key.indexOf('_') >= 0) {
            return name.toLowerCase(Locale.ROOT).startsWith(key);
        }
        if (service.equalsIgnoreCase(key)) {
            return true;
        }
        String aliased = SERVICE_ALIASES.getOrDefault(key, "");
        if (!aliased.isEmpty() && service.equalsIgnoreCase(aliased)) {
            return true;
        }
        return service.isBlank() && !aliased.isEmpty()
                && name.toLowerCase(Locale.ROOT).startsWith(key + "_");
    }

    private static String serviceOf(String name, String advertised, Map<String, String> services) {
        if (!advertised.isBlank()) {
            return advertised;
        }
        return services.getOrDefault(name, "");
    }

    private static ObjectNode liveNode(Tool tool, String service) {
        ObjectNode node = Json.newObject();
        node.put("name", tool.name());
        node.put("service", service);
        node.put("description", tool.description() == null ? "" : tool.description());
        ArrayNode params = node.putArray("params");
        addRequiredParams(params, tool.inputSchema());
        return node;
    }

    private static void addRequiredParams(ArrayNode params, JsonNode schema) {
        if (schema == null || !schema.path("required").isArray()) {
            return;
        }
        for (JsonNode required : schema.path("required")) {
            String paramName = required.asText("");
            if (!paramName.isEmpty()) {
                ObjectNode param = params.addObject();
                param.put("name", paramName);
                param.put("required", true);
            }
        }
    }

    private static ObjectNode filteredIndex(JsonNode index, List<JsonNode> matched) {
        ObjectNode filtered = Json.newObject();
        filtered.put("schemaVersion", index.path("schemaVersion").asText(""));
        ArrayNode tools = filtered.putArray("tools");
        for (JsonNode tool : matched) {
            tools.add(tool);
        }
        return filtered;
    }

    private static String textLine(JsonNode tool) {
        return tool.path("name").asText("") + " — " + ToolsCommand.firstSentence(tool.path("description").asText(""));
    }

    private static String compactLine(JsonNode tool) {
        return formatCompact(tool.path("name").asText(""), requiredNames(tool.path("params")));
    }

    private static List<String> requiredNames(JsonNode params) {
        List<String> required = new ArrayList<>();
        if (!params.isArray()) {
            return required;
        }
        for (JsonNode param : params) {
            if (param.path("required").asBoolean(false)) {
                String paramName = param.path("name").asText("");
                if (!paramName.isEmpty()) {
                    required.add(paramName);
                }
            }
        }
        return required;
    }

    private static String formatCompact(String name, List<String> required) {
        if (required.isEmpty()) {
            return name;
        }
        return name + " (" + String.join(", ", required) + ")";
    }

    private static void printUnknownGroup(PrintWriter err, String group, JsonNode index) {
        err.println("No tools match group '" + group.trim() + "'.");
        err.println("Available groups: " + String.join(", ", new TreeSet<>(SERVICE_ALIASES.keySet())) + ".");
        err.println("Services: " + String.join(", ", serviceNames(index)) + ".");
        err.println("A tool-name prefix such as browser_, report_, or design_ also matches.");
        err.flush();
    }

    private static Set<String> serviceNames(JsonNode index) {
        Set<String> names = new TreeSet<>();
        JsonNode tools = index.path("tools");
        if (!tools.isArray()) {
            return names;
        }
        for (JsonNode tool : tools) {
            String service = tool.path("service").asText("");
            if (!service.isEmpty()) {
                names.add(service);
            }
        }
        return names;
    }

    private static Optional<JsonNode> find(JsonNode index, String toolName) {
        JsonNode tools = index.path("tools");
        if (!tools.isArray()) {
            return Optional.empty();
        }
        for (JsonNode tool : tools) {
            if (toolName.equals(tool.path("name").asText(""))) {
                return Optional.of(tool);
            }
        }
        return Optional.empty();
    }

    private static void writeDescribed(PrintWriter out, JsonNode tool) {
        out.println("name: " + tool.path("name").asText(""));
        out.println("service: " + tool.path("service").asText(""));
        out.println("description: " + tool.path("description").asText(""));
        writeAlias(out, "cli", cliAlias(tool));
        writeAlias(out, "slash", textOrEmpty(tool.path("slashAlias")));
        writeParameters(out, tool.path("params"));
        writeExample(out, tool.path("example"));
    }

    private static void writeAlias(PrintWriter out, String label, String value) {
        if (!value.isBlank()) {
            out.println(label + ": " + value);
        }
    }

    private static String cliAlias(JsonNode tool) {
        String value = textOrEmpty(tool.path("cliCommand"));
        if (value.isBlank()) {
            return "";
        }
        if (value.startsWith("shaft-cli ")) {
            return value;
        }
        return "shaft-cli " + value;
    }

    private static void writeParameters(PrintWriter out, JsonNode params) {
        out.println("parameters:");
        if (!params.isArray() || params.isEmpty()) {
            out.println("- (none)");
            return;
        }
        for (JsonNode param : params) {
            out.println("- " + parameterLine(param));
        }
    }

    private static String parameterLine(JsonNode param) {
        String name = param.path("name").asText("");
        String type = param.path("type").asText("");
        String requirement = param.path("required").asBoolean(false) ? "required" : "optional";
        String description = textOrEmpty(param.path("description"));
        String line = name + " (" + type + ", " + requirement + ")";
        if (description.isBlank()) {
            return line;
        }
        return line + ": " + description;
    }

    private static void writeExample(PrintWriter out, JsonNode example) {
        if (example.isMissingNode() || example.isNull()) {
            return;
        }
        out.println("example:");
        out.println(pretty(example));
    }

    private static String textOrEmpty(JsonNode node) {
        if (node.isMissingNode() || node.isNull()) {
            return "";
        }
        return node.asText("");
    }

    private static void printUnknownTool(PrintWriter err, JsonNode index, String toolName) {
        err.println("Unknown tool '" + toolName + "'.");
        List<String> matches = closeMatches(toolNames(index), toolName);
        if (matches.isEmpty()) {
            err.println("No close matches. List tools with `shaft-cli tools --cached --compact`.");
        } else {
            err.println("Close matches: " + String.join(", ", matches) + ".");
        }
        err.flush();
    }

    private static List<String> closeMatches(List<String> names, String toolName) {
        String needle = toolName.toLowerCase(Locale.ROOT);
        List<String> matches = new ArrayList<>();
        for (String name : names) {
            if (isClose(name, needle)) {
                matches.add(name);
            }
        }
        matches.sort(Comparator.comparingInt((String name) -> distance(name.toLowerCase(Locale.ROOT), needle))
                .thenComparing(Comparator.naturalOrder()));
        if (matches.size() > 5) {
            return new ArrayList<>(matches.subList(0, 5));
        }
        return matches;
    }

    private static boolean isClose(String name, String needle) {
        String lower = name.toLowerCase(Locale.ROOT);
        if (lower.contains(needle) || needle.contains(lower)) {
            return true;
        }
        return distance(lower, needle) <= 3;
    }

    private static List<String> toolNames(JsonNode index) {
        List<String> names = new ArrayList<>();
        JsonNode tools = index.path("tools");
        if (!tools.isArray()) {
            return names;
        }
        for (JsonNode tool : tools) {
            String name = tool.path("name").asText("");
            if (!name.isEmpty()) {
                names.add(name);
            }
        }
        return names;
    }

    private static int distance(String left, String right) {
        if (Math.abs(left.length() - right.length()) > 3) {
            return 4;
        }
        int[] row = new int[right.length() + 1];
        for (int column = 0; column < row.length; column++) {
            row[column] = column;
        }
        for (int i = 1; i <= left.length(); i++) {
            fillDistanceRow(row, left, right, i);
        }
        return row[right.length()];
    }

    private static void fillDistanceRow(int[] row, String left, String right, int i) {
        int previous = row[0];
        row[0] = i;
        for (int j = 1; j <= right.length(); j++) {
            int insert = row[j] + 1;
            int delete = row[j - 1] + 1;
            int replace = previous + replacementCost(left, right, i, j);
            previous = row[j];
            row[j] = Math.min(insert, Math.min(delete, replace));
        }
    }

    private static int replacementCost(String left, String right, int i, int j) {
        if (left.charAt(i - 1) == right.charAt(j - 1)) {
            return 0;
        }
        return 1;
    }

    private static Map<String, String> servicesByName() {
        Map<String, String> services = new HashMap<>();
        Optional<JsonNode> index = readCachedQuiet();
        if (index.isEmpty()) {
            return services;
        }
        for (JsonNode tool : index.get().path("tools")) {
            services.put(tool.path("name").asText(""), tool.path("service").asText(""));
        }
        return services;
    }

    private static Optional<JsonNode> readCachedQuiet() {
        try (InputStream stream = ToolsCommand.class.getResourceAsStream(ToolsCommand.CACHED_INDEX_RESOURCE)) {
            if (stream == null) {
                return Optional.empty();
            }
            return Optional.of(Json.MAPPER.readTree(stream));
        } catch (IOException | RuntimeException _) {
            return Optional.empty();
        }
    }

    private static JsonNode indexForGroups() {
        return readCachedQuiet().orElseGet(Json::newObject);
    }

    private static String pretty(JsonNode node) {
        return Json.MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(node);
    }
}
