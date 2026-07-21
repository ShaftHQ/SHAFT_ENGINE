package com.shaft.intellij.mcp;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * Lazy-holder reader for the bundled build-time copy of shaft-mcp's canonical
 * {@code META-INF/shaft-mcp/tool-index.json} (design doc Decision 4/5, amendment A7): the
 * single source of truth for the surviving tool catalog and any curated {@code intentKeywords}
 * overlay entries, consumed by {@code com.shaft.intellij.ui.AssistantCommand}'s deterministic
 * routing instead of a second hand-maintained keyword table (issue #3870/#3866 T4).
 *
 * <p>Loaded once, on first access, via the classic initialization-on-demand holder idiom (never
 * eagerly at plugin/class-load time, per amendment A7) -- reading a several-thousand-line JSON
 * resource on every IDE startup for a feature that may never be used would be wasteful. A missing,
 * unreadable, or malformed resource degrades to an empty catalog rather than throwing:
 * AssistantCommand's routing must keep working off its built-in fallback keyword lists even if the
 * bundled copy is absent (for example a test classpath that only wires shaft-mcp's classes, not its
 * resources).</p>
 */
public final class ToolCatalogIndex {
    private static final String RESOURCE_PATH = "/META-INF/shaft-mcp/tool-index.json";

    private ToolCatalogIndex() {
    }

    /**
     * @return every surviving tool name in the bundled catalog, or empty when the resource is
     *     missing/unreadable
     */
    public static Set<String> toolNames() {
        return Holder.TOOL_NAMES;
    }

    /**
     * @param toolName exact tool name (e.g. {@code "capture_start"})
     * @return the curated {@code intentKeywords} for that tool, or an empty list when the tool is
     *     unknown or has no curated keywords yet
     */
    public static List<String> intentKeywords(String toolName) {
        return Holder.INTENT_KEYWORDS.getOrDefault(toolName, List.of());
    }

    /**
     * @return every curated {@code slashAlias} (lower-cased) mapped to its canonical tool name
     *     (issue #3883(a)), or an empty map when the resource is missing/unreadable or no tool
     *     carries a curated alias yet. A tool with no {@code slashAlias} overlay entry contributes
     *     no mapping.
     */
    public static Map<String, String> slashAliases() {
        return Holder.SLASH_ALIASES;
    }

    /**
     * Initialization-on-demand holder: the JSON resource is read only when a caller first touches
     * {@link #toolNames()} or {@link #intentKeywords(String)}, not when {@link ToolCatalogIndex}
     * itself is loaded.
     */
    private static final class Holder {
        private static final Set<String> TOOL_NAMES;
        private static final Map<String, List<String>> INTENT_KEYWORDS;
        private static final Map<String, String> SLASH_ALIASES;

        static {
            Set<String> names = new LinkedHashSet<>();
            Map<String, List<String>> keywords = new LinkedHashMap<>();
            Map<String, String> aliases = new LinkedHashMap<>();
            parse(names, keywords, aliases);
            TOOL_NAMES = Collections.unmodifiableSet(names);
            INTENT_KEYWORDS = Collections.unmodifiableMap(keywords);
            SLASH_ALIASES = Collections.unmodifiableMap(aliases);
        }

        private static void parse(Set<String> names, Map<String, List<String>> keywords, Map<String, String> aliases) {
            try (InputStream stream = ToolCatalogIndex.class.getResourceAsStream(RESOURCE_PATH)) {
                if (stream == null) {
                    return;
                }
                JsonElement root;
                try (InputStreamReader reader = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
                    root = JsonParser.parseReader(reader);
                }
                if (!root.isJsonObject()) {
                    return;
                }
                JsonElement tools = root.getAsJsonObject().get("tools");
                if (tools == null || !tools.isJsonArray()) {
                    return;
                }
                for (JsonElement element : tools.getAsJsonArray()) {
                    if (!element.isJsonObject()) {
                        continue;
                    }
                    JsonObject tool = element.getAsJsonObject();
                    JsonElement nameElement = tool.get("name");
                    if (nameElement == null || !nameElement.isJsonPrimitive()) {
                        continue;
                    }
                    String name = nameElement.getAsString();
                    names.add(name);
                    keywords.put(name, stringArray(tool.get("intentKeywords")));
                    JsonElement aliasElement = tool.get("slashAlias");
                    if (aliasElement != null && aliasElement.isJsonPrimitive()) {
                        String alias = aliasElement.getAsString();
                        if (alias != null && !alias.isBlank()) {
                            aliases.put(alias.toLowerCase(Locale.ROOT), name);
                        }
                    }
                }
            } catch (IOException | RuntimeException ignored) {
                // Best-effort catalog only: AssistantCommand's built-in keyword lists remain the
                // fallback source of truth when the bundled resource is missing/malformed.
            }
        }

        private static List<String> stringArray(JsonElement element) {
            if (element == null || !element.isJsonArray()) {
                return List.of();
            }
            JsonArray array = element.getAsJsonArray();
            if (array.isEmpty()) {
                return List.of();
            }
            List<String> values = new ArrayList<>(array.size());
            for (JsonElement item : array) {
                if (item.isJsonPrimitive()) {
                    values.add(item.getAsString());
                }
            }
            return List.copyOf(values);
        }
    }
}
