package com.shaft.intellij.mcp;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/** Reads the canonical SHAFT CLI command surface bundled from {@code shaft-cli}. */
public final class ShaftCliCommandIndex {
    private static final String RESOURCE_PATH = "/META-INF/shaft-cli/command-index.json";

    private ShaftCliCommandIndex() {
    }

    /** @return every indexed top-level SHAFT CLI command, sorted by name */
    public static List<CommandMetadata> commands() {
        return Holder.COMMANDS;
    }

    private static final class Holder {
        private static final List<CommandMetadata> COMMANDS = load();

        private static List<CommandMetadata> load() {
            try (InputStream stream = ShaftCliCommandIndex.class.getResourceAsStream(RESOURCE_PATH)) {
                if (stream == null) {
                    return List.of();
                }
                JsonElement root;
                try (InputStreamReader reader = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
                    root = JsonParser.parseReader(reader);
                }
                if (!root.isJsonObject()) {
                    return List.of();
                }
                JsonElement commandsElement = root.getAsJsonObject().get("commands");
                if (commandsElement == null || !commandsElement.isJsonArray()) {
                    return List.of();
                }
                List<CommandMetadata> commands = new ArrayList<>();
                JsonArray commandArray = commandsElement.getAsJsonArray();
                for (JsonElement element : commandArray) {
                    if (!element.isJsonObject()) {
                        continue;
                    }
                    JsonObject command = element.getAsJsonObject();
                    String name = string(command, "name");
                    if (!name.isBlank()) {
                        commands.add(new CommandMetadata(name, string(command, "description")));
                    }
                }
                commands.sort(Comparator.comparing(CommandMetadata::name));
                return List.copyOf(commands);
            } catch (RuntimeException | java.io.IOException ignored) {
                return List.of();
            }
        }

        private static String string(JsonObject object, String key) {
            JsonElement value = object.get(key);
            return value != null && value.isJsonPrimitive() ? value.getAsString() : "";
        }
    }

    /** Metadata for one top-level {@code shaft-cli} command. */
    public record CommandMetadata(String name, String description) {
    }
}
