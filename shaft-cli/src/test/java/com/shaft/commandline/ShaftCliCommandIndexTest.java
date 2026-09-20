package com.shaft.commandline;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import picocli.CommandLine;

import java.io.InputStream;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftCliCommandIndexTest {

    @Test
    void bundledCommandIndexMatchesTheRegisteredPicocliSurface() throws Exception {
        InputStream stream = ShaftCli.class.getResourceAsStream("/META-INF/shaft-cli/command-index.json");
        assertNotNull(stream, "the canonical SHAFT CLI command index must be bundled");

        JsonNode root;
        try (stream) {
            root = new ObjectMapper().readTree(stream);
        }
        assertEquals("1.0", root.path("schemaVersion").asText());
        Map<String, String> indexed = StreamSupport.stream(root.path("commands").spliterator(), false)
                .collect(Collectors.toMap(
                        command -> command.path("name").asText(),
                        command -> command.path("description").asText()));

        Map<String, CommandLine> registered = new CommandLine(new ShaftCli()).getSubcommands();
        assertEquals(registered.keySet(), indexed.keySet());
        for (String name : registered.keySet()) {
            String description = String.join(" ",
                            registered.get(name).getCommandSpec().usageMessage().description())
                    .replace("%n", "")
                    .replaceAll("\\s+", " ")
                    .trim();
            assertEquals(indexed.get(name), description, name);
        }
    }

    @Test
    void threeStageUxFamiliesAreIndexed() throws Exception {
        // SC-001 / #5943: design and report CLI families must stay in command-index.json.
        InputStream stream = ShaftCli.class.getResourceAsStream("/META-INF/shaft-cli/command-index.json");
        assertNotNull(stream);
        JsonNode root;
        try (stream) {
            root = new ObjectMapper().readTree(stream);
        }
        var names = StreamSupport.stream(root.path("commands").spliterator(), false)
                .map(command -> command.path("name").asText())
                .collect(Collectors.toSet());
        assertTrue(names.contains("design"), "command-index must list design");
        assertTrue(names.contains("report"), "command-index must list report");
        assertTrue(names.contains("capture"), "command-index must list capture");
        assertTrue(names.contains("doctor"), "command-index must list doctor");
        Map<String, CommandLine> registered = new CommandLine(new ShaftCli()).getSubcommands();
        assertTrue(registered.containsKey("design"));
        assertTrue(registered.containsKey("report"));
    }
}

