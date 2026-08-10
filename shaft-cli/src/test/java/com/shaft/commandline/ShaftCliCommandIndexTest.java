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
}
