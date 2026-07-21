package com.shaft.commandline.command;

import com.shaft.commandline.mcp.CallToolResult;
import com.shaft.commandline.mcp.InitializeResult;
import com.shaft.commandline.mcp.McpClient;
import com.shaft.commandline.mcp.Tool;
import com.shaft.commandline.runtime.McpConnector;
import com.shaft.commandline.util.Json;
import org.junit.jupiter.api.Test;
import picocli.CommandLine;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ObjectNode;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link ToolsCommand}'s real (non-{@code --cached}) connection path, its default
 * constructor, and {@code firstSentence}'s branches -- none of which {@code ToolsCommandCachedTest}
 * exercises, since that class only drives {@code --cached}.
 */
class ToolsCommandTest {

    @Test
    void defaultConstructorUsesRealConnectionFactoryWithoutConnecting() {
        // Constructing the real ConnectionFactory must not itself attempt any MCP connection;
        // only calling the command would. This just proves wiring the production constructor
        // (the one Picocli uses) doesn't blow up before any command actually runs.
        ToolsCommand command = new ToolsCommand();

        assertTrue(true, "constructed without throwing: " + command);
    }

    @Test
    void nonCachedJsonPrintsRawToolsListFromTheLiveConnection() {
        ObjectNode raw = Json.newObject();
        raw.put("marker", "raw-tools-list");
        FakeMcpClient client = new FakeMcpClient(List.of(), raw);
        StringWriter out = new StringWriter();

        int exit = new CommandLine(new ToolsCommand((requiresSession, stdioOk) -> client))
                .setOut(new PrintWriter(out, true))
                .execute("--json");

        assertEquals(0, exit);
        assertTrue(out.toString().contains("raw-tools-list"), out.toString());
        assertTrue(client.closed, "client must be closed after use");
    }

    @Test
    void nonCachedListsToolsUsingFirstSentenceOfDescription() {
        FakeMcpClient client = new FakeMcpClient(List.of(
                new Tool("browser_navigate", "Navigates to a URL. Extra detail that must be trimmed.", null)
        ), Json.newObject());
        StringWriter out = new StringWriter();

        int exit = new CommandLine(new ToolsCommand((requiresSession, stdioOk) -> client))
                .setOut(new PrintWriter(out, true))
                .execute();

        assertEquals(0, exit);
        assertTrue(out.toString().contains("browser_navigate — Navigates to a URL."), out.toString());
        assertFalse(out.toString().contains("Extra detail"), out.toString());
        assertTrue(client.closed, "client must be closed after use");
    }

    @Test
    void firstSentenceReturnsEmptyForNullDescription() throws Exception {
        assertEquals("", invokeFirstSentence(null));
    }

    @Test
    void firstSentenceReturnsEmptyForBlankDescription() throws Exception {
        assertEquals("", invokeFirstSentence("   "));
    }

    @Test
    void firstSentenceCutsAtTheFirstPeriodSpace() throws Exception {
        assertEquals("First sentence.", invokeFirstSentence("First sentence. Second sentence."));
    }

    @Test
    void firstSentenceCutsAtANewlineWhenItComesBeforeAnyPeriod() throws Exception {
        assertEquals("First line", invokeFirstSentence("First line\nSecond line. Third."));
    }

    @Test
    void firstSentenceReturnsTheWholeStrippedDescriptionWhenNoTerminatorExists() throws Exception {
        assertEquals("No terminator here", invokeFirstSentence("  No terminator here  "));
    }

    private static String invokeFirstSentence(String description) throws Exception {
        Method method = ToolsCommand.class.getDeclaredMethod("firstSentence", String.class);
        method.setAccessible(true);
        return (String) method.invoke(null, (Object) description);
    }

    private static final class FakeMcpClient implements McpClient {
        private final List<Tool> tools;
        private final JsonNode raw;
        private boolean closed;

        private FakeMcpClient(List<Tool> tools, JsonNode raw) {
            this.tools = tools;
            this.raw = raw;
        }

        @Override
        public InitializeResult initialize() {
            throw new UnsupportedOperationException("not used by `tools`");
        }

        @Override
        public List<Tool> listTools() {
            return tools;
        }

        @Override
        public CallToolResult callTool(String name, ObjectNode arguments) {
            throw new UnsupportedOperationException("not used by `tools`");
        }

        @Override
        public JsonNode listToolsRaw() {
            return raw;
        }

        @Override
        public JsonNode callToolRaw(String name, ObjectNode arguments) {
            throw new UnsupportedOperationException("not used by `tools`");
        }

        @Override
        public void close() {
            closed = true;
        }
    }
}
