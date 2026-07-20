package com.shaft.commandline.command;

import com.shaft.commandline.runtime.McpConnector;
import org.junit.jupiter.api.Test;
import picocli.CommandLine;

import java.io.PrintWriter;
import java.io.StringWriter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * {@code shaft-cli tools --cached} lists the tool catalog from the bundled offline snapshot
 * (tracker #3866 T3/#3869) instead of connecting to a live shaft-mcp server.
 */
class ToolsCommandCachedTest {

    /** Fails the test if the cached path ever tries to open an MCP connection. */
    private static final McpConnector REFUSES_TO_CONNECT = (requiresSession, stdioOk) -> {
        throw new AssertionError("`tools --cached` must not connect to a live shaft-mcp server.");
    };

    @Test
    void cachedListsToolsWithoutConnecting() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new ToolsCommand(REFUSES_TO_CONNECT))
                .setOut(new PrintWriter(out, true))
                .execute("--cached");

        assertEquals(0, exit);
        assertTrue(out.toString().contains("browser_navigate"), "cached listing should include a known tool");
        assertFalse(out.toString().contains("natural_act"), "cached listing must not include a deleted tool");
    }

    @Test
    void cachedJsonListsAllEightyNineTools() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new ToolsCommand(REFUSES_TO_CONNECT))
                .setOut(new PrintWriter(out, true))
                .execute("--cached", "--json");

        assertEquals(0, exit);
        assertTrue(out.toString().contains("\"totalTools\" : 89"),
                "cached --json output should include the manifest's totalTools field");
    }
}
