package com.shaft.commandline.command;

import com.shaft.commandline.runtime.McpConnector;
import com.shaft.commandline.testsupport.FakeMcpServer;
import com.shaft.commandline.testsupport.InProcessMcp;
import org.junit.jupiter.api.Test;
import picocli.CommandLine;

import java.io.PrintWriter;
import java.io.StringWriter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Parity contract: the generic {@code call} command dispatches to any tool name advertised by
 * {@code tools/list}, including names the CLI never hard-coded. Exercises the real picocli command
 * wiring end-to-end against the in-process fake server.
 */
class CallDispatchParityTest {

    private final McpConnector connector = InProcessMcp.connector();

    @Test
    void toolsListsArbitraryToolName() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new ToolsCommand(connector))
                .setOut(new PrintWriter(out, true))
                .execute();
        assertEquals(0, exit);
        assertTrue(out.toString().contains(FakeMcpServer.ARBITRARY_TOOL),
                "tools output should list the arbitrary tool name");
    }

    @Test
    void callDispatchesArbitraryToolName() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new CallCommand(connector))
                .setOut(new PrintWriter(out, true))
                .execute(FakeMcpServer.ARBITRARY_TOOL, "key=value");
        assertEquals(0, exit);
        assertTrue(out.toString().contains("called " + FakeMcpServer.ARBITRARY_TOOL),
                "generic call should dispatch an arbitrary tool name");
        assertTrue(out.toString().contains("value"), "arguments should reach the tool");
    }

    @Test
    void callReturnsNonZeroOnToolError() {
        int exit = new CommandLine(new CallCommand(connector))
                .setOut(new PrintWriter(new StringWriter(), true))
                .setErr(new PrintWriter(new StringWriter(), true))
                .execute(FakeMcpServer.FAILING_TOOL);
        assertEquals(1, exit);
    }

    @Test
    void callRendersRawJsonWhenRequested() {
        StringWriter out = new StringWriter();
        int exit = new CommandLine(new CallCommand(connector))
                .setOut(new PrintWriter(out, true))
                .execute(FakeMcpServer.ECHO_TOOL, "--json", "value=hi");
        assertEquals(0, exit);
        assertTrue(out.toString().contains("\"content\""), "raw output should include the JSON result");
    }
}
