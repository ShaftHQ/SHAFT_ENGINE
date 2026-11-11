package com.shaft.commandline.command;

import com.shaft.commandline.runtime.McpConnector;
import com.shaft.commandline.testsupport.InProcessMcp;
import org.junit.jupiter.api.Test;
import picocli.CommandLine;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.concurrent.Callable;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Curated alias commands are pure action-to-tool mappings over {@code call}. These tests exercise
 * the mappings that previously had no direct invocation, against the in-process fake MCP server.
 */
class CuratedAliasCommandTest {

    private final McpConnector connector = InProcessMcp.connector();

    @Test
    void guideSearchDispatchesRegardlessOfActionCase() {
        assertDispatches(new GuideCommand(connector), "search", "shaft_guide_search", "query=click element");
        assertDispatches(new GuideCommand(connector), "SEARCH", "shaft_guide_search", "query=click element");
    }

    @Test
    void unknownGuideActionListsTheValidActionAndDoesNotCallATool() {
        assertUnknown(new GuideCommand(connector), "guide", "browse");
    }

    @Test
    void elementAliasesDispatchClickTypeAndHover() {
        assertDispatches(new ElementCommand(connector), "click", "element_click", "locator=#go");
        assertDispatches(new ElementCommand(connector), "type", "element_type", "text=hello");
        assertDispatches(new ElementCommand(connector), "hover", "element_hover", "locator=#go");
    }

    @Test
    void unknownElementActionIsRejected() {
        assertUnknown(new ElementCommand(connector), "element", "drag");
    }

    @Test
    void browserAliasesDispatchNavigateScreenshotDomAndUrl() {
        assertDispatches(new BrowserCommand(connector), "navigate", "browser_navigate", "url=https://example.test");
        assertDispatches(new BrowserCommand(connector), "screenshot", "browser_take_screenshot");
        assertDispatches(new BrowserCommand(connector), "dom", "browser_get_page_dom");
        assertDispatches(new BrowserCommand(connector), "URL", "browser_get_current_url");
    }

    @Test
    void unknownBrowserActionIsRejected() {
        assertUnknown(new BrowserCommand(connector), "browser", "fly");
    }

    @Test
    void doctorAliasesDispatchTheRemainingReadOnlyTools() {
        assertDispatches(new DoctorCommand(connector), "analyze", "doctor_analyze_trace", "trace=target/missing.json");
        assertDispatches(new DoctorCommand(connector), "suggest", "doctor_suggest_fix");
        assertDispatches(new DoctorCommand(connector), "cause-label", "doctor_cause_label");
    }

    @Test
    void unknownDoctorActionIsRejected() {
        assertUnknown(new DoctorCommand(connector), "doctor", "heal");
    }

    @Test
    void unknownDesignActionIsRejected() {
        assertUnknown(new DesignCommand(connector), "design", "ship");
    }

    private void assertDispatches(Callable<Integer> command, String action, String tool, String... extra) {
        StringWriter out = new StringWriter();
        StringWriter err = new StringWriter();
        String[] args = new String[extra.length + 1];
        args[0] = action;
        System.arraycopy(extra, 0, args, 1, extra.length);
        int exit = new CommandLine(command)
                .setOut(new PrintWriter(out, true))
                .setErr(new PrintWriter(err, true))
                .execute(args);
        assertEquals(0, exit, action + " stderr: " + err);
        assertTrue(out.toString().contains("called " + tool), out.toString());
        for (String argument : extra) {
            assertTrue(out.toString().contains(argument.substring(argument.indexOf('=') + 1)), out.toString());
        }
    }

    private void assertUnknown(Callable<Integer> command, String label, String action) {
        StringWriter out = new StringWriter();
        StringWriter err = new StringWriter();
        int exit = new CommandLine(command)
                .setOut(new PrintWriter(out, true))
                .setErr(new PrintWriter(err, true))
                .execute(action);
        assertEquals(2, exit);
        assertTrue(err.toString().contains("Unknown " + label + " action: " + action), err.toString());
        assertTrue(err.toString().contains("Valid actions:"), err.toString());
        assertFalse(out.toString().contains("called "), out.toString());
    }
}
