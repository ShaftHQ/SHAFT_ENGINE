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
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Progressive discovery for {@code shaft-cli tools}: {@code --group}, {@code --compact}, and
 * {@code tools describe}. The refusing connector proves the cached and describe paths stay offline.
 */
class ToolsCommandDiscoveryTest {

    private static final McpConnector REFUSES_TO_CONNECT = (requiresSession, stdioOk) -> {
        throw new AssertionError("offline tools discovery must not connect to a live shaft-mcp server");
    };

    @Test
    void cachedGroupBrowserCompactStaysSmallAndOffline() {
        Result result = run("--cached", "--group", "browser", "--compact");

        assertEquals(0, result.exit);
        int bytes = result.out.getBytes(StandardCharsets.UTF_8).length;
        assertTrue(bytes <= 2048, "compact browser listing was " + bytes + " bytes");
        assertFalse(result.out.isEmpty());
        assertFalse(result.out.contains(" — "), result.out);
        assertFalse(result.out.contains("opens a URL"), result.out);
        assertFalse(result.out.contains("newWindow"), result.out);
        assertTrue(result.out.contains("browser_navigate (targetUrl)"), result.out);
        assertTrue(result.out.contains("browser_refresh\n"), result.out);
        for (String line : result.out.split("\\R")) {
            if (!line.isBlank()) {
                assertTrue(line.startsWith("browser_"), line);
            }
        }
    }

    @Test
    void cachedGroupMatchesServiceNameAndShortAlias() {
        Result exact = run("--cached", "--group", "CaptureService", "--compact");
        Result alias = run("--cached", "--group", "capture", "--compact");
        Result upper = run("--cached", "--group", "CAPTURE", "--compact");

        assertEquals(0, exact.exit);
        assertEquals(exact.out, alias.out);
        assertEquals(exact.out, upper.out);
        assertTrue(exact.out.contains("capture_start"), exact.out);
        assertFalse(exact.out.contains("browser_"), exact.out);
    }

    @Test
    void cachedUnknownGroupExplainsTheValidGroups() {
        Result result = run("--cached", "--group", "unknown-group");

        assertNotEquals(0, result.exit);
        assertTrue(result.out.isBlank(), result.out);
        assertTrue(result.err.contains("unknown-group"), result.err);
        assertTrue(result.err.contains("capture"), result.err);
        assertTrue(result.err.contains("CaptureService"), result.err);
        assertTrue(result.err.contains("browser"), result.err);
    }

    @Test
    void shortAliasDoesNotCollapseIntoANamePrefix() {
        Result doctor = run("--cached", "--group", "doctor", "--compact");
        Result doctorPrefix = run("--cached", "--group", "doctor_", "--compact");
        Result report = run("--cached", "--group", "report", "--compact");
        Result reportPrefix = run("--cached", "--group", "report_", "--compact");

        assertEquals(0, doctor.exit);
        assertTrue(doctor.out.contains("doctor_suggest_fix"), doctor.out);
        assertFalse(doctor.out.contains("doctor_analyze_trace"), doctor.out);
        assertTrue(doctorPrefix.out.contains("doctor_analyze_trace"), doctorPrefix.out);
        assertTrue(report.out.contains("test_automation_scenarios"), report.out);
        assertFalse(report.out.contains("report_open"), report.out);
        assertTrue(reportPrefix.out.contains("report_open"), reportPrefix.out);
        assertFalse(reportPrefix.out.contains("test_automation_scenarios"), reportPrefix.out);
    }

    @Test
    void cachedDefaultListingStillPrintsDescriptions() {
        Result result = run("--cached");

        assertEquals(0, result.exit);
        assertTrue(result.out.contains("browser_navigate — "), result.out);
    }

    @Test
    void describeCaptureStartPrintsTextAndJsonOffline() throws Exception {
        Result text = run("describe", "capture_start");
        Result json = run("describe", "capture_start", "--json");

        assertEquals(0, text.exit, text.err);
        assertTrue(text.out.contains("name: capture_start"), text.out);
        assertTrue(text.out.contains("service: CaptureService"), text.out);
        assertTrue(text.out.contains("shaft-cli capture start"), text.out);
        assertTrue(text.out.contains("slash: start"), text.out);
        assertTrue(text.out.contains("targetUrl"), text.out);
        assertEquals(0, json.exit, json.err);
        JsonNode tool = Json.MAPPER.readTree(json.out);
        assertEquals("capture_start", tool.path("name").asText());
        assertEquals("CaptureService", tool.path("service").asText());
        assertTrue(tool.path("params").isArray());
        assertFalse(tool.has("tools"));
    }

    @Test
    void describeDesignHandoffShowsTheCliAlias() {
        Result result = run("describe", "design_handoff");

        assertEquals(0, result.exit, result.err);
        assertTrue(result.out.contains("shaft-cli design handoff"), result.out);
    }

    @Test
    void describeUnknownToolFailsWithHelp() {
        Result unknown = run("describe", "no_such_tool");
        Result near = run("describe", "capture_star");

        assertNotEquals(0, unknown.exit);
        assertTrue(unknown.err.contains("no_such_tool"), unknown.err);
        assertTrue(unknown.err.contains("shaft-cli tools"), unknown.err);
        assertNotEquals(0, near.exit);
        assertTrue(near.err.contains("capture_start"), near.err);
    }

    @Test
    void liveGroupFiltersByPrefixAndByCachedService() {
        ObjectNode schema = Json.newObject();
        schema.putArray("required").add("targetUrl");
        ObjectNode customSchema = Json.newObject();
        customSchema.putArray("required").add("url");
        FakeClient client = new FakeClient(List.of(
                new Tool("browser_navigate", "Navigates to a URL. Extra detail.", schema),
                new Tool("browser_custom", "Custom browser tool. Extra.", customSchema),
                new Tool("element_click", "Clicks an element. Extra.", null)
        ), rawTools());

        Result compact = run(client, "--group", "browser", "--compact");
        Result prefix = run(client, "--group", "browser_", "--compact");
        Result json = run(client, "--json", "--group", "browser_");

        Result aliasJson = run(client, "--json", "--group", "browser");

        assertEquals(0, compact.exit, compact.err);
        assertTrue(compact.out.contains("browser_navigate (targetUrl)"), compact.out);
        assertTrue(compact.out.contains("browser_custom (url)"), compact.out);
        assertFalse(compact.out.contains("element_click"), compact.out);
        assertFalse(compact.out.contains("Navigates"), compact.out);
        assertTrue(prefix.out.contains("browser_custom (url)"), prefix.out);
        assertTrue(prefix.out.contains("browser_navigate (targetUrl)"), prefix.out);
        assertFalse(prefix.out.contains("element_click"), prefix.out);
        assertEquals(0, json.exit, json.err);
        assertTrue(json.out.contains("browser_navigate"), json.out);
        assertTrue(json.out.contains("browser_custom"), json.out);
        assertFalse(json.out.contains("element_click"), json.out);
        assertEquals(0, aliasJson.exit, aliasJson.err);
        assertTrue(aliasJson.out.contains("browser_custom"), aliasJson.out);
        assertTrue(aliasJson.out.contains("browser_navigate"), aliasJson.out);
        assertFalse(aliasJson.out.contains("element_click"), aliasJson.out);
        assertTrue(client.closed);
    }

    @Test
    void liveReportAliasMatchesUnknownPrefixedToolAndSkipsCachedTraceReport() {
        ObjectNode schema = Json.newObject();
        schema.putArray("required").add("path");
        ObjectNode raw = Json.newObject();
        var tools = raw.putArray("tools");
        tools.addObject().put("name", "report_custom").put("description", "Custom report tool. Extra.");
        tools.addObject().put("name", "report_open").put("description", "Opens a report. Extra.");
        tools.addObject().put("name", "test_automation_scenarios").put("description", "Lists scenarios. Extra.");
        FakeClient client = new FakeClient(List.of(
                new Tool("report_custom", "Custom report tool. Extra.", schema),
                new Tool("report_open", "Opens a report. Extra.", null),
                new Tool("test_automation_scenarios", "Lists scenarios. Extra.", null)
        ), raw);

        Result compact = run(client, "--group", "report", "--compact");

        assertEquals(0, compact.exit, compact.err);
        assertTrue(compact.out.contains("report_custom (path)"), compact.out);
        assertTrue(compact.out.contains("test_automation_scenarios"), compact.out);
        assertFalse(compact.out.contains("report_open"), compact.out);
    }

    @Test
    void groupMatchCoversAliasPrefixFallbackAndKnownServiceExclusion() {
        assertTrue(ToolCatalog.matches("browser_navigate", "BrowserService", "  "));
        assertTrue(ToolCatalog.matches("browser_custom", "", "browser"));
        assertTrue(ToolCatalog.matches("Browser_Custom", "", "BROWSER"));
        assertTrue(ToolCatalog.matches("browser_navigate", "BrowserService", "browser"));
        assertTrue(ToolCatalog.matches("browser_navigate", "BrowserService", "BrowserService"));
        assertFalse(ToolCatalog.matches("report_open", "TraceService", "report"));
        assertTrue(ToolCatalog.matches("report_open", "TraceService", "report_"));
        assertFalse(ToolCatalog.matches("element_click", "", "browser"));
        assertFalse(ToolCatalog.matches("browser_custom", "ElementService", "browser"));
    }

    @Test
    void renderCachedFiltersJsonAndTextAndRejectsAnUnknownGroup() throws Exception {
        JsonNode index = Json.MAPPER.readTree("""
                {
                  "schemaVersion": "1.0",
                  "tools": [
                    {
                      "name": "browser_navigate",
                      "service": "BrowserService",
                      "description": "Navigates to a URL. Extra.",
                      "params": [{"name": "targetUrl", "type": "string", "required": true, "description": "page url"}],
                      "cliCommand": "browser navigate",
                      "slashAlias": null,
                      "example": {"request": {}}
                    },
                    {
                      "name": "element_click",
                      "service": "ElementService",
                      "description": "Clicks.",
                      "params": []
                    }
                  ]
                }
                """);
        StringWriter out = new StringWriter();
        StringWriter err = new StringWriter();
        PrintWriter outWriter = new PrintWriter(out, true);
        PrintWriter errWriter = new PrintWriter(err, true);

        assertEquals(0, ToolCatalog.renderCached(index, outWriter, errWriter, true, false, "browser"));
        assertTrue(out.toString().contains("browser_navigate"), out.toString());
        assertFalse(out.toString().contains("element_click"), out.toString());

        out.getBuffer().setLength(0);
        assertEquals(0, ToolCatalog.renderCached(index, outWriter, errWriter, false, false, "browser"));
        assertTrue(out.toString().contains("browser_navigate — Navigates to a URL."), out.toString());
        assertFalse(out.toString().contains("Extra."), out.toString());

        out.getBuffer().setLength(0);
        err.getBuffer().setLength(0);
        assertEquals(2, ToolCatalog.renderCached(index, outWriter, errWriter, false, true, "missing-group"));
        assertTrue(err.toString().contains("missing-group"), err.toString());

        JsonNode broken = Json.MAPPER.readTree("{\"schemaVersion\":\"1.0\",\"tools\":{}}");
        out.getBuffer().setLength(0);
        err.getBuffer().setLength(0);
        assertEquals(2, ToolCatalog.renderCached(broken, outWriter, errWriter, false, true, "browser"));
        assertTrue(err.toString().contains("browser"), err.toString());
    }

    @Test
    void renderLivePrintsSentencesAndRejectsBadOrEmptyJson() {
        ObjectNode schema = Json.newObject();
        schema.putArray("required").add("url");
        FakeClient client = new FakeClient(List.of(
                new Tool("browser_custom", "Custom browser tool. Extra detail.", schema)
        ), Json.MAPPER.readTree("[]"));
        StringWriter out = new StringWriter();
        StringWriter err = new StringWriter();
        PrintWriter outWriter = new PrintWriter(out, true);
        PrintWriter errWriter = new PrintWriter(err, true);

        assertEquals(0, ToolCatalog.renderLive(client, outWriter, errWriter, false, false, "browser"));
        assertTrue(out.toString().contains("browser_custom — Custom browser tool."), out.toString());
        assertFalse(out.toString().contains("Extra detail."), out.toString());

        out.getBuffer().setLength(0);
        err.getBuffer().setLength(0);
        assertEquals(1, ToolCatalog.renderLive(client, outWriter, errWriter, true, false, "browser"));
        assertTrue(err.toString().contains("not a JSON object"), err.toString());

        FakeClient empty = new FakeClient(List.of(), Json.newObject());
        out.getBuffer().setLength(0);
        err.getBuffer().setLength(0);
        assertEquals(2, ToolCatalog.renderLive(empty, outWriter, errWriter, true, false, "browser"));
        assertTrue(err.toString().contains("browser"), err.toString());

        out.getBuffer().setLength(0);
        err.getBuffer().setLength(0);
        assertEquals(2, ToolCatalog.renderLive(client, outWriter, errWriter, false, true, "no-such-group"));
        assertTrue(err.toString().contains("no-such-group"), err.toString());
    }

    @Test
    void describeAutobotClientsPrintsTheRecordedExample() {
        Result described = run("describe", "autobot_local_agent_clients");

        assertEquals(0, described.exit, described.err);
        assertTrue(described.out.contains("example:"), described.out);
        assertTrue(described.out.contains("- (none)"), described.out);
        assertFalse(described.out.contains("cli:"), described.out);
    }

    @Test
    void cachedCompactWithoutGroupAndGroupedTextStayOffline() {
        Result compact = run("--cached", "--compact");
        Result grouped = run("--cached", "--group", "browser");

        assertEquals(0, compact.exit, compact.err);
        assertTrue(compact.out.contains("browser_navigate"), compact.out);
        assertFalse(compact.out.contains(" — "), compact.out);
        assertEquals(0, grouped.exit, grouped.err);
        assertTrue(grouped.out.contains("browser_navigate — "), grouped.out);
        assertFalse(grouped.out.contains("element_click"), grouped.out);
    }

    @Test
    void liveCompactWithoutGroupListsEveryReturnedTool() {
        ObjectNode schema = Json.newObject();
        schema.putArray("required").add("targetUrl");
        FakeClient client = new FakeClient(List.of(
                new Tool("browser_navigate", "Navigates to a URL. Extra detail.", schema),
                new Tool("element_click", "Clicks an element.", null)
        ), rawTools());

        Result compact = run(client, "--compact");

        assertEquals(0, compact.exit, compact.err);
        assertTrue(compact.out.contains("browser_navigate (targetUrl)"), compact.out);
        assertTrue(compact.out.contains("element_click"), compact.out);
        assertTrue(client.closed);
    }

    private static ObjectNode rawTools() {
        ObjectNode raw = Json.newObject();
        var tools = raw.putArray("tools");
        tools.addObject().put("name", "browser_navigate").put("description", "Navigates to a URL. Extra detail.");
        tools.addObject().put("name", "browser_custom").put("description", "Custom browser tool. Extra.");
        tools.addObject().put("name", "element_click").put("description", "Clicks an element. Extra.");
        return raw;
    }

    private static Result run(String... args) {
        return run(REFUSES_TO_CONNECT, args);
    }

    private static Result run(McpClient client, String... args) {
        return run((requiresSession, stdioOk) -> client, args);
    }

    private static Result run(McpConnector connector, String... args) {
        StringWriter out = new StringWriter();
        StringWriter err = new StringWriter();
        int exit = new CommandLine(new ToolsCommand(connector))
                .setOut(new PrintWriter(out, true))
                .setErr(new PrintWriter(err, true))
                .execute(args);
        return new Result(exit, out.toString(), err.toString());
    }

    private record Result(int exit, String out, String err) {
    }

    private static final class FakeClient implements McpClient {
        private final List<Tool> tools;
        private final JsonNode raw;
        private boolean closed;

        private FakeClient(List<Tool> tools, JsonNode raw) {
            this.tools = tools;
            this.raw = raw;
        }

        @Override
        public InitializeResult initialize() {
            throw new UnsupportedOperationException("not used by tools discovery");
        }

        @Override
        public List<Tool> listTools() {
            return tools;
        }

        @Override
        public CallToolResult callTool(String name, ObjectNode arguments) {
            throw new UnsupportedOperationException("not used by tools discovery");
        }

        @Override
        public JsonNode listToolsRaw() {
            return raw;
        }

        @Override
        public JsonNode callToolRaw(String name, ObjectNode arguments) {
            throw new UnsupportedOperationException("not used by tools discovery");
        }

        @Override
        public void close() {
            closed = true;
        }
    }
}
