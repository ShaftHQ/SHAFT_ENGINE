package com.shaft.commandline.command;

import com.shaft.commandline.mcp.McpClient;
import com.shaft.commandline.mcp.Tool;
import com.shaft.commandline.runtime.ConnectionFactory;
import com.shaft.commandline.runtime.McpConnector;
import com.shaft.commandline.util.Json;
import picocli.CommandLine.Command;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Option;
import picocli.CommandLine.Spec;
import tools.jackson.databind.JsonNode;

import java.io.PrintWriter;
import java.util.Optional;
import java.util.concurrent.Callable;

/**
 * Lists the tools shaft-mcp exposes. {@code --cached} reads the bundled index offline.
 * {@code --group} filters by service or tool-name prefix, {@code --compact} prints required
 * argument names only, and {@code describe} prints one tool.
 */
@Command(mixinStandardHelpOptions = true,
        name = "tools",
        description = "List the tools exposed by shaft-mcp. Filter with --group, "
                + "print one line per tool with --compact, or run describe <tool> for one tool.",
        subcommands = {ToolsDescribeCommand.class})
public final class ToolsCommand implements Callable<Integer> {

    /** Classpath location of the offline catalog snapshot read by {@code --cached}. */
    static final String CACHED_INDEX_RESOURCE = "/tool-index-cache.json";

    @Option(names = "--json", description = "Print the raw tools/list result.")
    private boolean json;

    @Option(names = "--cached",
            description = "List the generated tool-index snapshot bundled with this shaft-cli version, "
                    + "without connecting to a live shaft-mcp server.")
    private boolean cached;

    @Option(names = "--group", paramLabel = "SERVICE_OR_PREFIX",
            description = "Keep tools in one service (CaptureService or capture) or with one name prefix (browser_).")
    private String group = "";

    @Option(names = "--compact",
            description = "Print one line per tool: the name, plus required argument names. Omits descriptions.")
    private boolean compact;

    @Spec
    private CommandSpec spec;

    private final McpConnector factory;

    /**
     * Uses the real connection factory.
     */
    public ToolsCommand() {
        this(new ConnectionFactory());
    }

    /**
     * @param factory the connection provider
     */
    public ToolsCommand(McpConnector factory) {
        this.factory = factory;
    }

    @Override
    public Integer call() {
        PrintWriter out = spec.commandLine().getOut();
        PrintWriter err = spec.commandLine().getErr();
        if (cached) {
            return callCached(out, err);
        }
        return callLive(out, err);
    }

    /**
     * Renders the bundled generated copy of
     * {@code shaft-mcp/.../META-INF/shaft-mcp/tool-index.json} without opening an MCP connection.
     */
    private Integer callCached(PrintWriter out, PrintWriter err) {
        Optional<JsonNode> loaded = ToolCatalog.readCached(err);
        if (loaded.isEmpty()) {
            return 1;
        }
        return ToolCatalog.renderCached(loaded.get(), out, err, json, compact, group);
    }

    private Integer callLive(PrintWriter out, PrintWriter err) {
        try (McpClient client = factory.connect(false, true)) {
            if (json && group.isBlank()) {
                out.println(Json.MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(client.listToolsRaw()));
                out.flush();
                return 0;
            }
            if (!json && !compact && group.isBlank()) {
                writeLegacyLive(client, out);
                return 0;
            }
            return ToolCatalog.renderLive(client, out, err, json, compact, group);
        }
    }

    private void writeLegacyLive(McpClient client, PrintWriter out) {
        for (Tool tool : client.listTools()) {
            out.println(tool.name() + " — " + firstSentence(tool.description()));
        }
        out.flush();
    }

    static String firstSentence(String description) {
        if (description == null || description.isBlank()) {
            return "";
        }
        int period = description.indexOf(". ");
        int newline = description.indexOf('\n');
        int cut = -1;
        if (period >= 0) {
            cut = period + 1;
        }
        if (newline >= 0 && (cut < 0 || newline < cut)) {
            cut = newline;
        }
        return (cut > 0 ? description.substring(0, cut) : description).strip();
    }
}
