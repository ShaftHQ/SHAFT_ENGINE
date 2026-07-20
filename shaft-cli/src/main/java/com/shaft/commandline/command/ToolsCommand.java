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

import java.io.InputStream;
import java.io.PrintWriter;
import java.util.concurrent.Callable;

/**
 * Lists the tools shaft-mcp exposes via {@code tools/list}.
 */
@Command(mixinStandardHelpOptions = true,
        name = "tools", description = "List the tools exposed by shaft-mcp.")
public final class ToolsCommand implements Callable<Integer> {

    /** Classpath location of the offline catalog snapshot read by {@code --cached}. */
    static final String CACHED_INDEX_RESOURCE = "/tool-index-cache.json";

    @Option(names = "--json", description = "Print the raw tools/list result.")
    private boolean json;

    @Option(names = "--cached",
            description = "List the tool catalog from the bundled offline snapshot, without connecting "
                    + "to a live shaft-mcp server. Faster, but only as fresh as the last snapshot refresh "
                    + "(see the snapshot's own \"note\" field).")
    private boolean cached;

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
        if (cached) {
            return callCached(out);
        }
        try (McpClient client = factory.connect(false, true)) {
            if (json) {
                out.println(Json.MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(client.listToolsRaw()));
                out.flush();
                return 0;
            }
            for (Tool tool : client.listTools()) {
                out.println(tool.name() + " — " + firstSentence(tool.description()));
            }
            out.flush();
            return 0;
        }
    }

    /**
     * Renders the bundled offline catalog snapshot ({@link #CACHED_INDEX_RESOURCE}) without opening
     * an MCP connection. This is a stop-gap over T2/#3868's canonical
     * {@code shaft-mcp/.../META-INF/shaft-mcp/tool-index.json}: see the snapshot's own {@code note}
     * field for the follow-up.
     */
    private Integer callCached(PrintWriter out) {
        PrintWriter err = spec.commandLine().getErr();
        JsonNode index;
        try (InputStream stream = ToolsCommand.class.getResourceAsStream(CACHED_INDEX_RESOURCE)) {
            if (stream == null) {
                err.println("Cached tool index not found on the classpath: " + CACHED_INDEX_RESOURCE);
                err.flush();
                return 1;
            }
            index = Json.MAPPER.readTree(stream);
        } catch (java.io.IOException exception) {
            err.println("Cached tool index could not be read: " + exception.getMessage());
            err.flush();
            return 1;
        } catch (RuntimeException exception) {
            err.println("Cached tool index is not valid JSON: " + exception.getMessage());
            err.flush();
            return 1;
        }
        if (json) {
            out.println(Json.MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(index));
            out.flush();
            return 0;
        }
        for (JsonNode tool : index.path("tools")) {
            out.println(tool.path("name").asText("") + " — " + firstSentence(tool.path("description").asText("")));
        }
        out.flush();
        return 0;
    }

    private static String firstSentence(String description) {
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
