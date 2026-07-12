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

import java.io.PrintWriter;
import java.util.concurrent.Callable;

/**
 * Lists the tools shaft-mcp exposes via {@code tools/list}.
 */
@Command(mixinStandardHelpOptions = true,
        name = "tools", description = "List the tools exposed by shaft-mcp.")
public final class ToolsCommand implements Callable<Integer> {

    @Option(names = "--json", description = "Print the raw tools/list result.")
    private boolean json;

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
