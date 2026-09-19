package com.shaft.commandline.command;

import com.shaft.commandline.runtime.ConnectionFactory;
import com.shaft.commandline.runtime.McpConnector;
import com.shaft.commandline.runtime.ToolInvoker;
import com.shaft.commandline.runtime.ToolOptions;
import picocli.CommandLine.Command;
import picocli.CommandLine.Mixin;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Parameters;
import picocli.CommandLine.Spec;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

/**
 * Curated Reporting-stage shortcuts. Pure alias over {@code call} to MCP {@code report_*} tools
 * (issues #5967 / S3-01, #5968 / S3-02).
 */
@Command(mixinStandardHelpOptions = true,
        name = "report",
        description = "Reporting shortcuts: history, flake.")
public final class ReportCommand implements Callable<Integer> {

    private static final Map<String, String> ACTIONS = Map.of(
            "history", "report_history",
            "flake", "report_flake");

    @Parameters(index = "0", paramLabel = "ACTION", description = "history, flake")
    private String action;

    @Parameters(index = "1..*", paramLabel = "key=value",
            description = "Arguments as key=value pairs, e.g. historyPath=target/history.jsonl.")
    private List<String> keyValues = new ArrayList<>();

    @Mixin
    private ToolOptions options;

    @Spec
    private CommandSpec spec;

    private final McpConnector factory;

    /** Uses the real connection factory. */
    public ReportCommand() {
        this(new ConnectionFactory());
    }

    /** @param factory the connection factory */
    public ReportCommand(McpConnector factory) {
        this.factory = factory;
    }

    @Override
    public Integer call() {
        String tool = AliasSupport.resolve(ACTIONS, action, "report", spec.commandLine().getErr());
        if (tool == null) {
            return 2;
        }
        return ToolInvoker.invoke(factory, tool, options, keyValues, false,
                spec.commandLine().getOut(), spec.commandLine().getErr());
    }
}
