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
 * (issues #5967 / S3-01, #5968 / S3-02, #5969 / S3-03, #5972 / S3-06, #5974 / S3-08).
 */
@Command(mixinStandardHelpOptions = true,
        name = "report",
        description = "Reporting shortcuts: history, flake, clusters, heal, mute, unmute, mutes.")
public final class ReportCommand implements Callable<Integer> {

    private static final Map<String, String> ACTIONS = Map.ofEntries(
            Map.entry("history", "report_history"),
            Map.entry("flake", "report_flake"),
            Map.entry("clusters", "report_clusters"),
            Map.entry("heal", "report_heal"),
            Map.entry("mute", "report_mute"),
            Map.entry("unmute", "report_mute"),
            Map.entry("mutes", "report_mute"));

    @Parameters(index = "0", paramLabel = "ACTION", description = "history, flake, clusters, heal, mute, unmute, mutes")
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
        List<String> args = new ArrayList<>(keyValues);
        String normalized = action == null ? "" : action.trim().toLowerCase(java.util.Locale.ROOT);
        if ("mute".equals(normalized) || "unmute".equals(normalized) || "mutes".equals(normalized)) {
            String muteAction = "mutes".equals(normalized) ? "list" : normalized;
            if (args.stream().noneMatch(v -> v != null && v.startsWith("action="))) {
                args.add(0, "action=" + muteAction);
            }
        }
        return ToolInvoker.invoke(factory, tool, options, args, false,
                spec.commandLine().getOut(), spec.commandLine().getErr());
    }
}
