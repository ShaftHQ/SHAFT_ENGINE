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
 * Curated capture shortcuts. Pure alias over {@code call}; requires a live session.
 */
@Command(mixinStandardHelpOptions = true,
        name = "capture",
        description = "Capture shortcuts: start | stop | status | code | step-delete | step-reorder.")
public final class CaptureCommand implements Callable<Integer> {

    private static final Map<String, String> ACTIONS = Map.of(
            "start", "capture_start",
            "stop", "capture_stop",
            "status", "capture_status",
            "code", "capture_code_blocks",
            "step-delete", "capture_step_delete",
            "step-reorder", "capture_step_reorder");

    @Parameters(index = "0", paramLabel = "ACTION",
            description = "start | stop | status | code | step-delete | step-reorder")
    private String action;

    @Parameters(index = "1..*", paramLabel = "key=value", description = "Arguments as key=value pairs.")
    private List<String> keyValues = new ArrayList<>();

    @Mixin
    private ToolOptions options;

    @Spec
    private CommandSpec spec;

    private final McpConnector factory;

    /** Uses the real connection factory. */
    public CaptureCommand() {
        this(new ConnectionFactory());
    }

    /** @param factory the connection factory */
    public CaptureCommand(McpConnector factory) {
        this.factory = factory;
    }

    @Override
    public Integer call() {
        String tool = AliasSupport.resolve(ACTIONS, action, "capture", spec.commandLine().getErr());
        if (tool == null) {
            return 2;
        }
        return ToolInvoker.invoke(factory, tool, options, keyValues, true,
                spec.commandLine().getOut(), spec.commandLine().getErr());
    }
}
