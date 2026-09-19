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
 * Curated Design-stage shortcuts. Pure alias over {@code call}.
 */
@Command(mixinStandardHelpOptions = true,
        name = "design", description = "Design shortcuts: ingest | analyze | gherkin.")
public final class DesignCommand implements Callable<Integer> {

    private static final Map<String, String> ACTIONS = Map.of(
            "ingest", "design_ingest",
            "analyze", "design_analyze",
            "gherkin", "design_gherkin_draft");

    @Parameters(index = "0", paramLabel = "ACTION", description = "ingest | analyze | gherkin")
    private String action;

    @Parameters(index = "1..*", paramLabel = "key=value", description = "Arguments as key=value pairs, e.g. text=\"As a shopper…\".")
    private List<String> keyValues = new ArrayList<>();

    @Mixin
    private ToolOptions options;

    @Spec
    private CommandSpec spec;

    private final McpConnector factory;

    /** Uses the real connection factory. */
    public DesignCommand() {
        this(new ConnectionFactory());
    }

    /** @param factory the connection factory */
    public DesignCommand(McpConnector factory) {
        this.factory = factory;
    }

    @Override
    public Integer call() {
        String tool = AliasSupport.resolve(ACTIONS, action, "design", spec.commandLine().getErr());
        if (tool == null) {
            return 2;
        }
        return ToolInvoker.invoke(factory, tool, options, keyValues, false,
                spec.commandLine().getOut(), spec.commandLine().getErr());
    }
}
