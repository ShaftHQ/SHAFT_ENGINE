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
 * Curated guide shortcut. Pure alias over {@code call}; stateless (runs one-shot without a session).
 */
@Command(mixinStandardHelpOptions = true,
        name = "guide", description = "Guide shortcut: search.")
public final class GuideCommand implements Callable<Integer> {

    private static final Map<String, String> ACTIONS = Map.of("search", "shaft_guide_search");

    @Parameters(index = "0", paramLabel = "ACTION", description = "search")
    private String action;

    @Parameters(index = "1..*", paramLabel = "key=value", description = "Arguments as key=value pairs, e.g. query=\"click element\".")
    private List<String> keyValues = new ArrayList<>();

    @Mixin
    private ToolOptions options;

    @Spec
    private CommandSpec spec;

    private final McpConnector factory;

    /** Uses the real connection factory. */
    public GuideCommand() {
        this(new ConnectionFactory());
    }

    /** @param factory the connection factory */
    public GuideCommand(McpConnector factory) {
        this.factory = factory;
    }

    @Override
    public Integer call() {
        String tool = AliasSupport.resolve(ACTIONS, action, "guide", spec.commandLine().getErr());
        if (tool == null) {
            return 2;
        }
        return ToolInvoker.invoke(factory, tool, options, keyValues, false,
                spec.commandLine().getOut(), spec.commandLine().getErr());
    }
}
