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
 * Curated doctor shortcuts. Pure alias over {@code call}; stateless (runs one-shot without a session).
 */
@Command(mixinStandardHelpOptions = true,
        name = "doctor", description = "Doctor shortcuts: analyze | suggest.")
public final class DoctorCommand implements Callable<Integer> {

    private static final Map<String, String> ACTIONS = Map.of(
            "analyze", "doctor_analyze_trace",
            "suggest", "doctor_suggest_fix");

    @Parameters(index = "0", paramLabel = "ACTION", description = "analyze | suggest")
    private String action;

    @Parameters(index = "1..*", paramLabel = "key=value", description = "Arguments as key=value pairs.")
    private List<String> keyValues = new ArrayList<>();

    @Mixin
    private ToolOptions options;

    @Spec
    private CommandSpec spec;

    private final McpConnector factory;

    /** Uses the real connection factory. */
    public DoctorCommand() {
        this(new ConnectionFactory());
    }

    /** @param factory the connection factory */
    public DoctorCommand(McpConnector factory) {
        this.factory = factory;
    }

    @Override
    public Integer call() {
        String tool = AliasSupport.resolve(ACTIONS, action, "doctor", spec.commandLine().getErr());
        if (tool == null) {
            return 2;
        }
        return ToolInvoker.invoke(factory, tool, options, keyValues, false,
                spec.commandLine().getOut(), spec.commandLine().getErr());
    }
}
