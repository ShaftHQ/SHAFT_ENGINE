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
import java.util.concurrent.Callable;

/**
 * Generic invocation of any shaft-mcp tool by name. This is the parity surface: every one of the
 * server's tools is reachable here without a dedicated CLI command.
 */
@Command(mixinStandardHelpOptions = true,
        name = "call", description = "Invoke any shaft-mcp tool by name.")
public final class CallCommand implements Callable<Integer> {

    @Parameters(index = "0", paramLabel = "TOOL", description = "The tool name (see `shaft-cli tools`).")
    private String toolName;

    @Parameters(index = "1..*", paramLabel = "key=value", description = "Arguments as key=value pairs.")
    private List<String> keyValues = new ArrayList<>();

    @Mixin
    private ToolOptions options;

    @Spec
    private CommandSpec spec;

    private final McpConnector factory;

    /**
     * Uses the real connection factory.
     */
    public CallCommand() {
        this(new ConnectionFactory());
    }

    /**
     * @param factory the connection provider
     */
    public CallCommand(McpConnector factory) {
        this.factory = factory;
    }

    @Override
    public Integer call() {
        return ToolInvoker.invoke(factory, toolName, options, keyValues, false,
                spec.commandLine().getOut(), spec.commandLine().getErr());
    }
}
