package com.shaft.commandline.command;

import picocli.CommandLine.Command;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;
import picocli.CommandLine.Spec;

import java.util.concurrent.Callable;

/**
 * Prints one tool from the bundled tool index. No shaft-mcp server is contacted.
 */
@Command(mixinStandardHelpOptions = true,
        name = "describe",
        description = "Print one shaft-mcp tool from the bundled index, with no server connection.")
public final class ToolsDescribeCommand implements Callable<Integer> {

    @Parameters(index = "0", paramLabel = "TOOL", description = "Tool name, for example capture_start.")
    private String toolName = "";

    @Option(names = "--json", description = "Print that tool's index object as JSON.")
    private boolean json;

    @Spec
    private CommandSpec spec = CommandSpec.create();

    /**
     * Prints the requested tool in text or JSON.
     *
     * @return 0 when the tool is printed, 2 when the name is unknown, 1 when the index cannot be read
     */
    @Override
    public Integer call() {
        return ToolCatalog.describe(
                spec.commandLine().getOut(), spec.commandLine().getErr(), toolName, json);
    }
}
