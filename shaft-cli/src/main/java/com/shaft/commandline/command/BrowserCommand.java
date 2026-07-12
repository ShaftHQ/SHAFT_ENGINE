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
 * Curated browser shortcuts. Pure alias over {@code call}; requires a live session.
 */
@Command(mixinStandardHelpOptions = true,
        name = "browser", description = "Browser shortcuts: navigate | screenshot | dom | url.")
public final class BrowserCommand implements Callable<Integer> {

    private static final Map<String, String> ACTIONS = Map.of(
            "navigate", "browser_navigate",
            "screenshot", "browser_take_screenshot",
            "dom", "browser_get_page_dom",
            "url", "browser_get_current_url");

    @Parameters(index = "0", paramLabel = "ACTION", description = "navigate | screenshot | dom | url")
    private String action;

    @Parameters(index = "1..*", paramLabel = "key=value", description = "Arguments as key=value pairs.")
    private List<String> keyValues = new ArrayList<>();

    @Mixin
    private ToolOptions options;

    @Spec
    private CommandSpec spec;

    private final McpConnector factory;

    /** Uses the real connection factory. */
    public BrowserCommand() {
        this(new ConnectionFactory());
    }

    /** @param factory the connection factory */
    public BrowserCommand(McpConnector factory) {
        this.factory = factory;
    }

    @Override
    public Integer call() {
        String tool = AliasSupport.resolve(ACTIONS, action, "browser", spec.commandLine().getErr());
        if (tool == null) {
            return 2;
        }
        return ToolInvoker.invoke(factory, tool, options, keyValues, true,
                spec.commandLine().getOut(), spec.commandLine().getErr());
    }
}
