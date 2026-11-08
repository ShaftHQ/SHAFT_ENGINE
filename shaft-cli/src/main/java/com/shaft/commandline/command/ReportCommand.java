package com.shaft.commandline.command;

import com.shaft.commandline.runtime.ConnectionFactory;
import com.shaft.commandline.runtime.McpConnector;
import com.shaft.commandline.runtime.ToolInvoker;
import com.shaft.commandline.runtime.ToolOptions;
import com.shaft.driver.SHAFT;
import picocli.CommandLine.Command;
import picocli.CommandLine.Mixin;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;
import picocli.CommandLine.Spec;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.Callable;

/**
 * Curated Reporting-stage shortcuts. Most actions alias MCP {@code report_*} tools
 * (issues #5967 / S3-01, #5968 / S3-02, #5969 / S3-03, #5972 / S3-06, #5974 / S3-08, #5975 / S3-09).
 * {@code generate} rebuilds the Allure report locally from an existing results directory (#7837).
 */
@Command(mixinStandardHelpOptions = true,
        name = "report",
        description = "Reporting shortcuts: generate, open, summary, history, flake, tags, clusters, heal, mute, unmute, mutes. "
                + "generate rebuilds the single-file Allure report from an existing results directory "
                + "inside this shaded jar, so Windows does not put the test classpath on the java command line.")
public final class ReportCommand implements Callable<Integer> {

    private static final Map<String, String> ACTIONS = Map.ofEntries(
            Map.entry("open", "report_open"),
            Map.entry("summary", "report_summary"),
            Map.entry("history", "report_history"),
            Map.entry("flake", "report_flake"),
            Map.entry("tags", "report_smart_tags"),
            Map.entry("clusters", "report_clusters"),
            Map.entry("heal", "report_heal"),
            Map.entry("mute", "report_mute"),
            Map.entry("unmute", "report_mute"),
            Map.entry("mutes", "report_mute"));

    @Parameters(index = "0", paramLabel = "ACTION",
            description = "generate, open, summary, history, flake, tags, clusters, heal, mute, unmute, mutes")
    private String action;

    @Parameters(index = "1..*", paramLabel = "key=value",
            description = "Arguments as key=value pairs, e.g. historyPath=target/history.jsonl or results=target/allure-results.")
    private List<String> keyValues = new ArrayList<>();

    @Option(names = "--results",
            description = "Existing allure-results directory for generate. The report is written beside it.")
    private Path results;

    @Option(names = "--single-file",
            description = "Force allure.singleFile=true for generate. The default SHAFT report is already one HTML file.")
    private boolean singleFile;

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
        if ("generate".equals(normalizedAction())) {
            return generateFromResults();
        }
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

    private String normalizedAction() {
        return action == null ? "" : action.trim().toLowerCase(java.util.Locale.ROOT);
    }

    /**
     * Resolves the results directory for {@code report generate} from {@code --results}
     * or a {@code results=} / {@code resultsDirectory=} argument.
     *
     * @return the directory, or empty when the caller did not name one
     */
    Optional<Path> resultsDirectory() {
        if (results != null) {
            return Optional.of(results);
        }
        for (String entry : keyValues) {
            int separator = entry.indexOf('=');
            if (separator <= 0) {
                continue;
            }
            String key = entry.substring(0, separator).trim();
            if (!"results".equals(key) && !"resultsDirectory".equals(key)) {
                continue;
            }
            String value = entry.substring(separator + 1).trim();
            if (value.isEmpty()) {
                continue;
            }
            return Optional.of(Path.of(value));
        }
        return Optional.empty();
    }

    private int generateFromResults() {
        Optional<Path> directory = resultsDirectory();
        if (directory.isEmpty()) {
            spec.commandLine().getErr().println("report generate requires --results <allure-results-directory>");
            return 2;
        }
        if (singleFile) {
            System.setProperty("allure.singleFile", "true");
        }
        try {
            Path report = SHAFT.Report.generateAllureReport(directory.get());
            spec.commandLine().getOut().println(report.toAbsolutePath().normalize());
            return 0;
        } catch (IllegalArgumentException | IllegalStateException failure) {
            spec.commandLine().getErr().println(failure.getMessage());
            return 1;
        }
    }
}
