package com.shaft.commandline;

import org.junit.jupiter.api.Test;
import picocli.CommandLine;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Model.OptionSpec;
import picocli.CommandLine.Model.PositionalParamSpec;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * #7007: CLI interface contract. Every command path with its options (names, arity,
 * required) and positional parameters is snapshotted; a flag removal or rename fails
 * until the snapshot is regenerated on purpose with {@code -Dshaft.contract.update=true}.
 * Exit codes for help and usage errors are pinned separately.
 */
class CliInterfaceContractTest {
    static final Path SNAPSHOT = Path.of("src/test/resources/contracts/cli-interface.txt");

    @Test
    void commandTreeMatchesTheGoldenSnapshot() throws Exception {
        String actual = describe(new CommandLine(new ShaftCli()));
        if (Boolean.getBoolean("shaft.contract.update")) {
            Files.writeString(SNAPSHOT, actual, StandardCharsets.UTF_8);
        }
        assertTrue(Files.exists(SNAPSHOT), "missing " + SNAPSHOT + "; generate with -Dshaft.contract.update=true");
        assertEquals(Files.readString(SNAPSHOT, StandardCharsets.UTF_8), actual,
                "CLI interface changed. If intended, regenerate " + SNAPSHOT
                        + " with -Dshaft.contract.update=true and label a breaking change.");
    }

    @Test
    void helpExitsZeroAndUsageErrorsExitTwo() {
        assertEquals(CommandLine.ExitCode.OK, run("--help"));
        assertEquals(CommandLine.ExitCode.USAGE, run("--definitely-not-an-option"));
        assertEquals(CommandLine.ExitCode.USAGE, run("definitely-not-a-command"));
    }

    private static int run(String... args) {
        StringWriter sink = new StringWriter();
        return new CommandLine(new ShaftCli()).setOut(new PrintWriter(sink)).setErr(new PrintWriter(sink)).execute(args);
    }

    static String describe(CommandLine root) {
        List<String> lines = new ArrayList<>();
        walk(root, lines);
        return String.join("\n", lines) + "\n";
    }

    private static void walk(CommandLine command, List<String> lines) {
        CommandSpec spec = command.getCommandSpec();
        lines.add(spec.qualifiedName(" "));
        Map<String, String> options = new TreeMap<>();
        for (OptionSpec option : spec.options()) {
            options.put(String.join(",", option.names()),
                    "arity=" + option.arity() + (option.required() ? " required" : ""));
        }
        options.forEach((names, detail) -> lines.add("  option " + names + " " + detail));
        for (PositionalParamSpec parameter : spec.positionalParameters()) {
            lines.add("  param " + parameter.index() + " " + parameter.paramLabel() + " arity=" + parameter.arity()
                    + (parameter.required() ? " required" : ""));
        }
        new TreeMap<>(command.getSubcommands()).forEach((name, sub) -> {
            if (name.equals(sub.getCommandName())) {
                walk(sub, lines);
            }
        });
    }
}
