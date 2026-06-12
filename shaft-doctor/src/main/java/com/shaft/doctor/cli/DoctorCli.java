package com.shaft.doctor.cli;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.shaft.doctor.DoctorAnalysisRequest;
import com.shaft.doctor.DoctorAnalyzer;
import com.shaft.doctor.model.DoctorAnalysisResult;

import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * Executable local CLI for SHAFT Doctor.
 */
public final class DoctorCli {
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);
    private static final PrintWriter OUTPUT = new PrintWriter(
            new OutputStreamWriter(new FileOutputStream(FileDescriptor.out), StandardCharsets.UTF_8), true);
    private static final PrintWriter ERROR = new PrintWriter(
            new OutputStreamWriter(new FileOutputStream(FileDescriptor.err), StandardCharsets.UTF_8), true);

    private DoctorCli() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * CLI entry point.
     *
     * @param args Doctor command arguments
     */
    public static void main(String[] args) {
        int exitCode = run(args);
        if (exitCode != 0) {
            System.exit(exitCode);
        }
    }

    /**
     * Runs a Doctor command.
     *
     * @param args command arguments
     * @return process-style exit code
     */
    public static int run(String[] args) {
        return run(args, OUTPUT, ERROR);
    }

    /**
     * Runs a Doctor command with explicit output streams.
     *
     * @param args command arguments
     * @param output command output
     * @param error command error output
     * @return process-style exit code
     */
    public static int run(String[] args, PrintWriter output, PrintWriter error) {
        try {
            if (args == null || args.length == 0 || !"analyze".equalsIgnoreCase(args[0])) {
                throw new IllegalArgumentException(usage());
            }
            Arguments options = Arguments.parse(Arrays.copyOfRange(args, 1, args.length));
            DoctorAnalysisResult result = new DoctorAnalyzer().analyze(new DoctorAnalysisRequest(
                    options.pathsRequired("input"),
                    options.paths("history"),
                    options.pathsRequired("allowed-root"),
                    options.path("output-dir", Path.of("target", "shaft-doctor")),
                    options.flag("include-screenshots"),
                    options.flag("include-page-snapshots"),
                    Math.toIntExact(options.positiveLong("minimum-results", 1)),
                    options.positiveLong("max-item-bytes", DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES),
                    options.positiveLong("max-bundle-bytes", DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES)));
            output.println(MAPPER.writeValueAsString(Map.of(
                    "bundleId", result.bundle().bundleId(),
                    "primaryCause", result.diagnosis().primaryCause(),
                    "confidence", result.diagnosis().confidence(),
                    "bundlePath", result.bundlePath(),
                    "jsonReportPath", result.jsonReportPath(),
                    "markdownReportPath", result.markdownReportPath())));
            return 0;
        } catch (RuntimeException exception) {
            error.println("SHAFT Doctor command failed: " + safeMessage(exception));
            return 1;
        } catch (com.fasterxml.jackson.core.JsonProcessingException exception) {
            error.println("SHAFT Doctor command failed: result could not be serialized.");
            return 1;
        }
    }

    private static String safeMessage(RuntimeException exception) {
        String message = exception.getMessage();
        return message == null || message.isBlank() ? "operation failed." : message;
    }

    private static String usage() {
        return "Usage: doctor analyze --input <path> [--input <path> ...] "
                + "--allowed-root <path> [--allowed-root <path> ...] "
                + "[--history <doctor-evidence.json>] [--output-dir <path>] "
                + "[--include-screenshots] [--include-page-snapshots] "
                + "[--minimum-results <count>] "
                + "[--max-item-bytes <bytes>] [--max-bundle-bytes <bytes>]";
    }

    private record Arguments(Map<String, List<String>> values, Set<String> flags) {
        private static Arguments parse(String[] args) {
            Map<String, List<String>> values = new LinkedHashMap<>();
            Set<String> flags = new LinkedHashSet<>();
            for (int index = 0; index < args.length; index++) {
                String argument = args[index];
                if (!argument.startsWith("--")) {
                    throw new IllegalArgumentException("Unexpected Doctor argument.");
                }
                String name = argument.substring(2).toLowerCase(Locale.ROOT);
                if (Set.of("include-screenshots", "include-page-snapshots").contains(name)) {
                    flags.add(name);
                } else {
                    if (index + 1 >= args.length || args[index + 1].startsWith("--")) {
                        throw new IllegalArgumentException("Doctor option --" + name + " requires a value.");
                    }
                    values.computeIfAbsent(name, ignored -> new ArrayList<>()).add(args[++index]);
                }
            }
            Map<String, List<String>> copy = new LinkedHashMap<>();
            values.forEach((key, value) -> copy.put(key, List.copyOf(value)));
            return new Arguments(Map.copyOf(copy), Set.copyOf(flags));
        }

        private List<Path> pathsRequired(String name) {
            List<Path> paths = paths(name);
            if (paths.isEmpty()) {
                throw new IllegalArgumentException("Doctor option --" + name + " is required.");
            }
            return paths;
        }

        private List<Path> paths(String name) {
            return values.getOrDefault(name, List.of()).stream().map(Path::of).toList();
        }

        private Path path(String name, Path fallback) {
            List<String> options = values.getOrDefault(name, List.of());
            return options.isEmpty() ? fallback : Path.of(options.getLast());
        }

        private boolean flag(String name) {
            return flags.contains(name);
        }

        private long positiveLong(String name, long fallback) {
            List<String> options = values.getOrDefault(name, List.of());
            if (options.isEmpty()) {
                return fallback;
            }
            try {
                long parsed = Long.parseLong(options.getLast());
                if (parsed <= 0) {
                    throw new NumberFormatException();
                }
                return parsed;
            } catch (NumberFormatException exception) {
                throw new IllegalArgumentException("Doctor option --" + name
                        + " must be a positive integer.", exception);
            }
        }
    }
}
