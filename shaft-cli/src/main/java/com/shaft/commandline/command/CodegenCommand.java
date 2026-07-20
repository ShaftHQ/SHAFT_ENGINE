package com.shaft.commandline.command;

import com.shaft.capture.cli.CaptureCli;
import com.shaft.commandline.runtime.CaptureDelegate;
import picocli.CommandLine.Command;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Spec;
import picocli.CommandLine.Unmatched;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.stream.Stream;

/**
 * Deterministic file-to-code codegen: delegates to the standalone {@code CaptureCli generate}
 * engine (no MCP session, no server -- Decision 6, CLI-first for single-session actions).
 * {@code --session} defaults to the most recently modified recording under {@code ./recordings}
 * when omitted, mirroring {@code capture_generate_replay}'s zero-arg default
 * ({@code CaptureService#resolveSessionPath}) so this also runs with minimal required flags.
 * Every other {@code CaptureCli generate} flag (see {@code CaptureCli}'s usage text) passes
 * through unchanged.
 */
@Command(mixinStandardHelpOptions = true, name = "codegen",
        description = "Generate SHAFT test code from a capture recording (deterministic, no MCP session).")
public final class CodegenCommand implements Callable<Integer> {

    @Unmatched
    private List<String> passthrough = new ArrayList<>();

    @Spec
    private CommandSpec spec;

    private final CaptureDelegate delegate;
    private final Path recordingsDir;

    /**
     * Uses the real {@code CaptureCli} engine and {@code ./recordings}.
     */
    public CodegenCommand() {
        this(CaptureCli::run, Path.of("recordings"));
    }

    /**
     * @param delegate      the capture engine seam
     * @param recordingsDir directory scanned for the newest recording when {@code --session} is omitted
     */
    CodegenCommand(CaptureDelegate delegate, Path recordingsDir) {
        this.delegate = delegate;
        this.recordingsDir = recordingsDir;
    }

    @Override
    public Integer call() {
        List<String> args = new ArrayList<>();
        args.add("generate");
        args.addAll(passthrough);
        if (passthrough.stream().noneMatch(CodegenCommand::isSessionToken)) {
            Path newest = newestRecording(recordingsDir);
            if (newest == null) {
                PrintWriter err = spec.commandLine().getErr();
                err.println("shaft-cli codegen: no --session given and no recordings found under "
                        + recordingsDir + "; pass --session <path> explicitly.");
                err.flush();
                return 2;
            }
            args.add("--session");
            args.add(newest.toString());
        }
        return delegate.run(args.toArray(new String[0]), List.of());
    }

    private static boolean isSessionToken(String token) {
        return "--session".equals(token) || token.startsWith("--session=");
    }

    private static Path newestRecording(Path dir) {
        if (!Files.isDirectory(dir)) {
            return null;
        }
        try (Stream<Path> files = Files.list(dir)) {
            return files.filter(file -> file.getFileName().toString().endsWith(".json"))
                    .filter(Files::isRegularFile)
                    .max(Comparator.comparing(CodegenCommand::lastModified))
                    .orElse(null);
        } catch (IOException exception) {
            return null;
        }
    }

    private static FileTime lastModified(Path file) {
        try {
            return Files.getLastModifiedTime(file);
        } catch (IOException exception) {
            return FileTime.fromMillis(0);
        }
    }
}
