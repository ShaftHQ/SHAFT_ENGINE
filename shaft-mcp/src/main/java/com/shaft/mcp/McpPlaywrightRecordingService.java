package com.shaft.mcp;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Local MCP recorder for Playwright actions performed through the Playwright tool surface.
 */
final class McpPlaywrightRecordingService {
    private final ObjectMapper mapper = new ObjectMapper();
    private final McpWorkspacePolicy workspacePolicy;
    private Path outputPath;
    private McpMobileRecording recording;

    McpPlaywrightRecordingService(McpWorkspacePolicy workspacePolicy) {
        this.workspacePolicy = workspacePolicy;
    }

    synchronized McpMobileRecordingStatus start(String outputPath, String mode, boolean includeSensitiveValues) {
        if (recording != null) {
            throw new IllegalStateException("A Playwright recording is already active.");
        }
        Path output = outputPath == null || outputPath.isBlank()
                ? workspacePolicy.output("recordings/playwright-" + Instant.now().toString().replace(':', '-') + ".json",
                "Playwright recording output path")
                : workspacePolicy.output(outputPath, "Playwright recording output path");
        this.outputPath = output;
        this.recording = new McpMobileRecording(
                McpMobileRecording.CURRENT_SCHEMA_VERSION,
                mode == null || mode.isBlank() ? "playwright" : mode,
                Instant.now().toString(),
                "",
                includeSensitiveValues,
                List.of(),
                List.of());
        persist();
        return status();
    }

    synchronized McpMobileRecordingStatus status() {
        if (recording == null) {
            return new McpMobileRecordingStatus(false, outputPath, "", 0, false, List.of());
        }
        return new McpMobileRecordingStatus(
                true,
                outputPath,
                recording.mode(),
                recording.actions().size(),
                recording.includeSensitiveValues(),
                recording.warnings());
    }

    synchronized McpMobileRecordingStatus stop(boolean discard) {
        if (recording == null) {
            return new McpMobileRecordingStatus(false, outputPath, "", 0, false,
                    List.of("No active Playwright recording."));
        }
        McpMobileRecording closed = new McpMobileRecording(
                recording.schemaVersion(),
                recording.mode(),
                recording.startedAt(),
                Instant.now().toString(),
                recording.includeSensitiveValues(),
                recording.actions(),
                recording.warnings());
        int count = closed.actions().size();
        Path finalPath = outputPath;
        if (!discard) {
            recording = closed;
            persist();
        } else if (outputPath != null) {
            try {
                Files.deleteIfExists(outputPath);
            } catch (IOException exception) {
                throw new IllegalArgumentException("Playwright recording output could not be discarded.", exception);
            }
        }
        recording = null;
        outputPath = null;
        return new McpMobileRecordingStatus(false, finalPath, closed.mode(), count, closed.includeSensitiveValues(),
                discard ? List.of("Recording discarded.") : closed.warnings());
    }

    synchronized McpMobileRecordedAction record(
            String action,
            locatorStrategy locatorStrategy,
            String locatorValue,
            Map<String, String> parameters,
            String javaCode,
            String redactedJavaCode,
            boolean sensitive) {
        if (recording == null) {
            return null;
        }
        Map<String, String> safeParameters = parameters == null ? Map.of() : parameters;
        List<String> warnings = new ArrayList<>();
        boolean sensitiveStored = !sensitive || recording.includeSensitiveValues();
        if (sensitive && !recording.includeSensitiveValues()) {
            safeParameters = Map.of("value", "<redacted>");
            warnings.add("Sensitive value omitted; replace the placeholder before replay.");
        }
        McpMobileRecordedAction recorded = new McpMobileRecordedAction(
                recording.actions().size() + 1L,
                Instant.now().toString(),
                action,
                locatorStrategy == null ? "" : locatorStrategy.name(),
                locatorValue,
                safeParameters,
                sensitiveStored ? javaCode : redactedJavaCode,
                sensitiveStored,
                warnings);
        List<McpMobileRecordedAction> actions = new ArrayList<>(recording.actions());
        actions.add(recorded);
        recording = new McpMobileRecording(
                recording.schemaVersion(),
                recording.mode(),
                recording.startedAt(),
                recording.stoppedAt(),
                recording.includeSensitiveValues(),
                actions,
                recording.warnings());
        persist();
        return recorded;
    }

    McpMobileReplayResult codeBlocks(String recordingPath, String driverVariableName) {
        Path path = workspacePolicy.existing(recordingPath, "Playwright recording path");
        McpMobileRecording stored = read(path);
        return new McpMobileReplayResult(path, true, 0, List.of(replayBlock(stored, driverVariableName)),
                replayWarnings(stored));
    }

    McpMobileRecording readRecording(String recordingPath) {
        return read(workspacePolicy.existing(recordingPath, "Playwright recording path"));
    }

    private McpMobileRecording read(Path path) {
        try {
            return mapper.readValue(path.toFile(), McpMobileRecording.class);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Playwright recording could not be read.", exception);
        }
    }

    private void persist() {
        try {
            Path parent = outputPath.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            mapper.writerWithDefaultPrettyPrinter().writeValue(outputPath.toFile(), recording);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Playwright recording could not be written.", exception);
        }
    }

    private McpCodeBlock replayBlock(McpMobileRecording stored, String driverVariableName) {
        String driver = javaIdentifierOrDefault(driverVariableName, "driver");
        StringBuilder code = new StringBuilder();
        code.append("public void replayPlaywrightJourney(SHAFT.GUI.Playwright ").append(driver).append(") {\n");
        for (McpMobileRecordedAction action : stored.actions()) {
            if (!action.warnings().isEmpty()) {
                code.append("    // ").append(String.join(" ", action.warnings())).append('\n');
            }
            for (String line : action.javaCode().replace("driver.", driver + ".").split("\\R")) {
                code.append("    ").append(line).append('\n');
            }
        }
        code.append("}\n");
        return new McpCodeBlock(
                "playwright-replay-method",
                "Generated Playwright replay method",
                McpCodeBlock.Kind.TEST_METHOD,
                "java",
                List.of("com.shaft.driver.SHAFT", "org.openqa.selenium.By", "org.openqa.selenium.WindowType"),
                code.toString(),
                "Paste into any class and call it with an initialized SHAFT.GUI.Playwright.",
                replayWarnings(stored).isEmpty(),
                List.of(),
                replayWarnings(stored));
    }

    private static List<String> replayWarnings(McpMobileRecording stored) {
        List<String> warnings = new ArrayList<>(stored.warnings());
        boolean redacted = stored.actions().stream().anyMatch(action -> !action.sensitiveValueStored());
        if (redacted) {
            warnings.add("One or more sensitive values were redacted; replace placeholders before replay.");
        }
        return List.copyOf(warnings);
    }

    private static String javaIdentifierOrDefault(String value, String fallback) {
        String candidate = value == null || value.isBlank() ? fallback : value.trim();
        if (!Character.isJavaIdentifierStart(candidate.charAt(0))) {
            return fallback;
        }
        for (int index = 1; index < candidate.length(); index++) {
            if (!Character.isJavaIdentifierPart(candidate.charAt(index))) {
                return fallback;
            }
        }
        return candidate;
    }
}
