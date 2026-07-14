package com.shaft.mcp;

import com.shaft.capture.model.CaptureReadiness;
import tools.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Local MCP recorder for mobile actions performed through the mobile tool surface.
 */
final class McpMobileRecordingService {
    private final ObjectMapper mapper = new ObjectMapper();
    private final McpWorkspacePolicy workspacePolicy;
    private Path outputPath;
    private McpMobileRecording recording;

    McpMobileRecordingService(McpWorkspacePolicy workspacePolicy) {
        this.workspacePolicy = workspacePolicy;
    }

    synchronized McpMobileRecordingStatus start(String outputPath, String mode, boolean includeSensitiveValues) {
        if (recording != null) {
            throw new IllegalStateException("A mobile recording is already active.");
        }
        Path output = outputPath == null || outputPath.isBlank()
                ? workspacePolicy.output("recordings/mobile-" + Instant.now().toString().replace(':', '-') + ".json",
                "Mobile recording output path")
                : workspacePolicy.output(outputPath, "Mobile recording output path");
        this.outputPath = output;
        this.recording = new McpMobileRecording(
                McpMobileRecording.CURRENT_SCHEMA_VERSION,
                mode == null || mode.isBlank() ? "mobile" : mode,
                Instant.now().toString(),
                "",
                includeSensitiveValues,
                List.of(),
                List.of(),
                1L);
        persist();
        return status();
    }

    synchronized McpMobileRecordingStatus status() {
        if (recording == null) {
            return new McpMobileRecordingStatus(false, outputPath, "", 0, false, List.of(),
                    CaptureReadiness.State.READY);
        }
        return new McpMobileRecordingStatus(
                true,
                outputPath,
                recording.mode(),
                recording.actions().size(),
                recording.includeSensitiveValues(),
                recording.warnings(),
                readinessFor(recording, false),
                stepSummaries(recording));
    }

    synchronized McpMobileRecordingStatus stop(boolean discard) {
        if (recording == null) {
            return new McpMobileRecordingStatus(false, outputPath, "", 0, false,
                    List.of("No active mobile recording."), CaptureReadiness.State.BLOCKED);
        }
        McpMobileRecording closed = new McpMobileRecording(
                recording.schemaVersion(),
                recording.mode(),
                recording.startedAt(),
                Instant.now().toString(),
                recording.includeSensitiveValues(),
                recording.actions(),
                recording.warnings(),
                recording.nextStepId());
        int count = closed.actions().size();
        Path finalPath = outputPath;
        if (!discard) {
            recording = closed;
            persist();
        } else if (outputPath != null) {
            try {
                Files.deleteIfExists(outputPath);
            } catch (IOException exception) {
                throw new IllegalArgumentException("Mobile recording output could not be discarded.", exception);
            }
        }
        recording = null;
        outputPath = null;
        CaptureReadiness.State readiness = discard
                ? CaptureReadiness.State.READY
                : readinessFor(closed, true);
        return new McpMobileRecordingStatus(false, finalPath, closed.mode(), count, closed.includeSensitiveValues(),
                discard ? List.of("Recording discarded.") : closed.warnings(), readiness, stepSummaries(closed));
    }

    /**
     * Deletes a recorded mobile step by its stable {@code stepId} and renumbers the remaining
     * steps' {@code sequence} to stay contiguous (1..N), so codegen (which iterates the action list
     * in order and derives {@code action-<sequence>} evidence ids) keeps working after the edit.
     * A missing recording or an unknown {@code stepId} is a no-op — the agent is expected to re-read
     * status and retry rather than have the recording silently corrupted or an exception thrown.
     *
     * @param stepId stable step id to remove, as surfaced in {@link McpMobileRecordingStatus#steps()}
     * @return updated recorder status
     */
    synchronized McpMobileRecordingStatus deleteStep(String stepId) {
        if (recording == null) {
            return new McpMobileRecordingStatus(false, outputPath, "", 0, false,
                    List.of("Ignored: no active mobile recording to edit."), CaptureReadiness.State.BLOCKED);
        }
        int index = indexOfStep(stepId);
        if (index < 0) {
            return status();
        }
        List<McpMobileRecordedAction> actions = new ArrayList<>(recording.actions());
        actions.remove(index);
        recording = new McpMobileRecording(
                recording.schemaVersion(),
                recording.mode(),
                recording.startedAt(),
                recording.stoppedAt(),
                recording.includeSensitiveValues(),
                renumber(actions),
                recording.warnings(),
                recording.nextStepId());
        persist();
        return status();
    }

    /**
     * Moves a recorded mobile step up or down by its stable {@code stepId}, then renumbers
     * {@code sequence} to stay contiguous (1..N). Moving the first step up, the last step down, an
     * unknown {@code stepId}, or an unrecognized {@code direction} are all no-ops.
     *
     * @param stepId stable step id to move, as surfaced in {@link McpMobileRecordingStatus#steps()}
     * @param direction "up" or "down" (case-insensitive)
     * @return updated recorder status
     */
    synchronized McpMobileRecordingStatus reorderStep(String stepId, String direction) {
        if (recording == null) {
            return new McpMobileRecordingStatus(false, outputPath, "", 0, false,
                    List.of("Ignored: no active mobile recording to edit."), CaptureReadiness.State.BLOCKED);
        }
        String normalizedDirection = direction == null ? "" : direction.trim().toLowerCase(Locale.ROOT);
        if (!normalizedDirection.equals("up") && !normalizedDirection.equals("down")) {
            return status();
        }
        int index = indexOfStep(stepId);
        if (index < 0) {
            return status();
        }
        int targetIndex = normalizedDirection.equals("up") ? index - 1 : index + 1;
        if (targetIndex < 0 || targetIndex >= recording.actions().size()) {
            return status();
        }
        List<McpMobileRecordedAction> actions = new ArrayList<>(recording.actions());
        Collections.swap(actions, index, targetIndex);
        recording = new McpMobileRecording(
                recording.schemaVersion(),
                recording.mode(),
                recording.startedAt(),
                recording.stoppedAt(),
                recording.includeSensitiveValues(),
                renumber(actions),
                recording.warnings(),
                recording.nextStepId());
        persist();
        return status();
    }

    private int indexOfStep(String stepId) {
        String targetId = stepId == null ? "" : stepId.trim();
        if (targetId.isBlank()) {
            return -1;
        }
        List<McpMobileRecordedAction> actions = recording.actions();
        for (int index = 0; index < actions.size(); index++) {
            if (targetId.equals(actions.get(index).stepId())) {
                return index;
            }
        }
        return -1;
    }

    private static List<McpMobileRecordedAction> renumber(List<McpMobileRecordedAction> actions) {
        List<McpMobileRecordedAction> renumbered = new ArrayList<>(actions.size());
        for (int index = 0; index < actions.size(); index++) {
            renumbered.add(withSequence(actions.get(index), index + 1L));
        }
        return List.copyOf(renumbered);
    }

    private static McpMobileRecordedAction withSequence(McpMobileRecordedAction action, long sequence) {
        return new McpMobileRecordedAction(
                action.stepId(),
                sequence,
                action.timestamp(),
                action.action(),
                action.locatorStrategy(),
                action.locatorValue(),
                action.parameters(),
                action.javaCode(),
                action.sensitiveValueStored(),
                action.warnings());
    }

    private static List<McpMobileStepSummary> stepSummaries(McpMobileRecording stored) {
        return stored.actions().stream()
                .map(action -> new McpMobileStepSummary(
                        action.stepId(),
                        action.sequence(),
                        action.action(),
                        action.locatorStrategy(),
                        action.locatorValue(),
                        !action.warnings().isEmpty(),
                        action.warnings()))
                .toList();
    }

    /**
     * Rolls a mobile recording's step warnings up into the shared Ready/Risky/Blocked verdict, so
     * the mobile status speaks the same readiness language as the web Capture recorder (#3497). A
     * stopped recording with no steps is {@code BLOCKED} (nothing to generate); coordinate-fallback
     * or redaction warnings on any step make it {@code RISKY}; a clean recording is {@code READY}.
     *
     * @param recording the live or just-closed recording
     * @param stopped whether the recording has been stopped
     * @return the readiness verdict
     */
    private static CaptureReadiness.State readinessFor(McpMobileRecording recording, boolean stopped) {
        if (stopped && recording.actions().isEmpty()) {
            return CaptureReadiness.State.BLOCKED;
        }
        return replayWarnings(recording).isEmpty()
                ? CaptureReadiness.State.READY
                : CaptureReadiness.State.RISKY;
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
        if (McpAppiumLocatorSuggester.isCoordinateFallback(action, locatorStrategy)) {
            warnings.add(McpAppiumLocatorSuggester.COORDINATE_FALLBACK_WARNING);
        }
        boolean sensitiveStored = !sensitive || recording.includeSensitiveValues();
        if (sensitive && !recording.includeSensitiveValues()) {
            safeParameters = Map.of("value", "<redacted>");
            warnings.add("Sensitive value omitted; this action requires manual value replacement before replay.");
        }
        String stepId = "m" + recording.nextStepId();
        McpMobileRecordedAction recorded = new McpMobileRecordedAction(
                stepId,
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
                recording.warnings(),
                recording.nextStepId() + 1);
        persist();
        return recorded;
    }

    synchronized void recordWarning(String warning) {
        if (recording == null || warning == null || warning.isBlank()) {
            return;
        }
        List<String> warnings = new ArrayList<>(recording.warnings());
        warnings.add(warning.trim());
        recording = new McpMobileRecording(
                recording.schemaVersion(),
                recording.mode(),
                recording.startedAt(),
                recording.stoppedAt(),
                recording.includeSensitiveValues(),
                recording.actions(),
                warnings,
                recording.nextStepId());
        persist();
    }

    McpMobileReplayResult codeBlocks(String recordingPath, String driverVariableName) {
        Path path = workspacePolicy.existing(recordingPath, "Mobile recording path");
        McpMobileRecording stored = read(path);
        return codeBlocks(path, stored, driverVariableName, null, "");
    }

    McpMobileReplayResult codeBlocks(
            String recordingPath,
            String driverVariableName,
            Path targetSource,
            String insertAfter) {
        Path path = workspacePolicy.existing(recordingPath, "Mobile recording path");
        McpMobileRecording stored = read(path);
        return codeBlocks(path, stored, driverVariableName, targetSource, insertAfter);
    }

    McpMobileRecording readRecording(String recordingPath) {
        return read(workspacePolicy.existing(recordingPath, "Mobile recording path"));
    }

    private McpMobileRecording read(Path path) {
        try {
            return normalize(mapper.readValue(path.toFile(), McpMobileRecording.class));
        } catch (RuntimeException exception) {
            throw new IllegalArgumentException("Mobile recording could not be read.", exception);
        }
    }

    /**
     * Backfills recordings persisted before schema 1.1 so step surgery ({@link #deleteStep(String)},
     * {@link #reorderStep(String, String)}) and codegen keep working on old files: Jackson leaves a
     * missing {@code stepId} as {@code ""} (coerced by the record's compact constructor) and a
     * missing {@code nextStepId} as {@code 0}, so any blank {@code stepId} is backfilled to
     * {@code "m" + sequence}, and {@code nextStepId} is raised above every numeric {@code stepId}
     * suffix in use (and above the action count) so newly recorded or mutated steps never collide
     * with, or reuse, a backfilled id.
     *
     * @param stored the recording as deserialized from disk
     * @return the recording with {@code stepId}/{@code nextStepId} backfilled when needed
     */
    private static McpMobileRecording normalize(McpMobileRecording stored) {
        boolean needsBackfill = stored.actions().stream().anyMatch(action -> action.stepId().isBlank());
        List<McpMobileRecordedAction> actions = needsBackfill
                ? stored.actions().stream()
                        .map(action -> action.stepId().isBlank()
                                ? withStepId(action, "m" + action.sequence())
                                : action)
                        .toList()
                : stored.actions();
        long highestSuffix = actions.stream()
                .mapToLong(McpMobileRecordingService::numericStepIdSuffix)
                .max()
                .orElse(0L);
        long nextStepId = Math.max(Math.max(highestSuffix + 1, actions.size() + 1L), stored.nextStepId());
        if (!needsBackfill && nextStepId == stored.nextStepId()) {
            return stored;
        }
        return new McpMobileRecording(
                stored.schemaVersion(),
                stored.mode(),
                stored.startedAt(),
                stored.stoppedAt(),
                stored.includeSensitiveValues(),
                actions,
                stored.warnings(),
                nextStepId);
    }

    private static McpMobileRecordedAction withStepId(McpMobileRecordedAction action, String stepId) {
        return new McpMobileRecordedAction(
                stepId,
                action.sequence(),
                action.timestamp(),
                action.action(),
                action.locatorStrategy(),
                action.locatorValue(),
                action.parameters(),
                action.javaCode(),
                action.sensitiveValueStored(),
                action.warnings());
    }

    private static long numericStepIdSuffix(McpMobileRecordedAction action) {
        String stepId = action.stepId();
        if (stepId.length() < 2 || stepId.charAt(0) != 'm') {
            return 0L;
        }
        try {
            return Long.parseLong(stepId.substring(1));
        } catch (NumberFormatException ignored) {
            return 0L;
        }
    }

    private McpMobileReplayResult codeBlocks(
            Path path,
            McpMobileRecording stored,
            String driverVariableName,
            Path targetSource,
            String insertAfter) {
        String driver = javaIdentifierOrDefault(driverVariableName, "driver");
        PomCandidates pom = pomCandidates(stored, driver);
        List<McpCodeBlock> blocks = new ArrayList<>();
        blocks.add(replayBlock(stored, driver));
        if (!pom.locators().isEmpty()) {
            blocks.add(locatorInventoryBlock(pom.locators()));
        }
        if (!pom.actions().isEmpty()) {
            blocks.add(actionSequenceBlock(pom.actions()));
        }
        if (!pom.locators().isEmpty() && !pom.actions().isEmpty()) {
            blocks.add(pageObjectDraftBlock(pom, driver, replayWarnings(stored)));
        }
        if (!locatorReviewLines(stored).isEmpty()) {
            blocks.add(locatorConfidenceQueueBlock(stored));
        }
        if (targetSource != null || !text(insertAfter).isBlank()) {
            blocks.addAll(targetInsertionBlocks(targetSource, insertAfter, pom));
        }
        return new McpMobileReplayResult(path, true, 0, blocks, replayWarnings(stored));
    }

    private void persist() {
        try {
            Path parent = outputPath.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            mapper.writerWithDefaultPrettyPrinter().writeValue(outputPath.toFile(), recording);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Mobile recording could not be written.", exception);
        }
    }

    private McpCodeBlock replayBlock(McpMobileRecording stored, String driver) {
        StringBuilder code = new StringBuilder();
        code.append("public void replayMobileJourney(SHAFT.GUI.WebDriver ").append(driver).append(") {\n");
        for (McpMobileRecordedAction action : stored.actions()) {
            if (!action.warnings().isEmpty()) {
                code.append("    // ").append(String.join(" ", action.warnings())).append('\n');
            }
            for (String line : replaceDriver(action.javaCode(), driver).split("\\R")) {
                code.append("    ").append(line).append('\n');
            }
        }
        code.append("}\n");
        return new McpCodeBlock(
                "mobile-replay-method",
                "Generated mobile replay method",
                McpCodeBlock.Kind.TEST_METHOD,
                "java",
                List.of("com.shaft.driver.SHAFT"),
                code.toString(),
                "Paste into any class and call it with an initialized SHAFT.GUI.WebDriver.",
                replayWarnings(stored).isEmpty(),
                List.of(),
                replayWarnings(stored));
    }

    private static McpCodeBlock locatorInventoryBlock(List<LocatorCandidate> locators) {
        StringBuilder code = new StringBuilder();
        for (LocatorCandidate locator : locators) {
            code.append("// #")
                    .append(locator.sequence())
                    .append(' ')
                    .append(locator.fieldName())
                    .append(" -> ")
                    .append(locator.expression())
                    .append('\n');
            code.append("// selected from recorded ")
                    .append(locator.action())
                    .append(" action ")
                    .append(locator.sequence())
                    .append('\n');
        }
        return new McpCodeBlock(
                "mobile-pom-locator-inventory",
                "Mobile Page Object locator inventory",
                McpCodeBlock.Kind.LOCATOR,
                "java",
                List.of("com.shaft.driver.SHAFT"),
                code.toString(),
                "Use these ranked SHAFT locator expressions when extracting recorded actions into mobile Page Object methods.",
                false,
                locators.stream().map(locator -> "action-" + locator.sequence()).toList(),
                List.of());
    }

    private static McpCodeBlock actionSequenceBlock(List<ActionCandidate> actions) {
        StringBuilder code = new StringBuilder();
        code.append("// Flow: replayMobileJourney\n");
        for (ActionCandidate action : actions) {
            if (!action.warnings().isEmpty()) {
                code.append("// ").append(String.join(" ", action.warnings())).append('\n');
            }
            code.append(action.line()).append('\n');
        }
        return new McpCodeBlock(
                "mobile-pom-action-sequence",
                "Mobile Page Object action sequence",
                McpCodeBlock.Kind.ACTION,
                "java",
                List.of(),
                code.toString(),
                "Paste action lines into intent-named mobile page methods; keep orchestration in tests.",
                false,
                actions.stream().map(action -> "action-" + action.sequence()).toList(),
                List.of());
    }

    private static McpCodeBlock pageObjectDraftBlock(
            PomCandidates pom,
            String driver,
            List<String> warnings) {
        StringBuilder code = new StringBuilder();
        code.append("public final class MobileJourneyPage {\n");
        code.append("    private final SHAFT.GUI.WebDriver ").append(driver).append(";\n");
        for (LocatorCandidate locator : pom.locators()) {
            code.append("    private final By ")
                    .append(locator.fieldName())
                    .append(" = ")
                    .append(locator.expression())
                    .append(";\n");
        }
        code.append("\n");
        code.append("    public MobileJourneyPage(SHAFT.GUI.WebDriver ").append(driver).append(") {\n");
        code.append("        this.").append(driver).append(" = ").append(driver).append(";\n");
        code.append("    }\n\n");
        code.append("    public MobileJourneyPage replayMobileJourney() {\n");
        for (ActionCandidate action : pom.actions()) {
            if (!action.warnings().isEmpty()) {
                code.append("        // ").append(String.join(" ", action.warnings())).append('\n');
            }
            code.append("        ").append(replaceLocatorExpressions(action.line(), pom.locators())).append('\n');
        }
        code.append("        return this;\n");
        code.append("    }\n");
        code.append("}\n");
        List<String> draftWarnings = warnings.isEmpty()
                ? List.of("Review method naming and assertion placement before making the draft copy-paste ready.")
                : warnings;
        return new McpCodeBlock(
                "mobile-page-object-draft",
                "Mobile Page Object draft",
                McpCodeBlock.Kind.ACTION,
                "java",
                List.of("com.shaft.driver.SHAFT", "org.openqa.selenium.By"),
                code.toString(),
                "Use as a starting point for a mobile Page Object; merge locator fields and actions into the existing class when one exists.",
                false,
                pom.locators().stream().map(locator -> "action-" + locator.sequence()).toList(),
                draftWarnings);
    }

    private static McpCodeBlock locatorConfidenceQueueBlock(McpMobileRecording stored) {
        StringBuilder code = new StringBuilder();
        code.append("# Review these mobile locator risks before applying generated code\n");
        for (String line : locatorReviewLines(stored)) {
            code.append("- ").append(line).append('\n');
        }
        return new McpCodeBlock(
                "mobile-locator-confidence-queue",
                "Mobile locator confidence queue",
                McpCodeBlock.Kind.INVESTIGATION,
                "text",
                List.of(),
                code.toString(),
                "Fix or explicitly accept these mobile locator risks before replay.",
                false,
                stored.actions().stream()
                        .filter(action -> !action.warnings().isEmpty())
                        .map(action -> "action-" + action.sequence())
                        .toList(),
                List.of());
    }

    private static List<String> locatorReviewLines(McpMobileRecording stored) {
        List<String> lines = new ArrayList<>();
        stored.actions().stream()
                .filter(action -> !action.warnings().isEmpty())
                .forEach(action -> action.warnings().forEach(warning ->
                        lines.add("action-" + action.sequence() + ": " + warning)));
        return List.copyOf(lines);
    }

    private static List<McpCodeBlock> targetInsertionBlocks(
            Path targetSource,
            String insertAfter,
            PomCandidates pom) {
        if (!hasTargetInsertionCandidates(pom)) {
            return List.of();
        }
        TargetInsertionContext context = targetInsertionContext(targetSource, insertAfter);
        return List.of(
                targetLocatorFieldsBlock(context, pom),
                targetActionSnippetBlock(context, insertAfter, pom),
                targetPatchPreviewBlock(context, insertAfter, pom));
    }

    private static boolean hasTargetInsertionCandidates(PomCandidates pom) {
        return !pom.locators().isEmpty() && !pom.actions().isEmpty();
    }

    private static TargetInsertionContext targetInsertionContext(Path targetSource, String insertAfter) {
        String targetName = targetSource == null ? "the target Page Object" : targetSource.getFileName().toString();
        if (targetSource == null || text(insertAfter).isBlank()) {
            return new TargetInsertionContext(targetName, true, List.of());
        }
        boolean anchorFound = targetSourceContainsAnchor(targetSource, insertAfter);
        List<String> warnings = anchorFound
                ? List.of()
                : List.of("Could not find insertion anchor `" + insertAfter + "` in "
                + targetName + "; paste the snippet manually.");
        return new TargetInsertionContext(targetName, anchorFound, warnings);
    }

    private static boolean targetSourceContainsAnchor(Path targetSource, String insertAfter) {
        try {
            String source = Files.readString(targetSource);
            return source.contains(insertAfter + "(") || source.contains(insertAfter);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Mobile target source could not be read.", exception);
        }
    }

    private static McpCodeBlock targetLocatorFieldsBlock(TargetInsertionContext context, PomCandidates pom) {
        return new McpCodeBlock(
                "mobile-target-locator-fields",
                "Mobile record-at-target locator fields",
                McpCodeBlock.Kind.LOCATOR,
                "java",
                List.of("org.openqa.selenium.By"),
                targetLocatorFields(pom.locators()),
                "Paste into " + context.targetName()
                        + " near the existing mobile locator fields before adding the action snippet.",
                true,
                locatorEvidenceIds(pom.locators()),
                context.warnings());
    }

    private static String targetLocatorFields(List<LocatorCandidate> locators) {
        StringBuilder code = new StringBuilder();
        for (LocatorCandidate locator : locators) {
            code.append("private final By ")
                    .append(locator.fieldName())
                    .append(" = ")
                    .append(locator.expression())
                    .append(";\n");
        }
        return code.toString();
    }

    private static McpCodeBlock targetActionSnippetBlock(
            TargetInsertionContext context,
            String insertAfter,
            PomCandidates pom) {
        return new McpCodeBlock(
                "mobile-target-action-snippet",
                "Mobile record-at-target action snippet",
                McpCodeBlock.Kind.ACTION,
                "java",
                List.of(),
                targetActionSnippet(pom),
                targetActionPlacement(context.targetName(), insertAfter),
                context.anchorFound(),
                actionEvidenceIds(pom.actions()),
                context.warnings());
    }

    private static String targetActionSnippet(PomCandidates pom) {
        StringBuilder code = new StringBuilder();
        for (ActionCandidate action : pom.actions()) {
            if (!action.warnings().isEmpty()) {
                code.append("// ").append(String.join(" ", action.warnings())).append('\n');
            }
            code.append(replaceLocatorExpressions(action.line(), pom.locators())).append('\n');
        }
        return code.toString();
    }

    private static String targetActionPlacement(String targetName, String insertAfter) {
        return text(insertAfter).isBlank()
                ? "Paste inside the existing mobile page method after adding locator fields."
                : "Paste after " + insertAfter + " in " + targetName + ".";
    }

    private static McpCodeBlock targetPatchPreviewBlock(
            TargetInsertionContext context,
            String insertAfter,
            PomCandidates pom) {
        StringBuilder code = new StringBuilder();
        code.append("# Preview only; MCP does not edit source files.\n");
        code.append("Target: ").append(context.targetName()).append('\n');
        code.append("Anchor: ").append(text(insertAfter).isBlank() ? "<manual>" : insertAfter).append("\n\n");
        code.append("Imports:\n");
        code.append("+ import org.openqa.selenium.By;\n\n");
        code.append("Locator fields:\n");
        for (String line : targetLocatorFields(pom.locators()).lines().toList()) {
            code.append("+ ").append(line).append('\n');
        }
        code.append("\n@@ ");
        code.append(text(insertAfter).isBlank() ? "manual insertion point" : "after " + insertAfter);
        code.append(" @@\n");
        for (String line : targetActionSnippet(pom).lines().toList()) {
            code.append("+ ").append(line).append('\n');
        }
        return new McpCodeBlock(
                "mobile-target-patch-preview",
                "Mobile record-at-target patch preview",
                McpCodeBlock.Kind.PATCH_PREVIEW,
                "diff",
                List.of("org.openqa.selenium.By"),
                code.toString(),
                "Review this preview before applying snippets to " + context.targetName() + ".",
                false,
                java.util.stream.Stream.concat(
                        locatorEvidenceIds(pom.locators()).stream(),
                        actionEvidenceIds(pom.actions()).stream())
                        .distinct()
                        .toList(),
                context.warnings());
    }

    private static List<String> locatorEvidenceIds(List<LocatorCandidate> locators) {
        return locators.stream().map(locator -> "action-" + locator.sequence()).toList();
    }

    private static List<String> actionEvidenceIds(List<ActionCandidate> actions) {
        return actions.stream().map(action -> "action-" + action.sequence()).toList();
    }

    private static PomCandidates pomCandidates(McpMobileRecording stored, String driver) {
        Map<String, LocatorCandidate> locators = new LinkedHashMap<>();
        Map<String, Integer> fieldCounts = new LinkedHashMap<>();
        List<ActionCandidate> actions = new ArrayList<>();
        for (McpMobileRecordedAction action : stored.actions()) {
            locatorStrategy strategy = strategy(action.locatorStrategy());
            if (strategy != null && !action.locatorValue().isBlank()) {
                String expression = McpMobileCode.locatorCode(strategy, action.locatorValue());
                locators.computeIfAbsent(expression, ignored -> new LocatorCandidate(
                        uniqueFieldName(action.locatorValue(), fieldCounts),
                        expression,
                        action.sequence(),
                        action.action()));
            }
            actions.add(new ActionCandidate(
                    action.sequence(),
                    action.action(),
                    replaceDriver(action.javaCode(), driver),
                    action.warnings()));
        }
        return new PomCandidates(List.copyOf(locators.values()), actions);
    }

    private static String replaceLocatorExpressions(String line, List<LocatorCandidate> locators) {
        String replaced = line;
        for (LocatorCandidate locator : locators) {
            replaced = replaced.replace(locator.expression(), locator.fieldName());
        }
        return replaced;
    }

    private static String replaceDriver(String code, String driver) {
        return code.replace("driver.", driver + ".");
    }

    private static List<String> replayWarnings(McpMobileRecording stored) {
        LinkedHashSet<String> warnings = new LinkedHashSet<>(stored.warnings());
        stored.actions().stream()
                .flatMap(action -> action.warnings().stream())
                .forEach(warnings::add);
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

    private static locatorStrategy strategy(String value) {
        if (value == null || value.isBlank()) {
            return null;
        }
        try {
            return locatorStrategy.valueOf(value.trim());
        } catch (IllegalArgumentException ignored) {
            return null;
        }
    }

    private static String uniqueFieldName(String locatorValue, Map<String, Integer> counts) {
        String base = suggestedFieldName(locatorValue);
        int count = counts.getOrDefault(base, 0) + 1;
        counts.put(base, count);
        if (count == 1) {
            return base;
        }
        return base.substring(0, base.length() - "Locator".length()) + count + "Locator";
    }

    private static String suggestedFieldName(String value) {
        String seed = text(value);
        int resourceId = seed.lastIndexOf(":id/");
        if (resourceId >= 0) {
            seed = seed.substring(resourceId + ":id/".length());
        } else if (seed.contains("/")) {
            seed = seed.substring(seed.lastIndexOf('/') + 1);
        }
        String base = lowerCamel(seed);
        return base.isBlank() ? "mobileElementLocator" : base + "Locator";
    }

    private static String lowerCamel(String value) {
        String[] parts = text(value)
                .replaceAll("([a-z0-9])([A-Z])", "$1 $2")
                .replaceAll("[^A-Za-z0-9]+", " ")
                .trim()
                .split("\\s+");
        if (parts.length == 0 || parts[0].isBlank()) {
            return "";
        }
        StringBuilder result = new StringBuilder(parts[0].toLowerCase(Locale.ROOT));
        for (int index = 1; index < parts.length; index++) {
            String part = parts[index].toLowerCase(Locale.ROOT);
            result.append(Character.toUpperCase(part.charAt(0))).append(part.substring(1));
        }
        return javaIdentifierOrDefault(result.toString(), "mobileElement");
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }

    private record LocatorCandidate(String fieldName, String expression, long sequence, String action) {
    }

    private record ActionCandidate(long sequence, String action, String line, List<String> warnings) {
        private ActionCandidate {
            warnings = warnings == null ? List.of() : List.copyOf(warnings);
        }
    }

    private record PomCandidates(List<LocatorCandidate> locators, List<ActionCandidate> actions) {
        private PomCandidates {
            locators = locators == null ? List.of() : List.copyOf(locators);
            actions = actions == null ? List.of() : List.copyOf(actions);
        }
    }

    private record TargetInsertionContext(String targetName, boolean anchorFound, List<String> warnings) {
        private TargetInsertionContext {
            warnings = warnings == null ? List.of() : List.copyOf(warnings);
        }
    }

}
