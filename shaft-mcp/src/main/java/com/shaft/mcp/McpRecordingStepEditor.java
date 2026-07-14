package com.shaft.mcp;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

/**
 * Model-generic step-surgery logic shared by the mobile and Playwright MCP recorders, both of
 * which persist {@link McpMobileRecording}/{@link McpMobileRecordedAction} (#3528). Holds only
 * pure, stateless helpers: index lookup, renumbering, stepId backfill, and delete/reorder
 * computation. Callers (one recorder service per model) own {@code synchronized}, {@code
 * persist()}, and the message strings surfaced when a recording is inactive.
 */
final class McpRecordingStepEditor {
    private McpRecordingStepEditor() {
    }

    /**
     * Backfills recordings persisted before schema 1.1 so step surgery ({@link #delete}, {@link
     * #reorder}) and codegen keep working on old files: Jackson leaves a missing {@code stepId} as
     * {@code ""} (coerced by the record's compact constructor) and a missing {@code nextStepId} as
     * {@code 0}, so any blank {@code stepId} is backfilled to {@code "m" + sequence}, and {@code
     * nextStepId} is raised above every numeric {@code stepId} suffix in use (and above the action
     * count) so newly recorded or mutated steps never collide with, or reuse, a backfilled id.
     *
     * @param stored the recording as deserialized from disk
     * @return the recording with {@code stepId}/{@code nextStepId} backfilled when needed
     */
    static McpMobileRecording normalize(McpMobileRecording stored) {
        boolean needsBackfill = stored.actions().stream().anyMatch(action -> action.stepId().isBlank());
        List<McpMobileRecordedAction> actions = needsBackfill
                ? stored.actions().stream()
                        .map(action -> action.stepId().isBlank()
                                ? withStepId(action, "m" + action.sequence())
                                : action)
                        .toList()
                : stored.actions();
        long highestSuffix = actions.stream()
                .mapToLong(McpRecordingStepEditor::numericStepIdSuffix)
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

    /**
     * Projects a recording's actions into the per-step summaries surfaced in recorder status.
     *
     * @param stored the live or just-closed recording
     * @return step summaries in recorded order
     */
    static List<McpMobileStepSummary> stepSummaries(McpMobileRecording stored) {
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
     * Finds the index of the recorded action with the given stable {@code stepId}.
     *
     * @param recording the recording to search
     * @param stepId stable step id to find; blank or unmatched returns {@code -1}
     * @return the matching index, or {@code -1} when not found
     */
    static int indexOfStep(McpMobileRecording recording, String stepId) {
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

    /**
     * Computes the recording with the step at {@code stepId} removed and the remaining steps'
     * {@code sequence} renumbered to stay contiguous (1..N). An unknown {@code stepId} is a no-op:
     * the same {@code recording} instance is returned (by reference), so callers can skip {@code
     * persist()}.
     *
     * @param recording the recording to edit
     * @param stepId stable step id to remove
     * @return the recording with the step removed, or {@code recording} unchanged when {@code
     *         stepId} does not match a step
     */
    static McpMobileRecording delete(McpMobileRecording recording, String stepId) {
        int index = indexOfStep(recording, stepId);
        if (index < 0) {
            return recording;
        }
        List<McpMobileRecordedAction> actions = new ArrayList<>(recording.actions());
        actions.remove(index);
        return new McpMobileRecording(
                recording.schemaVersion(),
                recording.mode(),
                recording.startedAt(),
                recording.stoppedAt(),
                recording.includeSensitiveValues(),
                renumber(actions),
                recording.warnings(),
                recording.nextStepId());
    }

    /**
     * Computes the recording with the step at {@code stepId} moved up or down and the remaining
     * steps' {@code sequence} renumbered to stay contiguous (1..N). Moving the first step up, the
     * last step down, an unknown {@code stepId}, or an unrecognized {@code direction} are all
     * no-ops: the same {@code recording} instance is returned (by reference), so callers can skip
     * {@code persist()}.
     *
     * @param recording the recording to edit
     * @param stepId stable step id to move
     * @param direction "up" or "down" (case-insensitive)
     * @return the recording with the step moved, or {@code recording} unchanged when the move is a
     *         no-op
     */
    static McpMobileRecording reorder(McpMobileRecording recording, String stepId, String direction) {
        String normalizedDirection = direction == null ? "" : direction.trim().toLowerCase(Locale.ROOT);
        if (!normalizedDirection.equals("up") && !normalizedDirection.equals("down")) {
            return recording;
        }
        int index = indexOfStep(recording, stepId);
        if (index < 0) {
            return recording;
        }
        int targetIndex = normalizedDirection.equals("up") ? index - 1 : index + 1;
        if (targetIndex < 0 || targetIndex >= recording.actions().size()) {
            return recording;
        }
        List<McpMobileRecordedAction> actions = new ArrayList<>(recording.actions());
        Collections.swap(actions, index, targetIndex);
        return new McpMobileRecording(
                recording.schemaVersion(),
                recording.mode(),
                recording.startedAt(),
                recording.stoppedAt(),
                recording.includeSensitiveValues(),
                renumber(actions),
                recording.warnings(),
                recording.nextStepId());
    }

    static List<McpMobileRecordedAction> renumber(List<McpMobileRecordedAction> actions) {
        List<McpMobileRecordedAction> renumbered = new ArrayList<>(actions.size());
        for (int index = 0; index < actions.size(); index++) {
            renumbered.add(withSequence(actions.get(index), index + 1L));
        }
        return List.copyOf(renumbered);
    }

    static McpMobileRecordedAction withSequence(McpMobileRecordedAction action, long sequence) {
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

    static McpMobileRecordedAction withStepId(McpMobileRecordedAction action, String stepId) {
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

    static long numericStepIdSuffix(McpMobileRecordedAction action) {
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
}
