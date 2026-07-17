package com.shaft.intellij.java;

/**
 * Where SHAFT-generated code should land: a resolved Java method/class ({@code insertAfter} is
 * that method's name) or the file open in the editor ({@code insertAfter} left blank so the
 * receiving tool appends to the class).
 *
 * <p>Shared mental model (issue #3662) for {@code RecordShaftFlowHereAction}, {@code
 * ShaftAssistantPanel#insertReviewIntoOpenFile}, and {@code GuidedWorkflowPanel#insertCodeAtCaret}
 * -- previously each computed this answer differently depending on which tab the user started
 * from. {@link #choose(JavaTargetContext, String)} is the single source of truth all three now
 * consult.
 *
 * @param targetSourcePath source file the insertion is anchored to
 * @param insertAfter resolved method name, or blank when anchored to the whole file
 * @param displayName compact label for notifications/status text
 */
public record InsertionAnchor(String targetSourcePath, String insertAfter, String displayName) {
    /**
     * Picks the anchor: the caret's enclosing method when it resolves to one, otherwise the file
     * open in the editor, otherwise no anchor at all.
     *
     * @param caretContext resolved Java context at the caret, or null when unavailable/unresolvable
     * @param openEditorFilePath path of the file open in the editor, or blank when none is open
     * @return the chosen anchor, or null when neither a caret nor an open file is available
     */
    public static InsertionAnchor choose(JavaTargetContext caretContext, String openEditorFilePath) {
        if (caretContext != null) {
            return new InsertionAnchor(caretContext.sourcePath(), caretContext.methodName(), caretContext.displayName());
        }
        if (openEditorFilePath == null || openEditorFilePath.isBlank()) {
            return null;
        }
        return new InsertionAnchor(openEditorFilePath, "", fileNameOf(openEditorFilePath));
    }

    private static String fileNameOf(String path) {
        int lastSeparator = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
        return lastSeparator < 0 ? path : path.substring(lastSeparator + 1);
    }
}
