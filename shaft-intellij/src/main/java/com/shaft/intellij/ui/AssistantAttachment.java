package com.shaft.intellij.ui;

/**
 * One file or image attached to an Assistant prompt via the attach affordances (issue #3727):
 * current-file / all-open-files / pick-file-from-disk / screenshot. Immutable value held by the
 * removable chip row in {@link ShaftAssistantPanel} and folded into the outbound prompt text by
 * {@link AssistantAttachments#outboundBlock}.
 *
 * <p>{@code content} is always empty for an image: the outbound transport is text-only (see
 * {@code AssistantCommand.java}'s {@code arguments.addProperty("prompt", ...)} call sites), so an
 * image attachment carries only its absolute path -- {@link AssistantAttachments#outboundBlock}
 * turns that into an explicit note telling a filesystem-capable agent to read the file itself.
 */
record AssistantAttachment(String path, String content, boolean image, boolean truncated) {
    /**
     * Embedded text-file content longer than this is truncated with a visible note (issue #3727
     * guard rail), mirroring {@link ShaftAssistantPanel}'s own MAX_AGENT_CONTEXT_CHARACTERS cap on
     * conversation history.
     */
    static final int MAX_CONTENT_CHARACTERS = 16_000;

    static AssistantAttachment forText(String path, String content) {
        String safe = content == null ? "" : content;
        boolean overLimit = safe.length() > MAX_CONTENT_CHARACTERS;
        return new AssistantAttachment(path, overLimit ? safe.substring(0, MAX_CONTENT_CHARACTERS) : safe,
                false, overLimit);
    }

    static AssistantAttachment forImage(String path) {
        return new AssistantAttachment(path, "", true, false);
    }

    /** Chip label: the file name only, never the full path. */
    String displayName() {
        return ShaftAssistantPanel.fileName(path);
    }
}
