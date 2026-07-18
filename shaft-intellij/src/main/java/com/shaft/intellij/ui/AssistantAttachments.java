package com.shaft.intellij.ui;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Dedupe and outbound-text-formatting helpers for the attach-to-prompt affordances (issue #3727).
 * Kept separate from {@link ShaftAssistantPanel} so the list bookkeeping and the outbound text
 * assembly are unit-testable without Swing.
 */
final class AssistantAttachments {
    private AssistantAttachments() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Adds {@code attachment} to {@code attachments}, replacing any existing entry with the same
     * path instead of appending a duplicate chip (guard rail: dedupe by path). Preserves the order
     * attachments were first added in; re-adding an existing path keeps its original position.
     */
    static List<AssistantAttachment> withAttachment(List<AssistantAttachment> attachments, AssistantAttachment attachment) {
        Map<String, AssistantAttachment> byPath = new LinkedHashMap<>();
        for (AssistantAttachment existing : attachments) {
            byPath.put(existing.path(), existing);
        }
        byPath.put(attachment.path(), attachment);
        return new ArrayList<>(byPath.values());
    }

    /** Removes the attachment at {@code path}, if present. */
    static List<AssistantAttachment> withoutAttachment(List<AssistantAttachment> attachments, String path) {
        List<AssistantAttachment> next = new ArrayList<>();
        for (AssistantAttachment existing : attachments) {
            if (!existing.path().equals(path)) {
                next.add(existing);
            }
        }
        return next;
    }

    /**
     * Renders the attachment list as the block folded into the outbound prompt text by {@code
     * AssistantCommand} (localAgentPrompt/cloudPrompt): a fenced code block per text file with its
     * path header, and an explicit path note for images -- the outbound transport is text-only
     * (both {@code autobot_local_agent_run} and {@code autobot_provider_chat} send a single JSON
     * {@code "prompt"} string property; see AssistantCommand.java's two
     * {@code arguments.addProperty("prompt", ...)} call sites), so an image's bytes are never
     * embedded, only its absolute path for a filesystem-capable agent to read.
     */
    static String outboundBlock(List<AssistantAttachment> attachments) {
        if (attachments == null || attachments.isEmpty()) {
            return "";
        }
        StringBuilder block = new StringBuilder("Attached context:");
        for (AssistantAttachment attachment : attachments) {
            block.append("\n\n");
            if (attachment.image()) {
                block.append("Image attached at: ").append(attachment.path())
                        .append("\nThis message is text-only: read the file at that absolute path to view "
                                + "the screenshot; its bytes are not embedded here.");
            } else {
                block.append("File: ").append(attachment.path());
                if (attachment.truncated()) {
                    block.append(" (truncated to ").append(AssistantAttachment.MAX_CONTENT_CHARACTERS)
                            .append(" characters)");
                }
                block.append("\n```\n").append(attachment.content()).append("\n```");
            }
        }
        return block.toString();
    }
}
