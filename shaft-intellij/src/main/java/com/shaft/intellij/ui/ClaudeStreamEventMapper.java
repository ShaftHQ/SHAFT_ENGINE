package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.shaft.intellij.ui.AssistantLocalAgentRunner.StructuredStreamParser;

import java.util.ArrayList;
import java.util.List;

/**
 * Maps Claude Code's {@code --output-format stream-json} events to human-readable transcript
 * lines. Handles the conversation events -- {@code assistant} (thinking/tool_use/text blocks),
 * {@code user} (the synthetic tool_result message Claude emits after a tool call), {@code
 * result} (the terminal answer/usage event) -- plus the two most user-visible {@code system}
 * subtypes ({@code init}, {@code compact_boundary}). The remaining official {@code system}
 * subtypes ({@code api_retry}, {@code plugin_install}, {@code hook_started}/{@code
 * hook_progress}/{@code hook_response}) are deliberately left unhandled (issue #3974): their
 * field shapes were never confirmed against a real Claude Code session, so they fall through to
 * {@link MapResult#UNKNOWN} -- safe raw-JSON passthrough in Verbose mode, same as any other
 * unrecognized event, rather than guessing at an unconfirmed schema.
 */
final class ClaudeStreamEventMapper implements StreamEventMapper {
    private final StructuredStreamParser state;

    ClaudeStreamEventMapper(StructuredStreamParser state) {
        this.state = state;
    }

    @Override
    public MapResult map(JsonObject event) {
        String type = StreamJson.stringField(event, "type");
        if ("assistant".equals(type)) {
            return describeAssistantEvent(event);
        }
        if ("user".equals(type)) {
            return describeToolResults(event);
        }
        if ("system".equals(type)) {
            return describeSystemEvent(event);
        }
        if ("result".equals(type)) {
            return describeResultEvent(event);
        }
        return MapResult.UNKNOWN;
    }

    /**
     * Describes an "assistant" event as one or more human-readable lines (joined with newlines),
     * covering every recognized block in the message rather than stopping at the first match: an
     * extended-thinking block followed by a tool call and some text all become separate lines, so
     * Verbose mode shows the CLI's full train of thought instead of only a fragment of it.
     */
    private MapResult describeAssistantEvent(JsonObject event) {
        JsonObject message = StreamJson.objectField(event, "message");
        JsonElement content = message == null ? null : message.get("content");
        if (content == null || !content.isJsonArray()) {
            return MapResult.UNKNOWN;
        }
        List<String> lines = new ArrayList<>();
        for (JsonElement blockElement : content.getAsJsonArray()) {
            if (!blockElement.isJsonObject()) {
                continue;
            }
            JsonObject block = blockElement.getAsJsonObject();
            String blockType = StreamJson.stringField(block, "type");
            if ("thinking".equals(blockType)) {
                String thinking = StreamJson.stringField(block, "thinking");
                if (thinking != null && !thinking.isBlank()) {
                    lines.add("Thinking: " + thinking);
                }
            } else if ("redacted_thinking".equals(blockType)) {
                lines.add("Thinking: (redacted by Claude for safety)");
            } else if ("tool_use".equals(blockType)) {
                lines.add(describeToolUseBlock(block));
            } else if ("text".equals(blockType)) {
                String text = StreamJson.stringField(block, "text");
                if (text != null && !text.isBlank()) {
                    lines.add(text);
                }
            }
        }
        return lines.isEmpty() ? MapResult.UNKNOWN : MapResult.rendered(String.join("\n", lines));
    }

    private String describeToolUseBlock(JsonObject block) {
        String toolName = StreamJson.stringField(block, "name");
        String toolUseId = StreamJson.stringField(block, "id");
        if (toolUseId != null && toolName != null) {
            state.rememberToolName(toolUseId, toolName);
        }
        JsonObject input = StreamJson.objectField(block, "input");
        recordFileMutation(toolName, input);
        String plan = "ExitPlanMode".equals(toolName) ? StreamJson.stringField(input, "plan") : null;
        if (plan != null && !plan.isBlank()) {
            // Claude Code's own built-in "propose this plan, ask to proceed" tool sends its
            // proposal in a "plan" input key that toolInputSummary does not recognize (issue
            // #3680): without this branch it would fall through to the generic bare "Calling tool
            // ExitPlanMode..." line below, hiding the one piece of information -- the plan itself
            // -- the user actually needs.
            state.setPlanProposal(plan);
            return "**Plan proposed:**\n\n" + plan;
        }
        String label = toolName == null ? "(unknown)" : toolName;
        String summary = StreamJson.toolInputSummary(input);
        return summary == null
                ? "Calling tool " + label + "..."
                : "Calling tool " + label + " (" + summary + ")...";
    }

    /**
     * Describes a Claude {@code system} event (session lifecycle/control-plane notices, distinct
     * from the assistant/user/result conversation events above). Official subtypes per the
     * documented stream-json schema: {@code init}, {@code api_retry}, {@code plugin_install},
     * {@code hook_started}/{@code hook_progress}/{@code hook_response}, and {@code
     * compact_boundary}. Only the two most user-visible subtypes get a compact rendering here
     * (issue #3922); the rest fall through to {@link MapResult#UNKNOWN} -- raw-JSON passthrough in
     * Verbose mode, same as any other unrecognized event, so nothing is silently hidden even
     * though it isn't translated yet.
     */
    private static MapResult describeSystemEvent(JsonObject event) {
        String subtype = StreamJson.stringField(event, "subtype");
        if ("init".equals(subtype)) {
            List<String> parts = new ArrayList<>();
            String model = StreamJson.stringField(event, "model");
            String sessionId = StreamJson.stringField(event, "session_id");
            if (model != null && !model.isBlank()) {
                parts.add("model " + model);
            }
            if (sessionId != null && !sessionId.isBlank()) {
                parts.add("session " + sessionId);
            }
            return MapResult.rendered(parts.isEmpty()
                    ? "Session started."
                    : "Session started (" + String.join(", ", parts) + ").");
        }
        if ("compact_boundary".equals(subtype)) {
            return MapResult.rendered("Conversation history was compacted to save context.");
        }
        return MapResult.UNKNOWN;
    }

    private MapResult describeResultEvent(JsonObject event) {
        String resultText = StreamJson.stringField(event, "result");
        state.setAnswer(resultText == null ? "" : resultText);
        JsonObject usage = StreamJson.objectField(event, "usage");
        state.setUsage(StreamJson.intField(usage, "input_tokens"), StreamJson.intField(usage, "output_tokens"));
        recordPermissionDenials(event.get("permission_denials"));
        String subtype = StreamJson.stringField(event, "subtype");
        if (StreamJson.booleanField(event, "is_error") || (subtype != null && subtype.startsWith("error"))) {
            state.setTerminalDetail(humanizeSubtype(subtype));
        }
        // Fully consumed: this event becomes the final answer, so it is mapped, not hidden.
        return MapResult.CONSUMED;
    }

    /**
     * Remembers the target path of a file-mutating built-in tool call (Claude's Write/Edit
     * family) so the final bubble can list what was actually created or edited.
     */
    private void recordFileMutation(String toolName, JsonObject input) {
        if (toolName == null || input == null) {
            return;
        }
        if (!"Write".equals(toolName) && !"Edit".equals(toolName)
                && !"MultiEdit".equals(toolName) && !"NotebookEdit".equals(toolName)) {
            return;
        }
        String path = StreamJson.firstNonBlank(
                StreamJson.stringField(input, "file_path"), StreamJson.stringField(input, "path"));
        if (path != null) {
            state.recordFileTouched(path);
        }
    }

    /**
     * Aggregates the terminal event's {@code permission_denials} array (Claude stream-json)
     * into per-tool counts for the activity footer.
     */
    private void recordPermissionDenials(JsonElement denials) {
        if (denials == null || !denials.isJsonArray()) {
            return;
        }
        for (JsonElement element : denials.getAsJsonArray()) {
            if (!element.isJsonObject()) {
                continue;
            }
            String toolName = StreamJson.stringField(element.getAsJsonObject(), "tool_name");
            if (toolName != null && !toolName.isBlank()) {
                state.recordToolFailure(toolName);
            }
        }
    }

    /**
     * Turns a machine error subtype (e.g. {@code error_max_turns}) into a short prose reason
     * (e.g. {@code max turns}) for the failure headline.
     */
    private static String humanizeSubtype(String subtype) {
        if (subtype == null || subtype.isBlank()) {
            return "the run failed";
        }
        String cleaned = subtype.replace("error_", "").replace('_', ' ').trim();
        return cleaned.isBlank() ? "the run failed" : cleaned;
    }

    /**
     * Describes a "user" event's {@code tool_result} blocks (Claude's stream-json protocol
     * delivers a completed tool call's outcome as a synthetic user-role message, not as part of
     * the assistant event that requested it) as one line per result, correlated back to the
     * requesting call's tool name via {@link StructuredStreamParser#toolNameFor}. Without this,
     * Verbose mode shows "Calling tool X..." and then nothing -- the exact "generic thinking, not
     * what it's actually doing" gap the Verbose toggle exists to close.
     */
    private MapResult describeToolResults(JsonObject event) {
        JsonObject message = StreamJson.objectField(event, "message");
        JsonElement content = message == null ? null : message.get("content");
        if (content == null || !content.isJsonArray()) {
            return MapResult.UNKNOWN;
        }
        List<String> lines = new ArrayList<>();
        for (JsonElement blockElement : content.getAsJsonArray()) {
            if (!blockElement.isJsonObject()) {
                continue;
            }
            JsonObject block = blockElement.getAsJsonObject();
            if (!"tool_result".equals(StreamJson.stringField(block, "type"))) {
                continue;
            }
            String toolName = state.toolNameFor(StreamJson.stringField(block, "tool_use_id"));
            String label = toolName == null ? "tool" : toolName;
            String text = toolResultText(block.get("content"));
            String summary = text == null || text.isBlank() ? "(no output)" : text.strip();
            lines.add((StreamJson.booleanField(block, "is_error") ? "Tool failed (" : "Tool result (")
                    + label + "): " + summary);
        }
        return lines.isEmpty() ? MapResult.UNKNOWN : MapResult.rendered(String.join("\n", lines));
    }

    /**
     * Extracts the human-readable text of a {@code tool_result} block's {@code content}, which per
     * Claude's stream-json protocol is either a plain string or an array of content blocks (text
     * blocks are joined; non-text blocks such as images are skipped since they have nothing to
     * show in a text transcript).
     */
    private static String toolResultText(JsonElement content) {
        if (content == null || content.isJsonNull()) {
            return null;
        }
        if (content.isJsonPrimitive() && content.getAsJsonPrimitive().isString()) {
            return content.getAsString();
        }
        if (content.isJsonArray()) {
            List<String> parts = new ArrayList<>();
            for (JsonElement element : content.getAsJsonArray()) {
                if (element.isJsonObject()) {
                    String text = StreamJson.stringField(element.getAsJsonObject(), "text");
                    if (text != null && !text.isBlank()) {
                        parts.add(text);
                    }
                }
            }
            return parts.isEmpty() ? null : String.join("\n", parts);
        }
        return null;
    }
}
