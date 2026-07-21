package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.shaft.intellij.ui.AssistantLocalAgentRunner.StructuredStreamParser;

import java.util.ArrayList;
import java.util.List;

/**
 * Maps Codex CLI's experimental {@code exec --json} events to human-readable transcript lines,
 * per {@code codex-rs/exec/src/exec_events.rs}'s {@code ThreadEvent}/{@code ThreadItemDetails}
 * enums (issue #3922's research pass). Handles the thread/turn lifecycle ({@code thread.started},
 * {@code turn.started}, {@code turn.completed}/{@code turn.failed}, the fatal top-level {@code
 * error}) and the real completed/updated item variants ({@code reasoning}, {@code
 * command_execution}/{@code mcp_tool_call}/{@code collab_tool_call}, {@code file_change}, {@code
 * agent_message}, {@code web_search}, {@code todo_list}, item-level {@code error}). There is no
 * {@code "tool_call"} item variant -- an earlier revision of this mapper matched it under a stale
 * assumption, but it never appears in live Codex output, so it is intentionally absent here and
 * falls through to {@link MapResult#UNKNOWN} like any other unrecognized item type.
 */
final class CodexStreamEventMapper implements StreamEventMapper {
    private final StructuredStreamParser state;

    CodexStreamEventMapper(StructuredStreamParser state) {
        this.state = state;
    }

    @Override
    public MapResult map(JsonObject event) {
        String type = StreamJson.stringField(event, "type");
        if ("item.completed".equals(type) || "item.updated".equals(type)) {
            return describeItemEvent(type, event);
        }
        if ("thread.started".equals(type)) {
            // ThreadStartedEvent per exec_events.rs carries only { thread_id }; a compact fixed
            // notice is enough to mark the session boundary without overclaiming detail.
            return MapResult.rendered("Codex session started.");
        }
        if ("turn.started".equals(type)) {
            // TurnStartedEvent per exec_events.rs is field-free ({}).
            return MapResult.rendered("Codex turn started.");
        }
        if ("turn.completed".equals(type) || "turn.failed".equals(type)) {
            return describeTurnTerminalEvent(type, event);
        }
        if ("error".equals(type)) {
            return describeTopLevelError(event);
        }
        return MapResult.UNKNOWN;
    }

    private MapResult describeTurnTerminalEvent(String type, JsonObject event) {
        JsonObject usage = StreamJson.objectField(event, "usage");
        Integer inputTokens = StreamJson.firstNonNull(
                StreamJson.intField(usage, "input_tokens"), state.currentInputTokens());
        Integer outputTokens = StreamJson.firstNonNull(
                StreamJson.intField(usage, "output_tokens"), state.currentOutputTokens());
        state.setUsage(inputTokens, outputTokens);
        if ("turn.completed".equals(type)) {
            String lastAgentMessage = StreamJson.stringField(event, "last_agent_message");
            String answer = lastAgentMessage != null
                    ? lastAgentMessage
                    : (state.currentAnswer() == null ? "" : state.currentAnswer());
            state.setAnswer(answer);
        } else {
            JsonObject error = StreamJson.objectField(event, "error");
            state.setTerminalDetail(StreamJson.firstNonBlank(StreamJson.stringField(event, "error"),
                    StreamJson.stringField(error, "message"), StreamJson.stringField(error, "type")));
        }
        // Fully consumed: this event becomes the final answer/usage, so it is mapped, not hidden.
        return MapResult.CONSUMED;
    }

    private MapResult describeTopLevelError(JsonObject event) {
        // ThreadEvent::Error per exec_events.rs -- a FATAL top-level error, distinct from
        // turn.failed's nested error above. Feeds terminalDetail so a run that dies here gets a
        // real reason in failureOutput() instead of the generic "exited with code N" fallback.
        String message = StreamJson.stringField(event, "message");
        if (message != null && !message.isBlank()) {
            state.setTerminalDetail(message);
            return MapResult.rendered("Error: " + message);
        }
        return MapResult.UNKNOWN;
    }

    /**
     * Describes an {@code item.completed}/{@code item.updated} event's {@code item.type}. Real
     * Codex {@code --json} item variants per {@code codex-rs/exec/src/exec_events.rs}'s {@code
     * ThreadItemDetails} enum: {@code agent_message}, {@code reasoning}, {@code command_execution},
     * {@code file_change}, {@code mcp_tool_call}, {@code collab_tool_call}, {@code web_search},
     * {@code todo_list}, {@code error}.
     */
    private MapResult describeItemEvent(String type, JsonObject event) {
        JsonObject item = StreamJson.objectField(event, "item");
        String itemType = StreamJson.stringField(item, "type");
        if ("reasoning".equals(itemType)) {
            String reasoning = StreamJson.firstNonBlank(
                    StreamJson.stringField(item, "text"), StreamJson.stringField(item, "summary"));
            return reasoning == null ? MapResult.UNKNOWN : MapResult.rendered("Reasoning: " + reasoning);
        }
        if ("command_execution".equals(itemType) || "mcp_tool_call".equals(itemType)
                || "collab_tool_call".equals(itemType)) {
            return describeToolCallItem(type, item);
        }
        if ("file_change".equals(itemType)) {
            if ("item.completed".equals(type)) {
                recordCodexFileMutation(item);
            }
            return MapResult.UNKNOWN;
        }
        if ("agent_message".equals(itemType)) {
            String text = StreamJson.stringField(item, "text");
            return text != null && !text.isBlank() ? MapResult.rendered(text) : MapResult.UNKNOWN;
        }
        if ("web_search".equals(itemType)) {
            String query = StreamJson.stringField(item, "query");
            return query != null && !query.isBlank() ? MapResult.rendered("Web search: " + query) : MapResult.UNKNOWN;
        }
        if ("todo_list".equals(itemType)) {
            String todoList = describeCodexTodoList(item);
            return todoList == null ? MapResult.UNKNOWN : MapResult.rendered(todoList);
        }
        if ("error".equals(itemType)) {
            // ErrorItem per exec_events.rs -- item-level, non-fatal (e.g. "command output
            // truncated"), distinct from the top-level ThreadEvent::Error handled above.
            String message = StreamJson.stringField(item, "message");
            return message != null && !message.isBlank() ? MapResult.rendered("Error: " + message) : MapResult.UNKNOWN;
        }
        return MapResult.UNKNOWN;
    }

    private MapResult describeToolCallItem(String type, JsonObject item) {
        String toolName = StreamJson.firstNonBlank(StreamJson.stringField(item, "name"),
                StreamJson.stringField(item, "tool"), StreamJson.stringField(item, "command"));
        String label = toolName == null ? "(unknown)" : toolName;
        String summary = StreamJson.toolInputSummary(item);
        List<String> lines = new ArrayList<>();
        lines.add(summary == null || summary.equals(label)
                ? "Calling tool " + label + "..."
                : "Calling tool " + label + " (" + summary + ")...");
        if ("item.completed".equals(type)) {
            recordCodexToolFailure(label, item);
            String output = StreamJson.firstNonBlank(StreamJson.stringField(item, "aggregated_output"),
                    StreamJson.stringField(item, "output"), StreamJson.stringField(item, "result"));
            if (output != null && !output.isBlank()) {
                Integer exitCode = StreamJson.intField(item, "exit_code");
                String outputSummary = output.strip();
                lines.add(exitCode != null && exitCode != 0
                        ? "Tool failed (" + label + ", exit " + exitCode + "): " + outputSummary
                        : "Tool result (" + label + "): " + outputSummary);
            }
        }
        return MapResult.rendered(String.join("\n", lines));
    }

    /**
     * Remembers the file(s) touched by a completed Codex {@code file_change} item (Codex's
     * {@code --json} counterpart to Claude's Write/Edit tool calls) so the final bubble can list
     * what was actually created or edited. Codex's {@code file_change} item reports a real
     * {@code status} ("completed" when the patch was applied, "failed" when it was not), so unlike
     * {@link ClaudeStreamEventMapper}'s request-time guess, this only credits paths whose patch
     * actually succeeded; a failed patch is instead counted as a failed/denied tool call via
     * {@link #recordCodexToolFailure} so it never claims a file was edited when it was not.
     */
    private void recordCodexFileMutation(JsonObject item) {
        JsonElement changes = item == null ? null : item.get("changes");
        if (changes == null || !changes.isJsonArray()) {
            return;
        }
        if ("failed".equals(StreamJson.stringField(item, "status"))) {
            state.recordToolFailure("file_change");
            return;
        }
        for (JsonElement element : changes.getAsJsonArray()) {
            if (!element.isJsonObject()) {
                continue;
            }
            String path = StreamJson.stringField(element.getAsJsonObject(), "path");
            if (path != null && !path.isBlank()) {
                state.recordFileTouched(path);
            }
        }
    }

    /**
     * Aggregates a completed Codex tool item's failure into the same per-tool bucket Claude's
     * {@code permission_denials} array populates, using the completed item's {@code status}
     * field (including {@code "failed"} and {@code "declined"} -- {@code CommandExecutionStatus}
     * per {@code exec_events.rs} -- the latter being Codex's approval/sandbox-rejection outcome,
     * which carries neither a non-zero exit code nor an error object of its own), a non-zero
     * {@code exit_code} (shell commands), or a present {@code error} object (MCP tool calls) as
     * the failure signal. Codex's {@code --json} schema has no field that distinguishes a
     * sandbox/approval denial from an ordinary execution failure (see issue #3679), so both are
     * merged here; {@code StructuredStreamParser#activitySummary} labels this bucket "Failed or
     * denied tool calls" for Codex runs rather than reusing Claude's more precise "Denied tool
     * calls" wording, so the footer never overclaims what the CLI's own output can actually prove.
     */
    private void recordCodexToolFailure(String toolName, JsonObject item) {
        if (toolName == null || toolName.isBlank() || item == null) {
            return;
        }
        Integer exitCode = StreamJson.intField(item, "exit_code");
        String status = StreamJson.stringField(item, "status");
        boolean failed = "failed".equals(status)
                || "declined".equals(status)
                || (exitCode != null && exitCode != 0)
                || StreamJson.objectField(item, "error") != null;
        if (failed) {
            state.recordToolFailure(toolName);
        }
    }

    /**
     * Renders a completed Codex {@code todo_list} item ({@code items: [{ text, completed }] }) as
     * a Markdown checklist, one line per entry.
     */
    private static String describeCodexTodoList(JsonObject item) {
        JsonElement itemsElement = item == null ? null : item.get("items");
        if (itemsElement == null || !itemsElement.isJsonArray()) {
            return null;
        }
        List<String> lines = new ArrayList<>();
        lines.add("Todo list:");
        for (JsonElement element : itemsElement.getAsJsonArray()) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject todo = element.getAsJsonObject();
            String text = StreamJson.stringField(todo, "text");
            if (text == null || text.isBlank()) {
                continue;
            }
            lines.add((StreamJson.booleanField(todo, "completed") ? "- [x] " : "- [ ] ") + text);
        }
        return lines.size() <= 1 ? null : String.join("\n", lines);
    }
}
