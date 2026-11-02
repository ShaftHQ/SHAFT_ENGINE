package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.shaft.intellij.ui.AssistantLocalAgentRunner.StructuredStreamParser;

import java.util.ArrayList;
import java.util.List;

/**
 * Maps the Grok CLI's headless {@code --output-format streaming-json} events (issue #6748) to
 * human-readable transcript lines. Per the CLI's headless-mode guide each line is one {@code
 * type}-tagged object: {@code text} and {@code thought} chunks (field {@code data}), {@code
 * tool_call} / {@code tool_call_update}, {@code usage}, {@code plan}, {@code available_commands},
 * a final {@code end} (always last, carrying {@code stopReason} and {@code sessionId}) and {@code
 * error}. The guide marks the event list non-exhaustive, so unrecognized types fall through to
 * {@link MapResult#UNKNOWN} instead of being treated as failures.
 *
 * <p>Text and thought arrive in arbitrary chunks, so they are buffered and only complete lines are
 * rendered; whatever is left is flushed when a different kind of event (or the final {@code end})
 * arrives, so progress shows up live without cutting words in half.
 */
final class GrokStreamEventMapper implements StreamEventMapper {
    private static final int MAX_REASONING_CHARS = 300;
    private final StructuredStreamParser state;
    private final StringBuilder text = new StringBuilder();
    private final StringBuilder answer = new StringBuilder();
    private final StringBuilder thought = new StringBuilder();
    private boolean errored;

    GrokStreamEventMapper(StructuredStreamParser state) {
        this.state = state;
    }

    @Override
    public MapResult map(JsonObject event) {
        String type = StreamJson.stringField(event, "type");
        if ("text".equals(type)) {
            return bufferText(StreamJson.stringField(event, "data"));
        }
        if ("thought".equals(type)) {
            return bufferThought(StreamJson.stringField(event, "data"));
        }
        List<String> lines = new ArrayList<>(flushBuffers(!"tool_call_update".equals(type) && !"usage".equals(type)));
        MapResult result = mapToolEvent(type, event);
        if (result instanceof MapResult.Unknown) {
            result = mapOther(type, event);
        }
        if (result instanceof MapResult.Rendered rendered) {
            lines.add(rendered.text());
        }
        if (!lines.isEmpty()) {
            return MapResult.rendered(String.join("\n", lines));
        }
        return result;
    }

    private MapResult mapToolEvent(String type, JsonObject event) {
        if ("tool_call".equals(type)) {
            return describeToolCall(event);
        }
        if ("tool_call_update".equals(type)) {
            return describeToolUpdate(event);
        }
        if ("usage".equals(type)) {
            recordUsage(StreamJson.objectField(event, "usage"));
            return MapResult.CONSUMED;
        }
        return MapResult.UNKNOWN;
    }

    private MapResult mapOther(String type, JsonObject event) {
        if ("plan".equals(type)) {
            String plan = describePlan(event);
            return plan == null ? MapResult.CONSUMED : MapResult.rendered(plan);
        }
        if ("available_commands".equals(type)) {
            return MapResult.CONSUMED;
        }
        if ("end".equals(type)) {
            return describeEnd(event);
        }
        if ("error".equals(type)) {
            String message = StreamJson.stringField(event, "message");
            if (message != null && !message.isBlank()) {
                errored = true;
                state.setTerminalDetail(message);
                return MapResult.rendered("Error: " + message);
            }
        }
        return MapResult.UNKNOWN;
    }

    private MapResult bufferText(String chunk) {
        if (chunk == null || chunk.isEmpty()) {
            return MapResult.CONSUMED;
        }
        answer.append(chunk);
        text.append(chunk);
        return drainCompleteLines(text);
    }

    private MapResult bufferThought(String chunk) {
        if (chunk == null || chunk.isEmpty()) {
            return MapResult.CONSUMED;
        }
        thought.append(chunk);
        return MapResult.CONSUMED;
    }

    /** Renders every complete line of {@code buffer}, keeping the unfinished tail buffered. */
    private static MapResult drainCompleteLines(StringBuilder buffer) {
        int lastBreak = buffer.lastIndexOf("\n");
        if (lastBreak < 0) {
            return MapResult.CONSUMED;
        }
        String complete = buffer.substring(0, lastBreak).strip();
        buffer.delete(0, lastBreak + 1);
        return complete.isEmpty() ? MapResult.CONSUMED : MapResult.rendered(complete);
    }

    private List<String> flushBuffers(boolean includeText) {
        List<String> lines = new ArrayList<>();
        String reasoning = thought.toString().strip().replaceAll("\\s+", " ");
        thought.setLength(0);
        if (!reasoning.isEmpty()) {
            lines.add("Reasoning: " + (reasoning.length() > MAX_REASONING_CHARS
                    ? reasoning.substring(0, MAX_REASONING_CHARS) + "..." : reasoning));
        }
        String remainder = text.toString().strip();
        if (includeText || remainder.length() > 0) {
            text.setLength(0);
            if (!remainder.isEmpty()) {
                lines.add(remainder);
            }
        }
        return lines;
    }

    private MapResult describeToolCall(JsonObject event) {
        state.recordToolCallObserved();
        String label = StreamJson.firstNonBlank(StreamJson.stringField(event, "toolName"),
                StreamJson.stringField(event, "title"));
        String name = label == null ? "(unknown)" : label;
        String summary = StreamJson.toolInputSummary(StreamJson.objectField(event, "rawInput"));
        return MapResult.rendered(summary == null || summary.equals(name)
                ? "Calling tool " + name + "..."
                : "Calling tool " + name + " (" + summary + ")...");
    }

    private MapResult describeToolUpdate(JsonObject event) {
        String status = StreamJson.stringField(event, "status");
        if ("failed".equals(status)) {
            state.recordToolFailure("tool_call");
            JsonElement output = event.get("rawOutput");
            String detail = output == null || output.isJsonNull() ? "" : ": " + output.toString();
            return MapResult.rendered("Tool failed" + detail);
        }
        return MapResult.CONSUMED;
    }

    private void recordUsage(JsonObject usage) {
        Integer input = StreamJson.intField(usage, "input_tokens");
        Integer output = StreamJson.intField(usage, "output_tokens");
        if (input != null || output != null) {
            state.setUsage(StreamJson.firstNonNull(input, state.currentInputTokens()),
                    StreamJson.firstNonNull(output, state.currentOutputTokens()));
        }
    }

    private static String describePlan(JsonObject event) {
        JsonElement entries = event.get("entries");
        if (entries == null || !entries.isJsonArray()) {
            return null;
        }
        List<String> items = new ArrayList<>();
        for (JsonElement entry : entries.getAsJsonArray()) {
            if (entry.isJsonObject()) {
                String content = StreamJson.stringField(entry.getAsJsonObject(), "content");
                if (content != null && !content.isBlank()) {
                    items.add("- " + content.strip());
                }
            }
        }
        return items.isEmpty() ? null : "Plan:\n" + String.join("\n", items);
    }

    private MapResult describeEnd(JsonObject event) {
        recordUsage(StreamJson.objectField(event, "usage"));
        String sessionId = StreamJson.stringField(event, "sessionId");
        if (sessionId != null && !sessionId.isBlank()) {
            state.setSessionId(sessionId);
        }
        if (errored) {
            // An error event already carries the reason; leaving the answer unset lets the failure
            // output explain it instead of presenting the partial text as a finished answer.
            return MapResult.CONSUMED;
        }
        String stopReason = StreamJson.stringField(event, "stopReason");
        if (stopReason != null && !"end_turn".equals(stopReason)) {
            state.setTerminalDetail("Grok stopped early (" + stopReason + ").");
        }
        state.setAnswer(answer.toString().strip());
        return MapResult.CONSUMED;
    }
}
