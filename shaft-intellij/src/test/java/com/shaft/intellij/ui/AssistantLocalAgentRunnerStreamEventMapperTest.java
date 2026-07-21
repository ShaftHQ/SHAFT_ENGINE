package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit-level pins for the mapper-contract seam introduced by issue #3974's restructuring:
 * {@link MapResult}'s three-state {@code Rendered}/{@code Consumed}/{@code Unknown} result, and
 * {@link ClaudeStreamEventMapper}/{@link CodexStreamEventMapper}'s {@link StreamEventMapper#map}
 * implementations. These exercise the mappers directly (bypassing {@link
 * AssistantLocalAgentRunner.StructuredStreamParser#accept}'s line-parsing/consumer plumbing,
 * already covered end-to-end by {@code AssistantLocalAgentRunnerVerboseStreamTest}) so a future
 * change to the JSON-to-{@code MapResult} translation itself fails at the smallest possible unit.
 */
class AssistantLocalAgentRunnerStreamEventMapperTest {

    // -- MapResult itself ------------------------------------------------------------------

    @Test
    void renderedRejectsANullText() {
        assertThrows(NullPointerException.class, () -> MapResult.rendered(null),
                "Rendered must never silently wrap a null line -- accept()'s raw-passthrough branch "
                        + "as opposed to Consumed's absorbed-into-state branch relies on Rendered "
                        + "always carrying real text.");
    }

    @Test
    void consumedAndUnknownAreSharedSingletonsNotFreshAllocationsPerCall() {
        assertSame(MapResult.CONSUMED, MapResult.CONSUMED);
        assertSame(MapResult.UNKNOWN, MapResult.UNKNOWN);
    }

    // -- ClaudeStreamEventMapper -------------------------------------------------------------

    @Test
    void claudeMapperRendersAThinkingBlock() {
        ClaudeStreamEventMapper mapper = new ClaudeStreamEventMapper(claudeState());

        MapResult result = mapper.map(parse(
                "{\"type\":\"assistant\",\"message\":{\"content\":"
                        + "[{\"type\":\"thinking\",\"thinking\":\"weighing options\"}]}}"));

        assertEquals(MapResult.rendered("Thinking: weighing options"), result);
    }

    @Test
    void claudeMapperReturnsUnknownWhenNoAssistantBlockIsRecognized() {
        ClaudeStreamEventMapper mapper = new ClaudeStreamEventMapper(claudeState());

        MapResult result = mapper.map(parse(
                "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"some_future_block\"}]}}"));

        assertInstanceOf(MapResult.Unknown.class, result);
    }

    @Test
    void claudeMapperReturnsUnknownForStillUnpinnedSystemSubtypes() {
        // Issue #3974 explicitly keeps api_retry/plugin_install/hook_* as raw passthrough: their
        // field shapes were never confirmed against a real Claude Code session, so rendering them
        // is out of scope for this restructuring -- pin that they stay Unknown, not Rendered.
        ClaudeStreamEventMapper mapper = new ClaudeStreamEventMapper(claudeState());

        for (String subtype : new String[] {"api_retry", "plugin_install", "hook_started", "hook_progress", "hook_response"}) {
            MapResult result = mapper.map(parse("{\"type\":\"system\",\"subtype\":\"" + subtype + "\"}"));
            assertInstanceOf(MapResult.Unknown.class, result, subtype + " must still be raw passthrough");
        }
    }

    @Test
    void claudeMapperConsumesTheResultEventAndUpdatesAccumulatedState() {
        AssistantLocalAgentRunner.StructuredStreamParser state = claudeState();
        ClaudeStreamEventMapper mapper = new ClaudeStreamEventMapper(state);

        MapResult result = mapper.map(parse(
                "{\"type\":\"result\",\"result\":\"the final answer\","
                        + "\"usage\":{\"input_tokens\":7,\"output_tokens\":3}}"));

        assertSame(MapResult.CONSUMED, result);
        assertEquals("the final answer", state.currentAnswer());
        assertEquals(7, state.currentInputTokens());
        assertEquals(3, state.currentOutputTokens());
    }

    @Test
    void claudeMapperReturnsUnknownForAnUnrecognizedTopLevelType() {
        ClaudeStreamEventMapper mapper = new ClaudeStreamEventMapper(claudeState());

        MapResult result = mapper.map(parse("{\"type\":\"some_future_top_level_type\"}"));

        assertInstanceOf(MapResult.Unknown.class, result);
    }

    // -- CodexStreamEventMapper ---------------------------------------------------------------

    @Test
    void codexMapperReturnsUnknownForTheStaleToolCallItemType() {
        // Issue #3922: "tool_call" is not a real Codex --json item variant (the real ones are
        // command_execution/mcp_tool_call/collab_tool_call); pin that it never renders.
        CodexStreamEventMapper mapper = new CodexStreamEventMapper(codexState());

        MapResult result = mapper.map(parse(
                "{\"type\":\"item.completed\",\"item\":{\"type\":\"tool_call\",\"name\":\"shell\"}}"));

        assertInstanceOf(MapResult.Unknown.class, result);
    }

    @Test
    void codexMapperRendersACompletedCommandExecutionItem() {
        CodexStreamEventMapper mapper = new CodexStreamEventMapper(codexState());

        MapResult result = mapper.map(parse(
                "{\"type\":\"item.completed\",\"item\":{\"type\":\"command_execution\",\"command\":\"ls\"}}"));

        assertInstanceOf(MapResult.Rendered.class, result);
        assertTrue(((MapResult.Rendered) result).text().contains("Calling tool ls"));
    }

    @Test
    void codexMapperConsumesTurnCompletedAndUpdatesTheAnswer() {
        AssistantLocalAgentRunner.StructuredStreamParser state = codexState();
        CodexStreamEventMapper mapper = new CodexStreamEventMapper(state);

        MapResult result = mapper.map(parse(
                "{\"type\":\"turn.completed\",\"last_agent_message\":\"done\","
                        + "\"usage\":{\"input_tokens\":10,\"output_tokens\":2}}"));

        assertSame(MapResult.CONSUMED, result);
        assertEquals("done", state.currentAnswer());
        assertEquals(10, state.currentInputTokens());
        assertEquals(2, state.currentOutputTokens());
    }

    @Test
    void codexMapperRendersAFatalTopLevelErrorWithAMessage() {
        CodexStreamEventMapper mapper = new CodexStreamEventMapper(codexState());

        MapResult result = mapper.map(parse("{\"type\":\"error\",\"message\":\"sandbox denied\"}"));

        assertEquals(MapResult.rendered("Error: sandbox denied"), result);
    }

    @Test
    void codexMapperReturnsUnknownForATopLevelErrorWithoutAMessage() {
        CodexStreamEventMapper mapper = new CodexStreamEventMapper(codexState());

        MapResult result = mapper.map(parse("{\"type\":\"error\"}"));

        assertInstanceOf(MapResult.Unknown.class, result);
    }

    @Test
    void codexMapperReturnsUnknownForAnUnrecognizedTopLevelType() {
        CodexStreamEventMapper mapper = new CodexStreamEventMapper(codexState());

        MapResult result = mapper.map(parse("{\"type\":\"some_future_top_level_type\"}"));

        assertInstanceOf(MapResult.Unknown.class, result);
    }

    private static JsonObject parse(String json) {
        return JsonParser.parseString(json).getAsJsonObject();
    }

    private static AssistantLocalAgentRunner.StructuredStreamParser claudeState() {
        return new AssistantLocalAgentRunner.StructuredStreamParser(
                AssistantLocalAgentRunner.StructuredStreamParser.Format.CLAUDE);
    }

    private static AssistantLocalAgentRunner.StructuredStreamParser codexState() {
        return new AssistantLocalAgentRunner.StructuredStreamParser(
                AssistantLocalAgentRunner.StructuredStreamParser.Format.CODEX);
    }
}
