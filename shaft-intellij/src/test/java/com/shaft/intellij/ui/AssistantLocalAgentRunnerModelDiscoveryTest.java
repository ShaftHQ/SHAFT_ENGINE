package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers model-name discovery: {@link AssistantLocalAgentRunner#parseModelNames} (and the
 * {@code collectModelNames} JSON walker it drives), plus {@link AssistantLocalAgentRunner#listModels}
 * (both overloads). {@code listModels(JsonObject)} and {@code parseModelNames}'s JSON branch mirror
 * the real shapes a CLI's model-listing command can emit: a bare JSON array of strings, an array of
 * objects keyed by {@code id}/{@code name}/{@code model}, or plain newline-delimited text.
 */
class AssistantLocalAgentRunnerModelDiscoveryTest {

    // -- parseModelNames / collectModelNames: JSON array and object shapes --------------------

    @Test
    void jsonArrayOfPlainStringsIsReturnedAsIs() {
        List<String> models = AssistantLocalAgentRunner.parseModelNames("[\"claude-3-opus\",\"claude-3-sonnet\"]");

        assertEquals(List.of("claude-3-opus", "claude-3-sonnet"), models);
    }

    @Test
    void jsonArrayOfObjectsPrefersIdThenNameThenModelField() {
        List<String> models = AssistantLocalAgentRunner.parseModelNames(
                "[{\"id\":\"gpt-4\"},{\"name\":\"custom-name-model\"},{\"model\":\"model-field-value\"}]");

        assertEquals(List.of("gpt-4", "custom-name-model", "model-field-value"), models);
    }

    @Test
    void objectWithBothIdAndNamePrefersId() {
        List<String> models = AssistantLocalAgentRunner.parseModelNames(
                "[{\"id\":\"preferred-id\",\"name\":\"ignored-name\"}]");

        assertEquals(List.of("preferred-id"), models);
    }

    @Test
    void nestedJsonArraysAreFlattenedRecursively() {
        List<String> models = AssistantLocalAgentRunner.parseModelNames(
                "[[\"nested-a\",\"nested-b\"],\"top-level\"]");

        assertEquals(List.of("nested-a", "nested-b", "top-level"), models);
    }

    @Test
    void nullElementsInAJsonArrayAreSkippedWithoutCrashing() {
        List<String> models = AssistantLocalAgentRunner.parseModelNames("[null,\"claude-3-opus\"]");

        assertEquals(List.of("claude-3-opus"), models);
    }

    @Test
    void aBareTopLevelJsonObjectIsAlsoParsedNotJustArrays() {
        List<String> models = AssistantLocalAgentRunner.parseModelNames("{\"id\":\"single-object-model\"}");

        assertEquals(List.of("single-object-model"), models);
    }

    @Test
    void numberEntriesDirectlyInAnArrayContributeNothing() {
        // Bare JSON numbers are neither strings nor objects, so collectModelNames silently skips
        // them instead of stringifying them into a bogus "model name".
        List<String> models = AssistantLocalAgentRunner.parseModelNames("[1, 2, \"claude-3-opus\"]");

        assertEquals(List.of("claude-3-opus"), models);
    }

    @Test
    void duplicateModelNamesCollapseToOneEntry() {
        List<String> models = AssistantLocalAgentRunner.parseModelNames("[\"claude-3-opus\",\"claude-3-opus\"]");

        assertEquals(List.of("claude-3-opus"), models);
    }

    /**
     * Edge case reachable in real output: a JSON object/array whose entries carry none of the
     * recognized {@code id}/{@code name}/{@code model} keys. {@code collectModelNames} then leaves
     * {@code models} empty, so {@code parseModelNames} falls through to its line-based parser on the
     * ORIGINAL (still-JSON) text rather than returning nothing outright. Here the whole one-line,
     * space-free JSON blob has no recognized model-name pattern and no spaces, so it is kept verbatim
     * as a "bare token" -- an accurate (if surprising) trace of the real fallback behavior.
     */
    @Test
    void jsonWithNoRecognizedModelFieldFallsBackToLineParsingOfTheRawText() {
        List<String> models = AssistantLocalAgentRunner.parseModelNames("{\"unrelated\":\"field\"}");

        assertEquals(List.of("{\"unrelated\":\"field\"}"), models);
    }

    @Test
    void malformedJsonStartingWithABracketFallsBackToLineBasedParsing() {
        // Invalid JSON that still starts with '[': JsonParser throws, caught, and parsing continues
        // as plain text. This line has a space and no recognized model pattern, so it yields nothing.
        List<String> models = AssistantLocalAgentRunner.parseModelNames("[not valid json");

        assertEquals(List.of(), models);
    }

    // -- parseModelNames: plain newline-delimited text branch ---------------------------------

    @Test
    void plainTextLinesExtractKnownModelPatternsAndKeepShortBareTokens() {
        String output = String.join("\n",
                "Available models:",
                "- claude-3-opus-latest (recommended)",
                "- gpt-4o",
                "* o1-preview",
                "random text line with spaces",
                "bareToken");

        List<String> models = AssistantLocalAgentRunner.parseModelNames(output);

        assertEquals(List.of("claude-3-opus-latest", "gpt-4o", "o1-preview", "bareToken"), models);
    }

    @Test
    void blankOrNullInputReturnsAnEmptyList() {
        assertTrue(AssistantLocalAgentRunner.parseModelNames(null).isEmpty());
        assertTrue(AssistantLocalAgentRunner.parseModelNames("   \n  ").isEmpty());
    }

    // -- listModels(JsonObject, ProcessLauncher) -----------------------------------------------

    @Test
    void listModelsParsesTheStubProcessJsonStdoutOnASuccessfulExit() {
        StubProcess process = new StubProcess("[\"gpt-5\",\"o1-mini\"]", "", 0, true);

        List<String> models = AssistantLocalAgentRunner.listModels(
                arguments("CODEX"), (command, workingDirectory, environment) -> process);

        assertEquals(List.of("gpt-5", "o1-mini"), models);
    }

    @Test
    void listModelsDestroysForciblyAndReturnsEmptyOnTimeout() {
        StubProcess process = new StubProcess("[\"gpt-5\"]", "", 0, false);

        List<String> models = AssistantLocalAgentRunner.listModels(
                arguments("CODEX"), (command, workingDirectory, environment) -> process);

        assertTrue(models.isEmpty(), "A timed-out probe must yield no models: " + models);
        assertTrue(process.destroyForciblyCalled(), "A timed-out probe must force-kill the process");
    }

    @Test
    void listModelsIgnoresStdoutAndReturnsEmptyOnANonZeroExit() {
        StubProcess process = new StubProcess("[\"gpt-5\",\"o1-mini\"]", "boom", 1, true);

        List<String> models = AssistantLocalAgentRunner.listModels(
                arguments("CODEX"), (command, workingDirectory, environment) -> process);

        assertTrue(models.isEmpty(), "A non-zero exit must never surface stdout as model names: " + models);
    }

    @Test
    void listModelsReturnsEmptyWhenSuccessfulExitHasBlankStdout() {
        StubProcess process = new StubProcess("   \n  ", "", 0, true);

        List<String> models = AssistantLocalAgentRunner.listModels(
                arguments("CODEX"), (command, workingDirectory, environment) -> process);

        assertTrue(models.isEmpty(), models.toString());
    }

    @Test
    void listModelsReturnsEmptyWhenTheProcessLauncherThrows() {
        List<String> models = AssistantLocalAgentRunner.listModels(arguments("CODEX"),
                (command, workingDirectory, environment) -> {
                    throw new java.io.IOException("simulated launch failure");
                });

        assertTrue(models.isEmpty(), models.toString());
    }

    // -- listModels(JsonObject): PATH-availability short-circuit --------------------------------

    /**
     * {@code listModels(JsonObject)}'s only branch safely testable without spawning a real CLI
     * subprocess is the PATH-unavailable short-circuit: the Copilot CLI is confirmed absent from
     * this machine's PATH (unlike {@code claude}/{@code codex}, which ARE actually installed here),
     * so this deterministically exercises {@code !isCommandAvailable(...)} without ever reaching
     * the real-process delegation. The "CLI is available" branch (AssistantLocalAgentRunner.java
     * lines 1285-1291, delegating to the two-arg overload via the real {@code launchProcess}) is not
     * exercised here: on a machine with claude/codex actually on PATH, doing so would spawn a real
     * CLI subprocess with unpredictable/slow/networked behavior, which is exactly the kind of fragile
     * test this task said to avoid instead of chase.
     */
    @Test
    void listModelsSingleArgShortCircuitsToEmptyListWhenCliIsNotOnPath() {
        JsonObject arguments = arguments("COPILOT_CLI");

        List<String> models = AssistantLocalAgentRunner.listModels(arguments);

        assertTrue(models.isEmpty(), models.toString());
    }

    private static JsonObject arguments(String client) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("client", client);
        arguments.addProperty("workingDirectory", ".");
        return arguments;
    }

    /**
     * Minimal stub {@link Process} that replays fixed stdout/stderr content, an exit code, and
     * whether {@link #waitFor(long, TimeUnit)} reports finished or timed out -- tracking whether
     * {@link #destroyForcibly()} was invoked so timeout-handling can be asserted directly.
     */
    private static final class StubProcess extends Process {
        private final InputStream stdout;
        private final InputStream stderr;
        private final int exitCode;
        private final boolean finishes;
        private final AtomicBoolean destroyForciblyCalled = new AtomicBoolean();

        StubProcess(String stdout, String stderr, int exitCode, boolean finishes) {
            this.stdout = new ByteArrayInputStream(stdout.getBytes(StandardCharsets.UTF_8));
            this.stderr = new ByteArrayInputStream(stderr.getBytes(StandardCharsets.UTF_8));
            this.exitCode = exitCode;
            this.finishes = finishes;
        }

        boolean destroyForciblyCalled() {
            return destroyForciblyCalled.get();
        }

        @Override
        public OutputStream getOutputStream() {
            return new ByteArrayOutputStream();
        }

        @Override
        public InputStream getInputStream() {
            return stdout;
        }

        @Override
        public InputStream getErrorStream() {
            return stderr;
        }

        @Override
        public int waitFor() {
            return exitCode;
        }

        @Override
        public boolean waitFor(long timeout, TimeUnit unit) {
            return finishes;
        }

        @Override
        public int exitValue() {
            return exitCode;
        }

        @Override
        public void destroy() {
            // Not exercised: listModels only ever force-destroys on timeout.
        }

        @Override
        public Process destroyForcibly() {
            destroyForciblyCalled.set(true);
            return this;
        }

        @Override
        public boolean isAlive() {
            return !finishes;
        }
    }
}
