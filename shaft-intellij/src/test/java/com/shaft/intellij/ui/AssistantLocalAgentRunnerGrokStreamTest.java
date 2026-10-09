package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #6748: a Grok assistant session must keep running and keep the user informed. Pins the live
 * stream (Grok's {@code streaming-json} events), the inactivity deadline that replaced the fixed
 * five-minute kill, the prompt file for long prompts and the visible reason for an interrupted run.
 */
class AssistantLocalAgentRunnerGrokStreamTest {

    @Test
    void grokRunsInItsLiveStreamingFormatWithoutUpdateChecks() {
        List<String> command = AssistantLocalAgentRunner.commandFor(arguments("short prompt"));

        assertEquals("grok", command.get(0), command.toString());
        assertTrue(command.containsAll(List.of("-p", "short prompt", "--output-format", "streaming-json",
                "--no-auto-update")), command.toString());
    }

    @Test
    void aLongPromptTravelsInATempFileThatIsRemovedAfterTheRun() throws Exception {
        String longPrompt = "x".repeat(AssistantLocalAgentRunner.GROK_MAX_ARGV_PROMPT_CHARS + 1);
        List<String> launched = new CopyOnWriteArrayList<>();

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                AssistantCommand.fromPrompt(longPrompt, "GROK", "ASK", ".", "", false), line -> { },
                (command, workingDirectory, environment) -> {
                    launched.addAll(command);
                    return new StubProcess(endEvent("done"), "", 0);
                }, false);
        running.future().get(5, TimeUnit.SECONDS);

        int flag = launched.indexOf("--prompt-file");
        assertTrue(flag >= 0, "Long prompts must not ride the command line (Windows limit): " + launched);
        assertFalse(launched.contains("-p"), launched.toString());
        assertFalse(Files.exists(Path.of(launched.get(flag + 1))), "The temp prompt file must be removed");
    }

    @Test
    void streamedEventsShowProgressLiveAndTheTextBecomesTheAnswer() throws Exception {
        String stdout = String.join("\n",
                "{\"type\":\"thought\",\"data\":\"Looking at \"}",
                "{\"type\":\"thought\",\"data\":\"the file\"}",
                "{\"type\":\"tool_call\",\"toolCallId\":\"c1\",\"toolName\":\"read_file\",\"rawInput\":{\"path\":\"src/Main.java\"}}",
                "{\"type\":\"tool_call_update\",\"toolCallId\":\"c1\",\"status\":\"completed\"}",
                "{\"type\":\"text\",\"data\":\"Hello wor\"}",
                "{\"type\":\"text\",\"data\":\"ld\\nsecond line\"}",
                "{\"type\":\"usage\",\"usage\":{\"input_tokens\":10,\"output_tokens\":4}}",
                "{\"type\":\"end\",\"stopReason\":\"end_turn\",\"sessionId\":\"s-1\"}") + "\n";
        List<String> live = new CopyOnWriteArrayList<>();

        ShaftMcpToolResult result = run(stdout, "", 0, live);

        assertTrue(result.success(), result.output());
        String streamed = String.join("\n", live);
        assertTrue(streamed.contains("Reasoning: Looking at the file"), streamed);
        assertTrue(streamed.contains("Calling tool read_file (src/Main.java)..."), streamed);
        assertTrue(streamed.contains("Hello world"), "A completed text line must stream before the run ends: " + streamed);
        assertTrue(streamed.contains("second line"), streamed);
        assertTrue(result.output().contains("Hello world\nsecond line"), result.output());
        assertFalse(result.output().contains("\"type\""), "Raw NDJSON must never reach the transcript");
    }

    @Test
    void anErrorEventGivesTheFailedRunAVisibleReasonAndResumeHint() throws Exception {
        String stdout = String.join("\n",
                "{\"type\":\"text\",\"data\":\"partial\"}",
                "{\"type\":\"error\",\"message\":\"Couldn't start session: quota exceeded\"}",
                "{\"type\":\"end\",\"stopReason\":\"error\",\"sessionId\":\"s-9\"}") + "\n";

        ShaftMcpToolResult result = run(stdout, "", 1, new CopyOnWriteArrayList<>());

        assertFalse(result.success());
        assertTrue(result.output().contains("quota exceeded"), result.output());
        assertTrue(result.output().contains("grok --resume s-9"), result.output());
    }

    @Test
    void outputKeepsAQuietRunAliveBeyondTheIdleLimit() throws Exception {
        AtomicLong lastActivity = new AtomicLong(System.nanoTime());
        FakeProcess process = new FakeProcess(450L);
        Thread activity = new Thread(() -> {
            for (int i = 0; i < 4; i++) {
                sleep(100);
                lastActivity.set(System.nanoTime());
            }
        });
        activity.start();

        boolean finished = AssistantLocalAgentRunner.awaitProcessWithIdleDeadline(process,
                Duration.ofMillis(250), Duration.ofSeconds(5), lastActivity::get, () -> 0L, 0L);
        activity.join();

        assertTrue(finished, "Output every 100ms must keep a 250ms-idle run alive well past 250ms");
    }

    @Test
    void aRunThatGoesSilentIsStoppedAfterTheIdleLimit() throws Exception {
        long start = System.nanoTime();

        boolean finished = AssistantLocalAgentRunner.awaitProcessWithIdleDeadline(new FakeProcess(60_000L),
                Duration.ofMillis(150), Duration.ofSeconds(5), () -> start, () -> 0L, 0L);

        assertFalse(finished);
        assertTrue(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start) < 2_000L);
    }

    @Test
    void theHardCeilingStillStopsARunThatNeverStopsStreaming() throws Exception {
        long start = System.nanoTime();

        boolean finished = AssistantLocalAgentRunner.awaitProcessWithIdleDeadline(new FakeProcess(60_000L),
                Duration.ofSeconds(30), Duration.ofMillis(200), System::nanoTime, () -> 0L, 0L);

        assertFalse(finished);
        assertTrue(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start) < 2_000L);
    }

    @Test
    void theInterruptedRunNoticeNamesTheReasonAndQuotesTheLastOutput() {
        String withOutput = ShaftAssistantPanel.interruptedRunNotice("Calling tool read_file...");
        String without = ShaftAssistantPanel.interruptedRunNotice("");

        assertTrue(withOutput.contains("panel was closed or reloaded"), withOutput);
        assertTrue(withOutput.contains("Calling tool read_file..."), withOutput);
        assertTrue(without.contains("Send your last prompt again"), without);
        assertFalse(without.contains("Last output"), without);
    }

    private static ShaftMcpToolResult run(String stdout, String stderr, int exitCode, List<String> live)
            throws Exception {
        StubProcess process = new StubProcess(stdout, stderr, exitCode);
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                AssistantCommand.fromPrompt("Explain", "GROK", "ASK", ".", "", false), live::add,
                (command, workingDirectory, environment) -> process, false);
        return running.future().get(5, TimeUnit.SECONDS);
    }

    private static JsonObject arguments(String prompt) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("client", "GROK");
        arguments.addProperty("mode", "ASK");
        arguments.addProperty("prompt", prompt);
        return arguments;
    }

    private static String endEvent(String text) {
        return "{\"type\":\"text\",\"data\":\"" + text + "\"}\n{\"type\":\"end\",\"stopReason\":\"end_turn\"}\n";
    }

    private static void sleep(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
        }
    }

    /** Replays fixed stdout/stderr and exits immediately with {@code exitCode}. */
    private static final class StubProcess extends Process {
        private final InputStream stdout;
        private final InputStream stderr;
        private final int exitCode;

        StubProcess(String stdout, String stderr, int exitCode) {
            this.stdout = new ByteArrayInputStream(stdout.getBytes(StandardCharsets.UTF_8));
            this.stderr = new ByteArrayInputStream(stderr.getBytes(StandardCharsets.UTF_8));
            this.exitCode = exitCode;
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
            return true;
        }

        @Override
        public int exitValue() {
            return exitCode;
        }

        @Override
        public void destroy() {
            // Completed-run stub: nothing to stop.
        }

        @Override
        public boolean isAlive() {
            return false;
        }
    }

    /** Reports "still running" until {@code finishAfterMillis} of real time has passed. */
    private static final class FakeProcess extends Process {
        private final long finishAfterMillis;
        private final long startNanos = System.nanoTime();

        FakeProcess(long finishAfterMillis) {
            this.finishAfterMillis = finishAfterMillis;
        }

        @Override
        public boolean waitFor(long timeout, TimeUnit unit) throws InterruptedException {
            Thread.sleep(Math.min(unit.toMillis(timeout), 20L));
            return TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos) >= finishAfterMillis;
        }

        @Override
        public OutputStream getOutputStream() {
            return OutputStream.nullOutputStream();
        }

        @Override
        public InputStream getInputStream() {
            return InputStream.nullInputStream();
        }

        @Override
        public InputStream getErrorStream() {
            return InputStream.nullInputStream();
        }

        @Override
        public int waitFor() {
            return 0;
        }

        @Override
        public int exitValue() {
            return 0;
        }

        @Override
        public void destroy() {
            // Not exercised.
        }

        @Override
        public boolean isAlive() {
            return true;
        }
    }
}
