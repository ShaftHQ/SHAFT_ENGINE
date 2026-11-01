package com.shaft.pilot.agent;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Package-visible coverage for {@link JdkLocalAgentProcessRunner} (issue #7081).
 */
class JdkLocalAgentProcessRunnerTest {

    @TempDir
    Path temp;

    @Test
    void runShouldCaptureStdoutExitCodeAndRespectWorkingDirectory() {
        JdkLocalAgentProcessRunner runner = new JdkLocalAgentProcessRunner();
        LocalAgentProcessResult result = runner.run(
                List.of("bash", "-lc", "pwd; printf 'out'; echo err 1>&2; exit 3"),
                temp,
                Map.of(),
                "",
                Duration.ofSeconds(10));

        assertEquals(3, result.exitCode());
        assertTrue(result.stdout().contains(temp.toAbsolutePath().normalize().toString())
                || result.stdout().contains(temp.toString()), result.stdout());
        assertTrue(result.stdout().contains("out"), result.stdout());
        assertTrue(result.stderr().contains("err"), result.stderr());
        assertFalse(result.timedOut());
        assertTrue(result.duration().toMillis() >= 0);
    }

    @Test
    void runShouldFeedStdinAndTimeOutLongCommands() {
        JdkLocalAgentProcessRunner runner = new JdkLocalAgentProcessRunner();
        LocalAgentProcessResult echoed = runner.run(
                List.of("bash", "-lc", "cat"),
                temp,
                Map.of("COVERAGE_ENV", "1"),
                "stdin-payload",
                Duration.ofSeconds(5));
        assertEquals(0, echoed.exitCode());
        assertTrue(echoed.stdout().contains("stdin-payload"), echoed.stdout());

        LocalAgentProcessResult timedOut = runner.run(
                List.of("bash", "-lc", "sleep 30"),
                temp,
                Map.of(),
                "",
                Duration.ofMillis(200));
        assertTrue(timedOut.timedOut());
        assertEquals(-1, timedOut.exitCode());
    }

    @Test
    void runShouldReturnFailedResultForMissingExecutable() {
        JdkLocalAgentProcessRunner runner = new JdkLocalAgentProcessRunner();
        LocalAgentProcessResult result = runner.run(
                List.of(temp.resolve("missing-binary-for-coverage").toString()),
                temp,
                Map.of(),
                "",
                Duration.ofSeconds(2));
        assertEquals(-1, result.exitCode());
        assertFalse(result.timedOut());
        assertFalse(result.stderr().isBlank());
    }
}
