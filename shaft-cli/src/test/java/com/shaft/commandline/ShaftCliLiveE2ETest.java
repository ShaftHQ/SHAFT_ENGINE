package com.shaft.commandline;

import com.shaft.commandline.util.Json;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * End-to-end exercise of {@code shaft-cli} as a real subprocess (issue #3872, tracked by #3866
 * T6): the CLI companion to the IntelliJ-panel live suite ({@code
 * com.shaft.intellij.ui.ShaftAssistantPanelLive*ToolE2ETest}), covering the deterministic
 * one-shot commands ({@code tools --cached}, {@code codegen}, {@code session status}) plus the
 * live-server commands ({@code tools}, {@code call}, {@code doctor analyze}).
 *
 * <p>Gated the SAME way as the whole T6 effort: {@code -Dshaft.intellij.liveToolE2E=true} (absent
 * or false skips every test here, so normal CI stays fast/green). The live-server tests
 * additionally require {@code -Dshaft.intellij.liveMcpCommand} pointing at a freshly built
 * shaft-mcp launcher (a {@code java @<args-file>} command, same property CI's
 * {@code guided-workflows-live.yml} recipe and the IntelliJ live suite already use) -- this test
 * passes that command to the spawned {@code shaft-cli} process via the {@code SHAFT_MCP_JAR}
 * environment variable ({@code McpLauncherLocator.tryEnvOverride}, checked first, before the
 * per-user installed-versions directory). Skipping that override matters: this machine has a
 * pre-existing {@code %LOCALAPPDATA%\ShaftHQ\shaft-mcp\versions\} installation from BEFORE the
 * 89-tool sweep (confirmed by inspecting its {@code tools} output -- it still lists deleted names
 * like {@code natural_act}/{@code playwright_initialize} as separate tools), so leaving {@code
 * SHAFT_MCP_JAR} unset would silently validate a stale catalog instead of this PR's current
 * 89-tool build.
 */
class ShaftCliLiveE2ETest {

    private static final String GATE_PROPERTY = "shaft.intellij.liveToolE2E";
    private static final String MCP_COMMAND_PROPERTY = "shaft.intellij.liveMcpCommand";

    @Test
    void toolsCachedOfflineListsToolsWithNoServer(@TempDir Path tempDir) throws IOException {
        assumeGateEnabled();

        ProcessResult result = runShaftCli(tempDir, Map.of(), "tools", "--cached");
        assertEquals(0, result.exitCode, "tools --cached should succeed: " + result.stderr);
        assertTrue(result.stdout.contains("browser_navigate"),
                "tools --cached should list the current 89-tool catalog: " + result.stdout);

        recordExample(tempDir, "tools-cached", "tools --cached", result);
    }

    @Test
    void toolsLiveConnectsToTheCurrentBuildServer(@TempDir Path tempDir) throws IOException {
        Map<String, String> serverEnv = assumeLiveServerConfigured();

        ProcessResult result = runShaftCli(tempDir, serverEnv, "tools");
        assertEquals(0, result.exitCode, "tools should succeed against the live server: " + result.stderr);
        assertTrue(result.stdout.contains("browser_navigate") && result.stdout.contains("element_click"),
                "tools (live) should list the current 89-tool catalog: " + result.stdout);
        assertTrue(!result.stdout.contains("natural_act") && !result.stdout.contains("mobile_natural_act"),
                "tools (live) must not list the tools deleted by the sweep: " + result.stdout);

        recordExample(tempDir, "tools-live", "tools", result);
    }

    @Test
    void callInvokesAToolAgainstTheLiveServer(@TempDir Path tempDir) throws IOException {
        Map<String, String> serverEnv = assumeLiveServerConfigured();

        // autobot_local_agent_clients is a zero-arg, zero-network, zero-credential tool -- safe
        // representative for the live `call` path (mirrors the IntelliJ suite's proof-of-concept).
        ProcessResult result = runShaftCli(tempDir, serverEnv, "call", "autobot_local_agent_clients");
        assertEquals(0, result.exitCode, "call autobot_local_agent_clients should succeed: " + result.stderr);
        assertTrue(result.stdout.contains("CODEX") && result.stdout.contains("CLAUDE_CODE"),
                "Expected the built-in local agent clients: " + result.stdout);

        recordExample(tempDir, "call-tool", "call autobot_local_agent_clients", result);
    }

    @Test
    void doctorAnalyzeWithNoTraceFileReportsAnHonestFailure(@TempDir Path tempDir) throws IOException {
        Map<String, String> serverEnv = assumeLiveServerConfigured();

        // `shaft-cli doctor analyze` is a curated alias for doctor_analyze_trace (DoctorCommand.java:26)
        // -- NOT doctor_analyze_failed_allure, confirmed by reading the source after an earlier attempt
        // using allureResultPaths= failed with a JSON-schema validation error naming tracePath/backend
        // as the real required params. No trace file exists in tempDir, so this is a real,
        // honestly-reported failure, not a fabricated success.
        ProcessResult result = runShaftCli(tempDir, serverEnv, "doctor", "analyze",
                "tracePath=missing-trace.zip", "backend=web");
        assertTrue(result.exitCode != 0, "doctor analyze with no trace file should fail, not fabricate a report");
        assertTrue(result.stderr.contains("Trace path was not found") || result.stdout.contains("Trace path was not found"),
                "Expected the tool's own file-not-found message: stdout=" + result.stdout
                        + " stderr=" + result.stderr);

        recordExample(tempDir, "doctor-analyze", "doctor analyze tracePath=missing-trace.zip backend=web", result);
    }

    @Test
    void codegenWithoutASessionReportsAClearUsageError(@TempDir Path tempDir) throws IOException {
        assumeGateEnabled();

        // codegen is deterministic and needs no live server; with no --session and no recordings/
        // directory it must fail with a clear, real usage error, never a fabricated result.
        ProcessResult result = runShaftCli(tempDir, Map.of(), "codegen");
        assertTrue(result.exitCode != 0, "codegen with no session should fail, not silently no-op");
        assertTrue(result.stderr.contains("--session") || result.stderr.contains("recordings"),
                "Expected a clear missing-session error: " + result.stderr);

        recordExample(tempDir, "codegen-error", "codegen (no args, no recordings)", result);
    }

    @Test
    void sessionStatusReportsCleanlyWithNoDaemonRunning(@TempDir Path tempDir) throws IOException {
        assumeGateEnabled();

        ProcessResult result = runShaftCli(tempDir, Map.of(), "session", "status");
        assertEquals(0, result.exitCode, "session status should always succeed: " + result.stderr);
        assertTrue(result.stdout.toLowerCase(java.util.Locale.ROOT).contains("no")
                        || result.stdout.toLowerCase(java.util.Locale.ROOT).contains("not"),
                "Expected a clean 'no session' report: " + result.stdout);

        recordExample(tempDir, "session-status", "session status", result);
    }

    private static void assumeGateEnabled() {
        Assumptions.assumeTrue(Boolean.getBoolean(GATE_PROPERTY),
                "Set -D" + GATE_PROPERTY + "=true to run the live shaft-cli E2E suite.");
    }

    /**
     * @return an environment map with {@code SHAFT_MCP_JAR} pointing at the args file named by
     *         {@code -Dshaft.intellij.liveMcpCommand}, so the spawned {@code shaft-cli} process
     *         launches the CURRENT build's shaft-mcp server rather than any per-user installed
     *         version (see the class doc for why that distinction matters on this machine)
     */
    private static Map<String, String> assumeLiveServerConfigured() {
        assumeGateEnabled();
        String mcpCommand = System.getProperty(MCP_COMMAND_PROPERTY, "").trim();
        Assumptions.assumeTrue(!mcpCommand.isBlank(),
                "Set -D" + MCP_COMMAND_PROPERTY + " to a SHAFT MCP stdio command (java @<args-file>) built from "
                        + "this checkout, so shaft-cli connects to the current tool catalog, not any pre-existing "
                        + "per-user shaft-mcp installation.");
        int atIndex = mcpCommand.indexOf('@');
        Assumptions.assumeTrue(atIndex >= 0,
                "Expected " + MCP_COMMAND_PROPERTY + " to reference an @<args-file>: " + mcpCommand);
        String argsFile = mcpCommand.substring(atIndex + 1).trim();
        Assumptions.assumeTrue(Files.isRegularFile(Path.of(argsFile)),
                "Args file from " + MCP_COMMAND_PROPERTY + " does not exist: " + argsFile);
        Map<String, String> env = new HashMap<>();
        env.put("SHAFT_MCP_JAR", argsFile);
        return env;
    }

    /**
     * Spawns shaft-cli as a real child process with the given arguments and environment overlay,
     * captures stdout and stderr, and returns the result.
     */
    private ProcessResult runShaftCli(Path tempDir, Map<String, String> extraEnvironment, String... args)
            throws IOException {
        List<String> command = buildCommand(tempDir, args);
        ProcessBuilder processBuilder = new ProcessBuilder(command);
        processBuilder.directory(tempDir.toFile());
        processBuilder.environment().putAll(extraEnvironment);
        Process process = processBuilder.start();

        String stdout = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        String stderr = new String(process.getErrorStream().readAllBytes(), StandardCharsets.UTF_8);

        int exitCode;
        try {
            exitCode = process.waitFor();
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            process.destroy();
            throw new IllegalStateException("shaft-cli process wait was interrupted", interrupted);
        }

        return new ProcessResult(exitCode, stdout, stderr);
    }

    /**
     * Builds the child command as {@code [java, @argsFile]} with the shaft-cli main class on the
     * classpath, using the same pattern as {@link com.shaft.commandline.mcp.ShaftCliEndToEndStdioIT}.
     * The {@code @argfile} avoids Windows' CreateProcess command-line length limit.
     */
    private static List<String> buildCommand(Path tempDir, String[] args) throws IOException {
        List<String> fullArgs = new ArrayList<>();
        fullArgs.add(ShaftCli.class.getName());
        fullArgs.addAll(List.of(args));

        Path argsFile = tempDir.resolve("shaft-cli-e2e.args");
        String classpath = System.getProperty("java.class.path").replace("\\", "/");
        StringBuilder content = new StringBuilder("-cp").append(System.lineSeparator())
                .append('"').append(classpath.replace("\"", "\\\"")).append('"').append(System.lineSeparator());
        for (String arg : fullArgs) {
            content.append(arg).append(System.lineSeparator());
        }
        Files.writeString(argsFile, content, StandardCharsets.UTF_8);

        return List.of(javaExecutable(), "@" + argsFile);
    }

    private static String javaExecutable() {
        return ProcessHandle.current().info().command()
                .filter(command -> command.toLowerCase(java.util.Locale.ROOT).contains("java"))
                .orElseGet(() -> {
                    String executable = System.getProperty("os.name", "").toLowerCase(java.util.Locale.ROOT)
                            .contains("win") ? "java.exe" : "java";
                    return Path.of(System.getProperty("java.home"), "bin", executable).toString();
                });
    }

    /**
     * Records the exact {@code {request, response}} pair this call produced against the real
     * shaft-cli process into {@code shaft-cli/build/live-tool-e2e-examples/<command>.json}, mirroring
     * the IntelliJ suite's recording mechanism so both halves of this issue capture evidence the
     * same way.
     */
    private static void recordExample(Path tempDir, String commandName, String commandLine, ProcessResult result)
            throws IOException {
        Path examplesDir = Path.of("build", "live-tool-e2e-examples").toAbsolutePath().normalize();
        Files.createDirectories(examplesDir);

        ObjectNode example = Json.newObject();
        example.put("tool", "shaft-cli");
        example.put("command", commandName);
        example.put("request", commandLine);
        ObjectNode response = example.putObject("response");
        response.put("exitCode", result.exitCode);
        response.put("stdout", result.stdout);
        response.put("stderr", result.stderr);

        Files.writeString(examplesDir.resolve(commandName + ".json"),
                Json.MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(example), StandardCharsets.UTF_8);
    }

    private record ProcessResult(int exitCode, String stdout, String stderr) {
    }
}
