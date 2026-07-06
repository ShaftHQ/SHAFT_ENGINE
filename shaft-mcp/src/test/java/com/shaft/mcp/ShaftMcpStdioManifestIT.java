package com.shaft.mcp;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Boots the real {@link ShaftMcpApplication} in a child JVM over stdio, launched from a working
 * directory outside the SHAFT_ENGINE repository, and asserts that {@code tools/list} always
 * reports the guidance/codegen tools alongside the runtime tools. This is the packaged-jar
 * equivalent of {@link ShaftMcpApplicationTests#shaftToolsBeanAlwaysRegistersGuidanceAndCodegenTools()}
 * and is the source of the tool-listing evidence captured for this change.
 *
 * <p>Tagged {@code external-e2e} because it forks an additional JVM process; run it explicitly with
 * {@code mvn -pl shaft-mcp test -DincludeMcpBrowserE2E -Dtest=ShaftMcpStdioManifestIT}.</p>
 */
@Tag("external-e2e")
class ShaftMcpStdioManifestIT {
    private static final Set<String> REQUIRED_GUIDANCE_TOOL_NAMES = Set.of(
            "shaft_guide_search",
            "test_automation_scenarios",
            "shaft_coding_partner_plan",
            "test_code_guardrails_check");
    private static final Duration STARTUP_TIMEOUT = Duration.ofSeconds(60);
    private static final ObjectMapper JSON = new ObjectMapper();

    @TempDir
    Path launchDirectoryOutsideRepo;

    @Test
    void toolsListOverStdioAlwaysIncludesGuidanceAndCodegenTools() throws Exception {
        assertTrue(Files.isDirectory(launchDirectoryOutsideRepo));

        Path argsFile = writeLaunchArgsFile();
        List<String> command = List.of(javaExecutable(), "@" + argsFile);

        ProcessBuilder builder = new ProcessBuilder(command);
        builder.directory(launchDirectoryOutsideRepo.toFile());
        builder.environment().remove("SHAFT_MCP_WORKSPACE_ROOT");
        Process process = builder.start();
        ExecutorService ioExecutor = Executors.newFixedThreadPool(2);
        try {
            CompletableFuture<String> stderr = CompletableFuture.supplyAsync(
                    () -> readAll(process.getErrorStream()), ioExecutor);
            try (BufferedReader input = new BufferedReader(
                    new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8));
                 OutputStream output = process.getOutputStream()) {

                send(output, initializeRequest(1));
                readResponseFor(input, ioExecutor, 1, STARTUP_TIMEOUT, process, stderr);
                send(output, initializedNotification());

                send(output, toolsListRequest(2));
                JsonNode result = readResponseFor(input, ioExecutor, 2, STARTUP_TIMEOUT, process, stderr);

                Set<String> toolNames = toolNames(result);
                writeToolListingEvidence(result);

                Set<String> missing = REQUIRED_GUIDANCE_TOOL_NAMES.stream()
                        .filter(name -> !toolNames.contains(name))
                        .collect(java.util.stream.Collectors.toSet());
                assertTrue(missing.isEmpty(),
                        "Packaged shaft-mcp tools/list run from outside the repo (" + launchDirectoryOutsideRepo
                                + ") is missing guidance/codegen tools: " + missing
                                + ". Full tools/list result: " + result);
                assertTrue(toolNames.size() > REQUIRED_GUIDANCE_TOOL_NAMES.size(),
                        "Expected runtime tools alongside the guidance/codegen tools, found only: " + toolNames);
            }
        } finally {
            process.destroyForcibly();
            process.waitFor(5, TimeUnit.SECONDS);
            ioExecutor.shutdownNow();
            Files.deleteIfExists(argsFile);
        }
    }

    private static String javaExecutable() {
        String javaHome = System.getProperty("java.home");
        Path executable = Path.of(javaHome, "bin", isWindows() ? "java.exe" : "java");
        return executable.toString();
    }

    private static boolean isWindows() {
        return System.getProperty("os.name", "").toLowerCase(java.util.Locale.ROOT).contains("win");
    }

    /**
     * Writes a {@code java @argfile} so the (possibly very long) test classpath never has to be
     * passed as a single command-line argument, matching how the packaged shaft-mcp jar is actually
     * launched by {@code scripts/mcp/install_shaft_mcp.py}. Classpath entries use forward slashes:
     * {@code @argfile} tokens are quote-parsed, and unescaped backslashes before other characters are
     * significant, so a literal Windows path with backslashes must not be embedded as-is.
     */
    private static Path writeLaunchArgsFile() throws IOException {
        String classpath = System.getProperty("java.class.path").replace('\\', '/');
        String mainClass = ShaftMcpApplication.class.getName();
        String content = "-cp" + System.lineSeparator()
                + "\"" + classpath + "\"" + System.lineSeparator()
                + mainClass + System.lineSeparator();
        Path argsFile = Files.createTempFile("shaft-mcp-stdio-manifest-it-", ".args");
        Files.writeString(argsFile, content, StandardCharsets.UTF_8);
        return argsFile;
    }

    private static String initializeRequest(int requestId) {
        return "{\"jsonrpc\":\"2.0\",\"id\":" + requestId + ",\"method\":\"initialize\","
                + "\"params\":{\"protocolVersion\":\"2024-11-05\",\"capabilities\":{},"
                + "\"clientInfo\":{\"name\":\"shaft-mcp-stdio-manifest-it\",\"version\":\"test\"}}}";
    }

    private static String initializedNotification() {
        return "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}";
    }

    private static String toolsListRequest(int requestId) {
        return "{\"jsonrpc\":\"2.0\",\"id\":" + requestId + ",\"method\":\"tools/list\"}";
    }

    private static void send(OutputStream output, String payload) throws IOException {
        output.write(payload.getBytes(StandardCharsets.UTF_8));
        output.write('\n');
        output.flush();
    }

    private static JsonNode readResponseFor(
            BufferedReader input,
            ExecutorService ioExecutor,
            int requestId,
            Duration timeout,
            Process process,
            CompletableFuture<String> stderr) throws Exception {
        long deadline = System.nanoTime() + timeout.toNanos();
        while (System.nanoTime() < deadline) {
            long remainingNanos = deadline - System.nanoTime();
            CompletableFuture<String> lineFuture = CompletableFuture.supplyAsync(() -> readLine(input), ioExecutor);
            String line;
            try {
                line = lineFuture.get(Math.max(1, TimeUnit.NANOSECONDS.toMillis(remainingNanos)),
                        TimeUnit.MILLISECONDS);
            } catch (java.util.concurrent.TimeoutException exception) {
                break;
            }
            if (line == null) {
                break;
            }
            String trimmed = line.trim();
            if (trimmed.isEmpty()) {
                continue;
            }
            JsonNode message;
            try {
                message = JSON.readTree(trimmed);
            } catch (RuntimeException exception) {
                // Startup logs and banners may be written to stdout before JSON-RPC frames; ignore them.
                continue;
            }
            if (message.has("id") && message.path("id").asInt(-1) == requestId) {
                if (message.has("error")) {
                    fail("SHAFT MCP stdio server returned an error for request " + requestId + ": "
                            + message.path("error"));
                }
                return message.path("result");
            }
        }
        String diagnostics = process.isAlive() ? "process is still alive" : "process exited with code "
                + safeExitCode(process);
        fail("Timed out waiting for SHAFT MCP stdio response to request " + requestId + " (" + diagnostics
                + "). stderr: " + readStderr(stderr));
        throw new IllegalStateException("unreachable");
    }

    private static String readLine(BufferedReader input) {
        try {
            return input.readLine();
        } catch (IOException exception) {
            return null;
        }
    }

    private static int safeExitCode(Process process) {
        try {
            return process.exitValue();
        } catch (IllegalThreadStateException exception) {
            return -1;
        }
    }

    private static String readStderr(CompletableFuture<String> stderr) {
        try {
            return stderr.get(250, TimeUnit.MILLISECONDS);
        } catch (Exception exception) {
            return "";
        }
    }

    private static String readAll(java.io.InputStream stream) {
        try (stream) {
            return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException exception) {
            return "";
        }
    }

    private static Set<String> toolNames(JsonNode result) {
        Set<String> names = new java.util.TreeSet<>();
        for (JsonNode tool : result.path("tools")) {
            String name = tool.path("name").asText();
            if (!name.isBlank()) {
                names.add(name);
            }
        }
        return names;
    }

    private static void writeToolListingEvidence(JsonNode result) {
        try {
            Path evidenceDirectory = Path.of("target", "shaft-mcp-evidence");
            Files.createDirectories(evidenceDirectory);
            Path evidenceFile = evidenceDirectory.resolve("stdio-tools-list.json");
            Files.writeString(evidenceFile, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(result),
                    StandardCharsets.UTF_8);
        } catch (IOException exception) {
            // Evidence capture is best-effort; the assertions above are the source of truth.
        }
    }
}
