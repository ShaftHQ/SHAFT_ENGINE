package com.shaft.intellij.mcp;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Minimal fake MCP server, spawned as a real subprocess, dedicated to
 * {@link ShaftMcpInvocationServiceCacheTest}. Kept separate from {@link FakeMcpServer} so this
 * cache-focused suite owns its own fixture instead of growing shared test infrastructure other
 * suites (which this task does not own) depend on.
 *
 * <p>Each {@code tools/list} response embeds a call counter as a sibling {@code "calls"} field in
 * the JSON-RPC result. The counter is persisted in the file named by the
 * {@value #COUNTER_FILE_PROPERTY} system property (set by the test on the java command line)
 * rather than kept in memory, so it stays monotonic across a respawn: an in-memory counter would
 * silently reset to 1 in a freshly spawned process and could not distinguish "the cache served a
 * stale pre-respawn payload" from "a genuine fresh round-trip to the new process, which coincidentally
 * looks identical". Because the test JVM and this server run in separate processes,
 * {@link #callCount(String)} recovers the counter purely by reading the returned payload string.</p>
 */
final class CountingToolsListServer {
    /** The one tool this server advertises. */
    static final String TOOL_NAME = "counting_tool";

    /** System property naming the file this server uses to persist its call counter. */
    static final String COUNTER_FILE_PROPERTY = "shaft.test.mcp.counterFile";

    private static final Pattern CONTENT_ID = Pattern.compile("\"id\"\\s*:\\s*(\\d+)");
    private static final Pattern METHOD = Pattern.compile("\"method\"\\s*:\\s*\"([^\"]+)\"");
    private static final Pattern CALL_COUNT = Pattern.compile("\"calls\"\\s*:\\s*(\\d+)");

    private CountingToolsListServer() {
    }

    public static void main(String[] args) throws IOException {
        Path counterFile = Path.of(System.getProperty(COUNTER_FILE_PROPERTY));
        BufferedReader input = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
        OutputStream output = System.out;
        String message;
        while ((message = input.readLine()) != null) {
            int requestId = requestId(message);
            switch (requestMethod(message)) {
                case "initialize" -> writeMessage(output, requestId,
                        "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"protocolVersion\":\"2024-11-05\"}}");
                case "tools/list" -> {
                    int calls = incrementAndGet(counterFile);
                    writeMessage(output, requestId,
                            "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"tools\":[{\"name\":\"" + TOOL_NAME
                                    + "\"}],\"calls\":" + calls + "}}");
                }
                case "tools/call" -> writeMessage(output, requestId,
                        "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"content\":[],\"isError\":false}}");
                default -> {
                    // Ignore notifications (e.g. notifications/initialized) and unknown methods.
                }
            }
        }
    }

    private static int incrementAndGet(Path counterFile) throws IOException {
        int current = Files.exists(counterFile)
                ? Integer.parseInt(Files.readString(counterFile, StandardCharsets.UTF_8).trim())
                : 0;
        int next = current + 1;
        Files.writeString(counterFile, String.valueOf(next), StandardCharsets.UTF_8);
        return next;
    }

    /**
     * Recovers the {@code "calls"} counter embedded in a {@code tools/list} raw payload.
     *
     * @param rawPayload the raw JSON-RPC {@code result} payload (as returned by
     *                    {@code ShaftMcpToolResult#output()})
     * @return the counter value, or -1 if the payload has none (not a {@code tools/list} response)
     */
    static int callCount(String rawPayload) {
        Matcher matcher = CALL_COUNT.matcher(rawPayload == null ? "" : rawPayload);
        return matcher.find() ? Integer.parseInt(matcher.group(1)) : -1;
    }

    private static void writeMessage(OutputStream output, int requestId, String responseTemplate) throws IOException {
        String response = responseTemplate.formatted(requestId);
        byte[] body = response.getBytes(StandardCharsets.UTF_8);
        output.write(body);
        output.write('\n');
        output.flush();
    }

    private static int requestId(String message) {
        Matcher matcher = CONTENT_ID.matcher(message);
        return matcher.find() ? Integer.parseInt(matcher.group(1)) : 1;
    }

    private static String requestMethod(String message) {
        Matcher matcher = METHOD.matcher(message);
        return matcher.find() ? matcher.group(1) : "";
    }
}
