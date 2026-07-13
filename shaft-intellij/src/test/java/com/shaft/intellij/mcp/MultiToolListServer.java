package com.shaft.intellij.mcp;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Fake MCP server, spawned as a real subprocess, dedicated to exercising
 * {@link ShaftMcpInvocationService}'s unknown-tool-name suggestion logic
 * ({@code suggestSimilar}/{@code isRelated}/{@code levenshtein}) with more than one known tool name
 * (a single-tool catalog, as used by {@link CountingToolsListServer}, never makes the suggestion
 * ranking actually compare candidates). Kept separate from {@link FakeMcpServer} and
 * {@link CountingToolsListServer} so this suite owns its own fixture instead of growing shared test
 * infrastructure other suites depend on.
 *
 * <p>The {@code tools/list} response also embeds one non-object entry ahead of the real tools so
 * tests can pin that {@code parseToolNames} skips anything that is not a JSON object instead of
 * throwing on it.</p>
 */
final class MultiToolListServer {
    static final String[] TOOL_NAMES = {
            "capture_start", "capture_stop", "capture_status", "driver_initialize",
    };

    private static final Pattern CONTENT_ID = Pattern.compile("\"id\"\\s*:\\s*(\\d+)");
    private static final Pattern METHOD = Pattern.compile("\"method\"\\s*:\\s*\"([^\"]+)\"");

    private MultiToolListServer() {
    }

    public static void main(String[] args) throws IOException {
        BufferedReader input = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
        OutputStream output = System.out;
        String message;
        while ((message = input.readLine()) != null) {
            int requestId = requestId(message);
            switch (requestMethod(message)) {
                case "initialize" -> writeMessage(output, requestId,
                        "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"protocolVersion\":\"2024-11-05\"}}");
                case "tools/list" -> writeMessage(output, requestId,
                        "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"tools\":["
                                + "42,"
                                + "{\"name\":\"" + TOOL_NAMES[0] + "\"},"
                                + "{\"name\":\"" + TOOL_NAMES[1] + "\"},"
                                + "{\"name\":\"" + TOOL_NAMES[2] + "\"},"
                                + "{\"name\":\"" + TOOL_NAMES[3] + "\"}"
                                + "]}}");
                case "tools/call" -> writeMessage(output, requestId,
                        "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"content\":[],\"isError\":false}}");
                default -> {
                    // Ignore notifications (e.g. notifications/initialized) and unknown methods.
                }
            }
        }
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
