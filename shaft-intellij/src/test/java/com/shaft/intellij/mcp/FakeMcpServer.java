package com.shaft.intellij.mcp;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

final class FakeMcpServer {
    private static final Pattern CONTENT_ID = Pattern.compile("\"id\"\\s*:\\s*(\\d+)");
    private static final Pattern METHOD = Pattern.compile("\"method\"\\s*:\\s*\"([^\"]+)\"");
    private static final Pattern TOOL_NAME = Pattern.compile("\"name\"\\s*:\\s*\"([^\"]+)\"");

    private FakeMcpServer() {
    }

    public static void main(String[] args) throws Exception {
        String mode = args.length == 0 ? "hang" : args[0];
        switch (mode) {
            case "stderrAndExit" -> runWithStderrFailure();
            case "stdoutNoiseThenToolsList" -> runStdoutNoiseThenToolsListServer();
            case "toolsList" -> runToolsListServer();
            case "toolResultArray" -> runToolCallResultServer("[\"first\",{\"name\":\"second\"}]");
            case "toolResultString" -> runToolCallResultServer("\"plain text result\"");
            case "echoTool" -> runEchoToolServer();
            case "silentToolCalls" -> runSilentToolCallServer();
            case "hang" -> Thread.sleep(Long.MAX_VALUE);
            default -> Thread.sleep(Long.MAX_VALUE);
        }
    }

    private static void runWithStderrFailure() {
        System.err.println("fake MCP failed during startup");
        System.exit(8);
    }

    private static void runToolsListServer() throws Exception {
        runServer("{\"tools\":[{\"name\":\"fake_tool\"}]}");
    }

    private static void runStdoutNoiseThenToolsListServer() throws Exception {
        System.out.println("2026-06-29 INFO fake MCP startup log");
        System.out.flush();
        runToolsListServer();
    }

    private static void runToolCallResultServer(String resultJson) throws Exception {
        runServer(resultJson);
    }

    /**
     * Responds to every tools/call with the requested tool name, so tests can prove concurrent
     * callers each receive the response matching their own request id.
     */
    private static void runEchoToolServer() throws Exception {
        BufferedReader input = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
        OutputStream output = System.out;
        while (true) {
            String message = input.readLine();
            if (message == null) {
                return;
            }
            int requestId = requestId(message);
            switch (requestMethod(message)) {
                case "initialize" -> writeMessage(output, requestId,
                        "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"protocolVersion\":\"2024-11-05\"}}");
                case "tools/call" -> writeMessage(output, requestId,
                        "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"echoedTool\":\""
                                + toolName(message) + "\"}}");
                default -> {
                }
            }
        }
    }

    /**
     * Answers the initialize handshake but never responds to tools/call, so tests can cancel an
     * in-flight request and prove the server process stays alive.
     */
    private static void runSilentToolCallServer() throws Exception {
        BufferedReader input = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
        OutputStream output = System.out;
        while (true) {
            String message = input.readLine();
            if (message == null) {
                return;
            }
            if ("initialize".equals(requestMethod(message))) {
                writeMessage(output, requestId(message),
                        "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"protocolVersion\":\"2024-11-05\"}}");
            }
        }
    }

    private static void runServer(String toolCallResultJson) throws Exception {
        BufferedReader input = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
        OutputStream output = System.out;
        while (true) {
            String message = input.readLine();
            if (message == null) {
                return;
            }
            int requestId = requestId(message);
            switch (requestMethod(message)) {
                case "initialize" -> writeMessage(output, requestId,
                        "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"protocolVersion\":\"2024-11-05\"}}");
                case "tools/list" -> writeMessage(output, requestId,
                        "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"tools\":[{\"name\":\"fake_tool\"}]}}");
                case "tools/call" -> writeMessage(output, requestId,
                        "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":" + toolCallResultJson + "}");
                default -> {
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

    private static String toolName(String message) {
        Matcher matcher = TOOL_NAME.matcher(message);
        return matcher.find() ? matcher.group(1) : "unknown";
    }
}
