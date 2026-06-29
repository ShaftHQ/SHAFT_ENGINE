package com.shaft.intellij.mcp;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

final class FakeMcpServer {
    private static final Pattern CONTENT_ID = Pattern.compile("\"id\"\\s*:\\s*(\\d+)");
    private static final Pattern METHOD = Pattern.compile("\"method\"\\s*:\\s*\"([^\"]+)\"");

    private FakeMcpServer() {
    }

    public static void main(String[] args) throws Exception {
        String mode = args.length == 0 ? "hang" : args[0];
        switch (mode) {
            case "stderrAndExit" -> runWithStderrFailure();
            case "toolsList" -> runToolsListServer();
            case "toolResultArray" -> runToolCallResultServer("[\"first\",{\"name\":\"second\"}]");
            case "toolResultString" -> runToolCallResultServer("\"plain text result\"");
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

    private static void runToolCallResultServer(String resultJson) throws Exception {
        runServer(resultJson);
    }

    private static void runServer(String toolCallResultJson) throws Exception {
        InputStream input = System.in;
        OutputStream output = System.out;
        while (true) {
            String message = readMessage(input);
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
        output.write(("Content-Length: " + body.length + "\r\n\r\n").getBytes(StandardCharsets.US_ASCII));
        output.write(body);
        output.flush();
    }

    private static String readMessage(InputStream input) throws IOException {
        int contentLength = readContentLength(input);
        if (contentLength < 0) {
            return null;
        }
        byte[] body = input.readNBytes(contentLength);
        if (body.length != contentLength) {
            return null;
        }
        return new String(body, StandardCharsets.UTF_8);
    }

    private static int readContentLength(InputStream input) throws IOException {
        ByteArrayOutputStream header = new ByteArrayOutputStream();
        int previous = -1;
        while (true) {
            int current = input.read();
            if (current == -1) {
                return -1;
            }
            header.write(current);
            String value = header.toString(StandardCharsets.US_ASCII);
            if ((previous == '\n' && current == '\n') || value.endsWith("\r\n\r\n")) {
                break;
            }
            previous = current;
        }
        String[] lines = header.toString(StandardCharsets.US_ASCII).split("\\r?\\n");
        for (String line : lines) {
            int separator = line.indexOf(':');
            if (separator > 0 && "content-length".equalsIgnoreCase(line.substring(0, separator).trim())) {
                return Integer.parseInt(line.substring(separator + 1).trim());
            }
        }
        return -1;
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
