package com.shaft.commandline.mcp;

/**
 * Extracts the JSON payload from a Server-Sent Events response body. shaft-mcp returns
 * {@code tools/list} and {@code tools/call} results over HTTP as a single SSE {@code message} event:
 *
 * <pre>
 * id:&lt;session-id&gt;
 * event:message
 * data:{"jsonrpc":"2.0",...}
 * </pre>
 *
 * Per the SSE spec, multiple {@code data:} lines within one event are joined with newlines.
 */
public final class SseParser {

    private SseParser() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Concatenates the {@code data:} lines of an SSE body into their raw payload.
     *
     * @param body the raw SSE response body
     * @return the concatenated {@code data} payload (typically a JSON string)
     */
    public static String extractData(String body) {
        StringBuilder data = new StringBuilder();
        for (String line : body.split("\n", -1)) {
            String trimmed = line.endsWith("\r") ? line.substring(0, line.length() - 1) : line;
            if (trimmed.startsWith("data:")) {
                String value = trimmed.substring("data:".length());
                if (value.startsWith(" ")) {
                    value = value.substring(1);
                }
                if (data.length() > 0) {
                    data.append('\n');
                }
                data.append(value);
            }
        }
        return data.toString();
    }
}
