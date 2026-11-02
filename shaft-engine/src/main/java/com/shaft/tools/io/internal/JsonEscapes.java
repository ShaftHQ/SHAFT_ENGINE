package com.shaft.tools.io.internal;

/**
 * RFC 8259 string escapes for SHAFT-owned JSON serializers.
 */
final class JsonEscapes {
    private JsonEscapes() {
        throw new IllegalStateException("Utility class");
    }

    static String escape(String value) {
        String source = value == null ? "" : value;
        StringBuilder escaped = new StringBuilder(source.length());
        for (int i = 0; i < source.length(); i++) {
            char c = source.charAt(i);
            switch (c) {
                case '\\' -> escaped.append("\\\\");
                case '"' -> escaped.append("\\\"");
                case '\n' -> escaped.append("\\n");
                case '\r' -> escaped.append("\\r");
                case '\t' -> escaped.append("\\t");
                default -> {
                    if (c < 0x20) {
                        escaped.append(String.format("\\u%04x", (int) c));
                    } else {
                        escaped.append(c);
                    }
                }
            }
        }
        return escaped.toString();
    }
}
