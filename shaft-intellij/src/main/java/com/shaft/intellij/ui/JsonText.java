package com.shaft.intellij.ui;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParser;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Small JSON text helpers for the SHAFT tool window.
 */
final class JsonText {
    private static final Gson PRETTY = new GsonBuilder().setPrettyPrinting().create();

    private JsonText() {
        throw new IllegalStateException("Utility class");
    }

    static String prettyOrOriginal(String text) {
        if (text == null || text.isBlank()) {
            return "";
        }
        try {
            return PRETTY.toJson(JsonParser.parseString(text));
        } catch (RuntimeException exception) {
            return text;
        }
    }

    static String validateObject(String text) {
        try {
            JsonParser.parseString(text == null || text.isBlank() ? "{}" : text).getAsJsonObject();
            return "";
        } catch (RuntimeException exception) {
            return exception.getMessage();
        }
    }

    /**
     * Computes the line and column of the first JSON syntax error.
     *
     * @param text the JSON text to validate
     * @return a JsonErrorLocation with line/column info, or null if valid
     */
    @Nullable
    static JsonErrorLocation findErrorLocation(@NotNull String text) {
        if (text.isBlank()) {
            return null;
        }
        try {
            JsonParser.parseString(text).getAsJsonObject();
            return null;
        } catch (RuntimeException exception) {
            return parseLocationFromException(text, exception);
        }
    }

    @Nullable
    private static JsonErrorLocation parseLocationFromException(String text, RuntimeException exception) {
        String message = exception.getMessage();
        if (message == null || message.isBlank()) {
            return new JsonErrorLocation(1, 0);
        }
        int[] location = extractLineColumn(message);
        if (location != null) {
            return new JsonErrorLocation(location[0], location[1]);
        }
        int offset = findOffsetFromMessage(text, message);
        return computeLineColumn(text, offset);
    }

    @Nullable
    private static int[] extractLineColumn(String message) {
        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile("line (\\d+) column (\\d+)");
        java.util.regex.Matcher matcher = pattern.matcher(message);
        if (matcher.find()) {
            return new int[]{Integer.parseInt(matcher.group(1)), Integer.parseInt(matcher.group(2))};
        }
        return null;
    }

    private static int findOffsetFromMessage(String text, String message) {
        String context = extractContext(message);
        if (context == null || context.isEmpty()) {
            return 0;
        }
        int idx = text.indexOf(context);
        return idx >= 0 ? idx : 0;
    }

    @Nullable
    private static String extractContext(String message) {
        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile("Unexpected char '(.)'");
        java.util.regex.Matcher matcher = pattern.matcher(message);
        if (matcher.find()) {
            return matcher.group(1);
        }
        return null;
    }

    private static JsonErrorLocation computeLineColumn(String text, int offset) {
        int line = 1;
        int column = 0;
        for (int i = 0; i < Math.min(offset, text.length()); i++) {
            if (text.charAt(i) == '\n') {
                line++;
                column = 0;
            } else {
                column++;
            }
        }
        return new JsonErrorLocation(line, column);
    }

    /**
     * Location of a JSON syntax error (line and column).
     */
    static final class JsonErrorLocation {
        final int line;
        final int column;

        JsonErrorLocation(int line, int column) {
            this.line = line;
            this.column = column;
        }

        @Override
        public String toString() {
            return String.format("line %d, column %d", line, column);
        }
    }
}
