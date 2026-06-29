package com.shaft.intellij.mcp;

import java.util.ArrayList;
import java.util.List;

/**
 * Small command-line parser for user-configured MCP stdio commands.
 */
public final class ShaftCommandLine {
    private ShaftCommandLine() {
        throw new IllegalStateException("Utility class");
    }

    public static List<String> parse(String commandLine) {
        List<String> tokens = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean quoted = false;
        char quote = 0;
        for (int index = 0; index < commandLine.length(); index++) {
            char c = commandLine.charAt(index);
            if (c == '\\' && index + 1 < commandLine.length() && quoted && commandLine.charAt(index + 1) == quote) {
                current.append(commandLine.charAt(index + 1));
                index++;
            } else if (quoted && c == quote) {
                quoted = false;
            } else if (!quoted && (c == '"' || c == '\'')) {
                quoted = true;
                quote = c;
            } else if (!quoted && Character.isWhitespace(c)) {
                add(tokens, current);
            } else {
                current.append(c);
            }
        }
        add(tokens, current);
        return tokens;
    }

    private static void add(List<String> tokens, StringBuilder current) {
        if (current.length() > 0) {
            tokens.add(current.toString());
            current.setLength(0);
        }
    }
}
