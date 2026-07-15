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

    /**
     * Splits a user-configured command line into tokens.
     *
     * <p>Backslashes before a double quote follow the Windows {@code CommandLineToArgvW} rule so
     * that native paths tokenize the way a Windows user expects: {@code 2n} backslashes plus a
     * {@code "} emit {@code n} backslashes and the quote toggles quoting; {@code 2n+1} backslashes
     * plus a {@code "} emit {@code n} backslashes and a literal {@code "}. In particular a quoted
     * path written with the documented doubled trailing separator -- {@code "C:\tools\shaft\\"} --
     * tokenizes to {@code C:\tools\shaft\} instead of swallowing the rest of the line. Backslashes
     * not before a double quote are always literal (never an escape), so ordinary paths such as
     * {@code "C:\Program Files\java.exe"} pass through unchanged. Single quotes are literal
     * (POSIX-style): no escapes apply until the closing {@code '}.</p>
     */
    public static List<String> parse(String commandLine) {
        List<String> tokens = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean inSingle = false;
        boolean inDouble = false;
        int index = 0;
        int length = commandLine.length();
        while (index < length) {
            char c = commandLine.charAt(index);
            if (c == '\\' && !inSingle) {
                int backslashes = 0;
                while (index < length && commandLine.charAt(index) == '\\') {
                    backslashes++;
                    index++;
                }
                if (index < length && commandLine.charAt(index) == '"') {
                    current.append("\\".repeat(backslashes / 2));
                    if ((backslashes & 1) == 1) {
                        current.append('"'); // odd count: the quote is escaped (literal)
                        index++;
                    }
                    // even count: leave the '"' for the next iteration to toggle quoting
                } else {
                    current.append("\\".repeat(backslashes)); // literal backslashes
                }
            } else if (inSingle) {
                if (c == '\'') {
                    inSingle = false;
                } else {
                    current.append(c);
                }
                index++;
            } else if (inDouble) {
                if (c == '"') {
                    inDouble = false;
                } else {
                    current.append(c);
                }
                index++;
            } else if (c == '"') {
                inDouble = true;
                index++;
            } else if (c == '\'') {
                inSingle = true;
                index++;
            } else if (Character.isWhitespace(c)) {
                add(tokens, current);
                index++;
            } else {
                current.append(c);
                index++;
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
