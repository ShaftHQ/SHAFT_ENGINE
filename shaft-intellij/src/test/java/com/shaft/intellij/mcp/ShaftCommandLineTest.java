package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins the tokenization of user-configured MCP stdio command lines ({@link ShaftCommandLine#parse}).
 * This is a first-run-critical path (the command a user types in Settings launches the MCP server),
 * and it had no unit coverage before. The Windows-path cases below guard the {@code
 * CommandLineToArgvW} backslash rule so a doubled trailing separator no longer swallows the rest of
 * the command line.
 */
class ShaftCommandLineTest {

    @Test
    void splitsPlainCommandOnWhitespaceAndCollapsesRuns() {
        assertEquals(List.of("java", "-jar", "app.jar"), ShaftCommandLine.parse("java -jar app.jar"));
        assertEquals(List.of("a", "b"), ShaftCommandLine.parse("a   b"));
        assertEquals(List.of("a", "b"), ShaftCommandLine.parse("  a\tb  "));
    }

    @Test
    void keepsUnquotedWindowsBackslashPathsLiteral() {
        assertEquals(
                List.of("java", "@C:\\Users\\Mohab\\.shaft\\shaft-mcp.args"),
                ShaftCommandLine.parse("java @C:\\Users\\Mohab\\.shaft\\shaft-mcp.args"));
        // Backslashes that are not before a double quote are always literal, never an escape.
        assertEquals(List.of("a\\\\b"), ShaftCommandLine.parse("a\\\\b"));
    }

    @Test
    void quotedPathWithSpacesBecomesASingleToken() {
        assertEquals(
                List.of("C:\\Program Files\\Java\\jdk-21\\bin\\java.exe", "-jar", "app.jar"),
                ShaftCommandLine.parse("\"C:\\Program Files\\Java\\jdk-21\\bin\\java.exe\" -jar app.jar"));
    }

    @Test
    void doubledTrailingBackslashInQuotedPathTokenizesInsteadOfSwallowingTheLine() {
        // The documented Windows form for a trailing separator inside quotes doubles the backslash.
        // Before the CommandLineToArgvW fix this kept the quote open and swallowed "-jar app.jar".
        assertEquals(
                List.of("C:\\tools\\shaft\\", "-jar", "app.jar"),
                ShaftCommandLine.parse("\"C:\\tools\\shaft\\\\\" -jar app.jar"));
    }

    @Test
    void escapedDoubleQuotesInsideDoubleQuotesAreLiteral() {
        assertEquals(List.of("say \"hi\""), ShaftCommandLine.parse("\"say \\\"hi\\\"\""));
    }

    @Test
    void singleQuotesAreLiteralWithNoEscapes() {
        // POSIX-style single quotes: backslashes stay literal until the closing quote.
        assertEquals(List.of("C:\\tools\\it"), ShaftCommandLine.parse("'C:\\tools\\it'"));
        assertEquals(List.of("a b c"), ShaftCommandLine.parse("'a b c'"));
    }

    @Test
    void preservesWhitespaceInsideQuotesAndDropsEmptyArguments() {
        assertEquals(List.of("-Dname=a b"), ShaftCommandLine.parse("\"-Dname=a b\""));
        assertTrue(ShaftCommandLine.parse("").isEmpty());
        assertTrue(ShaftCommandLine.parse("   ").isEmpty());
        assertEquals(List.of("   "), ShaftCommandLine.parse("\"   \""));
    }
}
