package com.shaft.intellij;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression guard for issue #3622: every {@code CompletableFuture.supplyAsync}/{@code runAsync}
 * call under {@code shaft-intellij/src/main} must pass an explicit executor. An argless call
 * silently defaults to {@code ForkJoinPool.commonPool()} — a JVM-wide pool shared with the IDE
 * itself and every other plugin — instead of the bounded {@code ShaftPluginExecutor} this
 * plugin's MCP/local-agent blocking work must run on.
 */
class NoUnboundedCommonPoolUsageTest {
    private static final Pattern CALL_START =
            Pattern.compile("CompletableFuture\\.(supplyAsync|runAsync)\\(");

    @Test
    void everySupplyAsyncAndRunAsyncCallPassesAnExplicitExecutor() throws IOException {
        List<String> violations = new ArrayList<>();
        try (var paths = Files.walk(Path.of("src/main"))) {
            for (Path path : paths.filter(p -> p.toString().endsWith(".java")).toList()) {
                String source = Files.readString(path);
                Matcher matcher = CALL_START.matcher(source);
                while (matcher.find()) {
                    String arguments = callArguments(source, matcher.end());
                    if (countTopLevelArguments(arguments) < 2) {
                        violations.add(path + " (offset " + matcher.start() + "): " + matcher.group() + "...");
                    }
                }
            }
        }
        assertTrue(violations.isEmpty(),
                "issue #3622: found supplyAsync/runAsync call(s) with no explicit executor "
                        + "(defaults to ForkJoinPool.commonPool()): " + violations);
    }

    /**
     * Returns the substring strictly between the call's opening paren (just consumed by the
     * matcher, so {@code afterOpenParen} points right after it) and its balanced closing paren.
     */
    private static String callArguments(String source, int afterOpenParen) {
        int depth = 1;
        int index = afterOpenParen;
        while (depth > 0) {
            char c = source.charAt(index);
            switch (c) {
                case '(', '{', '[' -> depth++;
                case ')', '}', ']' -> depth--;
                default -> { }
            }
            index++;
        }
        return source.substring(afterOpenParen, index - 1);
    }

    /**
     * Counts top-level (i.e. not nested inside further parens/braces/brackets — including a
     * lambda's own body) comma-separated arguments in a call's argument text.
     */
    private static int countTopLevelArguments(String arguments) {
        if (arguments.isBlank()) {
            return 0;
        }
        int depth = 0;
        int commas = 0;
        for (char c : arguments.toCharArray()) {
            switch (c) {
                case '(', '{', '[' -> depth++;
                case ')', '}', ']' -> depth--;
                case ',' -> {
                    if (depth == 0) {
                        commas++;
                    }
                }
                default -> { }
            }
        }
        return commas + 1;
    }
}
