package com.shaft.intellij.ui.firstrun;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SetupFailureReportTest {
    @BeforeEach
    void resetSession() {
        SetupFailureReport.clearSession();
    }

    @Test
    void redactDropsSecretsAndPathsButKeepsTheDiagnosticSentence() {
        String raw = """
                Plugin 9.8.7 on IU-243.21565.193 Linux. Step install.
                IllegalStateException at com.shaft.intellij.ui.firstrun.InstallStep.check
                The check needs attention.
                home=/home/mohab/secret project=/work/demo/shaft
                token ghp_abcDEF1234567890 and sk-live-secret and Bearer ya29.token
                mail ada@example.com
                """;
        String redacted = SetupFailureReport.redact(raw, "/home/mohab", "/work/demo/shaft");

        assertTrue(redacted.contains("9.8.7"), redacted);
        assertTrue(redacted.contains("IU-243.21565.193"), redacted);
        assertTrue(redacted.contains("Linux"), redacted);
        assertTrue(redacted.contains("install"), redacted);
        assertTrue(redacted.contains("IllegalStateException"), redacted);
        assertTrue(redacted.contains("com.shaft.intellij.ui.firstrun.InstallStep.check"), redacted);
        assertTrue(redacted.contains("The check needs attention."), redacted);
        assertFalse(redacted.contains("ghp_"));
        assertFalse(redacted.contains("sk-live"));
        assertFalse(redacted.contains("ya29.token"));
        assertFalse(redacted.contains("ada@example.com"));
        assertFalse(redacted.contains("/home/mohab"));
        assertFalse(redacted.contains("/work/demo/shaft"));
    }

    @Test
    void fingerprintIsStableForTheSameFailure() {
        IllegalStateException error = new IllegalStateException("boom");
        error.setStackTrace(new StackTraceElement[]{
                new StackTraceElement("com.shaft.intellij.ui.firstrun.InstallStep", "check", "InstallStep.java", 40),
                new StackTraceElement("java.lang.Thread", "run", "Thread.java", 1)
        });
        String first = SetupFailureReport.fingerprint(error, "install");
        String second = SetupFailureReport.fingerprint(error, "install");
        assertEquals(first, second);
        assertTrue(first.contains("IllegalStateException"));
        assertTrue(first.contains("com.shaft.intellij.ui.firstrun.InstallStep.check"));
        assertTrue(first.contains("install"));
    }

    @Test
    void ghCreateArgsAreAFixedRepoArgumentList() {
        List<String> args = SetupFailureReport.ghCreateArgs("other/repo", "Title", "Body");
        assertEquals(List.of("gh", "issue", "create", "--repo", "ShaftHQ/SHAFT_ENGINE"), args.subList(0, 5));
        assertTrue(args.contains("Title"));
        assertTrue(args.contains("Body"));
        assertFalse(String.join(" ", args).contains("sh"));
        assertFalse(args.contains("-c"));
        String url = SetupFailureReport.browserUrl("Title", "Body line");
        assertTrue(url.startsWith("https://github.com/ShaftHQ/SHAFT_ENGINE/issues/new?"));
        assertTrue(url.contains("title="));
        assertTrue(url.contains("body="));
    }

    @Test
    void optInFalseDoesNotStartAProcessOrOpenABrowser() {
        AtomicInteger runs = new AtomicInteger();
        AtomicInteger opens = new AtomicInteger();
        SetupFailureReport.file(false, "fp", "t", "b", () -> true,
                command -> {
                    runs.incrementAndGet();
                    return "";
                },
                url -> opens.incrementAndGet());
        assertEquals(0, runs.get());
        assertEquals(0, opens.get());
    }

    @Test
    void authenticatedGhCreatesOnceWhenTheFingerprintIsNew() {
        List<List<String>> commands = new ArrayList<>();
        SetupFailureReport.ProcessRunner runner = command -> {
            commands.add(command);
            if (command.contains("list")) {
                return "[]";
            }
            return "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/1";
        };
        SetupFailureReport.file(true, "fp-1", "Title", "Body", () -> true, runner, url -> {
            throw new AssertionError("browser");
        });
        SetupFailureReport.file(true, "fp-1", "Title", "Body", () -> true, runner, url -> {
            throw new AssertionError("browser");
        });
        assertEquals(2, commands.size());
        assertEquals("list", commands.get(0).get(2));
        assertEquals(List.of("gh", "issue", "create", "--repo", "ShaftHQ/SHAFT_ENGINE"), commands.get(1).subList(0, 5));
    }

    @Test
    void unauthenticatedReportOpensTheBrowserOnce() {
        List<String> urls = new ArrayList<>();
        SetupFailureReport.file(true, "fp-browser", "Title", "Body", () -> false, command -> {
            throw new AssertionError(command.toString());
        }, urls::add);
        SetupFailureReport.file(true, "fp-browser", "Title", "Body", () -> false, command -> {
            throw new AssertionError(command.toString());
        }, urls::add);
        assertEquals(1, urls.size());
        assertTrue(urls.get(0).startsWith("https://github.com/ShaftHQ/SHAFT_ENGINE/issues/new?"));
    }
}
