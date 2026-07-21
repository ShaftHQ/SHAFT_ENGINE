package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;

import java.util.function.Consumer;

/**
 * Opens a real interactive terminal tab running an agent CLI ({@code claude}/{@code codex}) in its
 * normal PTY-backed interactive mode (issue #3959) -- a standalone, additive entry point alongside
 * the existing Assistant chat, never a replacement of it. Deliberately reuses {@link
 * ShaftTerminalCommands}'s reflection-based Terminal-plugin detection and clipboard-paste-free
 * fallback rather than a second implementation of that discipline (the #3917 spike's load-bearing
 * constraint): the only behavior this adds on top is submitting the typed command immediately,
 * since launching the CLI *is* the point of the action, unlike the pre-type-and-let-the-user-review
 * flow {@code ShaftMcpSetupPanel} uses for setup scripts.
 */
public final class CliTerminalSupport {
    private CliTerminalSupport() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Opens (or reuses) a terminal tab named {@code tabName} rooted at {@code workingDirectory} and
     * immediately runs {@code command} in it. {@code onOutcome} mirrors {@link
     * ShaftTerminalCommands#openWithPreparedCommand(Project, String, String, String, Consumer)}:
     * invoked at most once, on the EDT (its real caller is a {@code javax.swing.Timer} listener --
     * see {@code ShaftMcpSetupPanel#copyCommandIntoTerminal} for why that needs no extra {@code
     * invokeLater}), once the async submit has genuinely finished. The boolean return value only
     * means "opening was scheduled" -- false means the Terminal plugin is missing/disabled/drifted
     * (or {@code project}/{@code command} was invalid), and the caller should fall back to telling
     * the user to run {@code command} themselves.
     */
    public static boolean openInteractiveCliTerminal(Project project, String workingDirectory, String tabName,
                                                       String command, Consumer<Boolean> onOutcome) {
        return ShaftTerminalCommands.openWithPreparedCommand(
                project, workingDirectory, tabName, submitCommand(command), onOutcome);
    }

    /**
     * Package-private for {@code CliTerminalSupportTest}: appends the carriage return that submits
     * the typed command to the shell, the one difference from {@code ShaftTerminalCommands}'s
     * default pre-type-without-Enter behavior.
     */
    static String submitCommand(String command) {
        return command + "\r";
    }

    /**
     * Whether {@code executable} is present on {@code PATH}, so callers outside this package (the
     * CLI terminal actions) can point the user at the install command instead of opening a terminal
     * tab that just prints "command not found". Public facade over the package-private {@link
     * CliExecutableDetector} -- kept the same minimal-exposure shape {@link ShaftToolWindowPanel}
     * already uses for {@code com.shaft.intellij.actions}.
     */
    public static boolean isExecutableOnPath(String executable) {
        return CliExecutableDetector.isOnPath(executable);
    }
}
