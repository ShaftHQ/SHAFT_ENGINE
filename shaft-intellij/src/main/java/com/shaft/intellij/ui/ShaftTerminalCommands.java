package com.shaft.intellij.ui;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;

import java.lang.reflect.Method;
import java.util.function.Consumer;

/**
 * Opens an IntelliJ terminal tab and pre-types a command into its shell so the user only has to
 * press Enter (issue #3426 A3). The Terminal plugin is an optional dependency, so everything here
 * is reflective and best-effort: when the plugin is missing or its API has drifted, callers fall
 * back to the clipboard copy they already made.
 */
final class ShaftTerminalCommands {
    private static final String MANAGER_CLASS = "org.jetbrains.plugins.terminal.TerminalToolWindowManager";
    /** Delay before typing: a freshly created shell needs a beat before it accepts input cleanly. */
    private static final int SHELL_READY_DELAY_MILLIS = 900;
    /** Retry cadence and cap when the TTY connector is not ready yet at the first attempt. */
    private static final int TYPE_RETRY_DELAY_MILLIS = 500;
    private static final int MAX_TYPE_ATTEMPTS = 10;

    private ShaftTerminalCommands() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Opens a new terminal tab named {@code tabName} in the project root and types {@code command}
     * into it without executing it. Returns {@code true} when the terminal tab was opened and the
     * pre-type was scheduled; {@code false} means the caller should tell the user to paste the
     * already-copied command manually. This overload never learns whether the async pre-type
     * actually succeeded; prefer the {@link Consumer} overload when the caller can react to that.
     */
    static boolean openWithPreparedCommand(Project project, String workingDirectory, String tabName, String command) {
        return openWithPreparedCommand(project, workingDirectory, tabName, command, typed -> { });
    }

    /**
     * Same as {@link #openWithPreparedCommand(Project, String, String, String)}, but {@code
     * onOutcome} is invoked exactly once, on the EDT (its real caller is a {@code javax.swing.Timer}
     * listener, which the Swing contract guarantees runs on the EDT), once the async pre-type has
     * genuinely finished: {@code true} only on a real {@code TtyConnector.write()} success, {@code
     * false} when retries were exhausted or the write threw (issue #3551 — the boolean return here
     * only means "typing was scheduled", not "the command was typed").
     */
    static boolean openWithPreparedCommand(Project project, String workingDirectory, String tabName, String command,
                                            Consumer<Boolean> onOutcome) {
        if (project == null || command == null || command.isBlank()
                || ApplicationManager.getApplication() == null) {
            return false;
        }
        try {
            Class<?> managerClass = Class.forName(MANAGER_CLASS);
            Object manager = managerClass.getMethod("getInstance", Project.class).invoke(null, project);
            Object widget = createShellWidget(managerClass, manager, workingDirectory, tabName);
            if (widget == null) {
                return false;
            }
            scheduleCommandTyping(widget, command, onOutcome);
            return true;
        } catch (Throwable terminalUnavailable) {
            // Terminal plugin disabled/missing, or its API drifted: the clipboard fallback covers it.
            return false;
        }
    }

    private static Object createShellWidget(
            Class<?> managerClass, Object manager, String workingDirectory, String tabName) throws Exception {
        // ShellTerminalWidget first: it exposes getTtyConnector() directly and survives on every
        // platform this plugin targets. The newer createShellWidget API is the fallback shape.
        try {
            Method createLocalShellWidget = managerClass.getMethod(
                    "createLocalShellWidget", String.class, String.class);
            return createLocalShellWidget.invoke(manager, workingDirectory, tabName);
        } catch (NoSuchMethodException newerPlatform) {
            Method createShellWidget = managerClass.getMethod(
                    "createShellWidget", String.class, String.class, boolean.class, boolean.class);
            return createShellWidget.invoke(manager, workingDirectory, tabName, true, false);
        }
    }

    /**
     * Types the command into the widget's TTY once the shell has had a moment to start. Uses the
     * plain jediterm {@code TtyConnector.write(String)} (types WITHOUT pressing Enter — running a
     * just-downloaded script stays the user's explicit decision). A slow shell start no longer
     * silently loses the command: while the TTY connector is missing, typing retries on a short
     * cadence before giving up to the clipboard fallback. {@code onOutcome} fires exactly once, on
     * genuine success or genuine failure (retry-exhaustion or a thrown {@code write()}) — never on
     * a still-retrying attempt (issue #3551).
     */
    private static void scheduleCommandTyping(Object widget, String command, Consumer<Boolean> onOutcome) {
        java.util.concurrent.atomic.AtomicInteger attempts = new java.util.concurrent.atomic.AtomicInteger();
        javax.swing.Timer typeSoon = new javax.swing.Timer(TYPE_RETRY_DELAY_MILLIS, null);
        typeSoon.setInitialDelay(SHELL_READY_DELAY_MILLIS);
        typeSoon.addActionListener(event -> {
            boolean typed = false;
            boolean writeThrew = false;
            try {
                Object connector = ttyConnector(widget);
                if (connector != null) {
                    connector.getClass().getMethod("write", String.class).invoke(connector, command);
                    typed = true;
                }
            } catch (Throwable failedToWrite) {
                // The write genuinely failed; report failure below instead of claiming success -
                // the command is still safely on the clipboard as a fallback.
                writeThrew = true;
            }
            boolean retriesExhausted = attempts.incrementAndGet() >= MAX_TYPE_ATTEMPTS;
            if (typed || writeThrew || retriesExhausted) {
                typeSoon.stop();
                onOutcome.accept(typed);
            }
        });
        typeSoon.setRepeats(true);
        typeSoon.start();
    }

    private static Object ttyConnector(Object widget) {
        try {
            Object connector = widget.getClass().getMethod("getTtyConnector").invoke(widget);
            if (connector != null) {
                return connector;
            }
        } catch (Throwable tryAccessor) {
            // Fall through to the newer TerminalWidget accessor shape.
        }
        try {
            Object accessor = widget.getClass().getMethod("getTtyConnectorAccessor").invoke(widget);
            return accessor == null ? null : accessor.getClass().getMethod("getTtyConnector").invoke(accessor);
        } catch (Throwable unavailable) {
            return null;
        }
    }
}
