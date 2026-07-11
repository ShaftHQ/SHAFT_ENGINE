package com.shaft.intellij.ui;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;

import java.lang.reflect.Method;

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

    private ShaftTerminalCommands() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Opens a new terminal tab named {@code tabName} in the project root and types {@code command}
     * into it without executing it. Returns {@code true} when the terminal tab was opened and the
     * pre-type was scheduled; {@code false} means the caller should tell the user to paste the
     * already-copied command manually.
     */
    static boolean openWithPreparedCommand(Project project, String workingDirectory, String tabName, String command) {
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
            scheduleCommandTyping(widget, command);
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
     * just-downloaded script stays the user's explicit decision).
     */
    private static void scheduleCommandTyping(Object widget, String command) {
        javax.swing.Timer typeSoon = new javax.swing.Timer(SHELL_READY_DELAY_MILLIS, event -> {
            try {
                Object connector = ttyConnector(widget);
                if (connector != null) {
                    connector.getClass().getMethod("write", String.class).invoke(connector, command);
                }
            } catch (Throwable ignored) {
                // Best effort: the command is already on the clipboard.
            }
        });
        typeSoon.setRepeats(false);
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
