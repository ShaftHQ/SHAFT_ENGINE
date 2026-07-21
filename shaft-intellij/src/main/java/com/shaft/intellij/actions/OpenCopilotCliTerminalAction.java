package com.shaft.intellij.actions;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.notifications.ShaftNotifier;
import com.shaft.intellij.ui.CliTerminalSupport;
import org.jetbrains.annotations.NotNull;

/**
 * Opens a real interactive terminal tab running the GitHub Copilot CLI ({@code copilot}, its
 * normal interactive TUI, entered with no arguments) so power users can opt into the raw CLI
 * experience alongside the guided Assistant chat, not instead of it (issue #3963, follow-up to
 * #3959/#3960's Claude Code and Codex terminal actions). Targets the standalone {@code copilot}
 * binary from the {@code @github/copilot} npm package (GitHub Copilot CLI, generally available),
 * not the {@code gh copilot} `gh` subcommand -- {@code CliExecutableDetector}/{@code
 * CliTerminalSupport} only know how to detect and launch a single bare executable on {@code PATH},
 * with no concept of "a subcommand of another CLI", matching the same single-binary pattern already
 * used for {@code claude} and {@code codex}. Only ever invokes an already-installed {@code copilot}
 * binary found on {@code PATH} -- nothing is bundled or redistributed.
 */
public final class OpenCopilotCliTerminalAction extends AnAction implements DumbAware {
    private static final String EXECUTABLE = "copilot";
    private static final String TAB_NAME = "Copilot CLI";
    private static final String NOTIFICATION_TITLE = "Copilot CLI terminal";
    private static final String INSTALL_HINT = "GitHub Copilot CLI (`copilot`) isn't on PATH. Install it first: "
            + "npm install -g @github/copilot";
    private static final String TERMINAL_UNAVAILABLE_HINT = "Couldn't open a terminal automatically -- the "
            + "JetBrains Terminal plugin may be missing or disabled. Open a terminal yourself and run: " + EXECUTABLE;

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        if (project == null) {
            return;
        }
        if (!CliTerminalSupport.isExecutableOnPath(EXECUTABLE)) {
            ShaftNotifier.warn(project, NOTIFICATION_TITLE, INSTALL_HINT);
            return;
        }
        String workingDirectory = project.getBasePath() == null ? "." : project.getBasePath();
        // No invokeLater: onOutcome's real caller is a javax.swing.Timer listener, which the Swing
        // contract already guarantees runs on the EDT (see ShaftMcpSetupPanel#copyCommandIntoTerminal
        // for the same reasoning against the same ShaftTerminalCommands seam).
        CliTerminalSupport.openInteractiveCliTerminal(project, workingDirectory, TAB_NAME, EXECUTABLE, typed -> {
            if (!typed) {
                ShaftNotifier.warn(project, NOTIFICATION_TITLE, TERMINAL_UNAVAILABLE_HINT);
            }
        });
    }
}
