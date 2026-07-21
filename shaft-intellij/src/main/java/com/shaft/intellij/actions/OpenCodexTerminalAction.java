package com.shaft.intellij.actions;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.notifications.ShaftNotifier;
import com.shaft.intellij.ui.CliTerminalSupport;
import org.jetbrains.annotations.NotNull;

/**
 * Opens a real interactive terminal tab running the Codex CLI ({@code codex}, no {@code exec}
 * one-shot flag -- its normal interactive TUI) so power users can opt into the raw CLI experience
 * alongside the guided Assistant chat, not instead of it (issue #3959, follow-up to the #3917
 * embed-terminal spike's no-go on replacing the chat). Only ever invokes an already-installed
 * {@code codex} binary found on {@code PATH} -- nothing is bundled or redistributed.
 */
public final class OpenCodexTerminalAction extends AnAction implements DumbAware {
    private static final String EXECUTABLE = "codex";
    private static final String TAB_NAME = "Codex";
    private static final String NOTIFICATION_TITLE = "Codex terminal";
    private static final String INSTALL_HINT = "Codex CLI (`codex`) isn't on PATH. Install it first: "
            + "npm install -g @openai/codex";
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
