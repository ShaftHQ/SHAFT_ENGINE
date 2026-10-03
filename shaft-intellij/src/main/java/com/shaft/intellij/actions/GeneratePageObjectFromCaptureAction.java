package com.shaft.intellij.actions;

import com.google.gson.JsonObject;
import com.intellij.openapi.actionSystem.ActionUpdateThread;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.notifications.ShaftNotifier;
import com.shaft.intellij.project.ShaftProjectDetector;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.nio.file.Path;

/**
 * Generates a Page Object from the newest Capture recording through {@code capture_code_blocks},
 * previews it, and writes it under {@code src/main/java/pages} only after confirmation (issue #6422).
 */
public final class GeneratePageObjectFromCaptureAction extends AnAction implements DumbAware {
    static final String PACKAGE = "pages";
    private static final String TITLE = "Page Object from capture";

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        if (project == null || project.getBasePath() == null) {
            return;
        }
        JsonObject arguments = new JsonObject();
        arguments.addProperty("backend", "web");
        ShaftMcpInvocationService.getInstance(project).startTool("capture_code_blocks", arguments).future()
                .whenComplete((result, error) -> ApplicationManager.getApplication()
                        .invokeLater(() -> preview(project, error == null ? result : null)));
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        event.getPresentation().setEnabledAndVisible(project != null && ShaftProjectDetector.isShaftProject(project));
    }

    @Override
    public @NotNull ActionUpdateThread getActionUpdateThread() {
        return ActionUpdateThread.BGT;
    }

    private static void preview(Project project, ShaftMcpToolResult result) {
        PageObjectFromCapture.Draft draft = result == null || !result.success()
                ? null : PageObjectFromCapture.draft(result.output(), PACKAGE);
        if (draft == null) {
            ShaftNotifier.warn(project, TITLE, "The newest Capture recording has no Page Object draft. "
                    + "Record a flow with at least one locator and one action, then try again.");
            return;
        }
        if (Messages.showOkCancelDialog(project, draft.source(), "Create " + draft.relativePath() + "?",
                "Create", "Cancel", null) != Messages.OK) {
            return;
        }
        WriteCommandAction.writeCommandAction(project).withName("Create SHAFT Page Object").run(() -> {
            try {
                Path target = Path.of(project.getBasePath(), "src/main/java", draft.relativePath());
                VirtualFile folder = VfsUtil.createDirectoryIfMissing(target.getParent().toString());
                if (folder == null || folder.findChild(target.getFileName().toString()) != null) {
                    ShaftNotifier.warn(project, TITLE, draft.relativePath() + " already exists; nothing was overwritten.");
                    return;
                }
                VirtualFile file = folder.createChildData(GeneratePageObjectFromCaptureAction.class,
                        target.getFileName().toString());
                VfsUtil.saveText(file, draft.source());
                FileEditorManager.getInstance(project).openFile(file, true);
            } catch (IOException failure) {
                ShaftNotifier.warn(project, TITLE, "Could not write the Page Object: " + failure.getMessage());
            }
        });
    }
}
