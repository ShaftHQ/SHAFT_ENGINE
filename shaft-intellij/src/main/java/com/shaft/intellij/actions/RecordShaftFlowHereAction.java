package com.shaft.intellij.actions;

import com.google.gson.JsonObject;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.ui.content.Content;
import com.shaft.intellij.java.JavaTargetContext;
import com.shaft.intellij.java.JavaTargetContextResolver;
import com.shaft.intellij.notifications.ShaftNotifier;
import com.shaft.intellij.project.ShaftProjectDetector;
import com.shaft.intellij.settings.ShaftSettingsState;
import com.shaft.intellij.ui.ShaftToolWindowPanel;
import org.jetbrains.annotations.NotNull;

import java.awt.datatransfer.StringSelection;

/**
 * Prepares an MCP record-at-target request from the current Java caret context.
 */
public final class RecordShaftFlowHereAction extends AnAction implements DumbAware {
    private static final String DEFAULT_CAPTURE_RECORDING_PATH = "recordings/intellij-capture.json";

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        Editor editor = event.getData(CommonDataKeys.EDITOR);
        if (project == null || editor == null) {
            return;
        }
        PsiFile file = PsiDocumentManager.getInstance(project).getPsiFile(editor.getDocument());
        JavaTargetContext context = JavaTargetContextResolver.resolve(file, editor.getCaretModel().getOffset());
        if (context == null) {
            ShaftNotifier.warn(project, "SHAFT", "Open a Java file and place the caret inside a class or method.");
            return;
        }

        JsonObject arguments = new JsonObject();
        arguments.addProperty("sessionPath", DEFAULT_CAPTURE_RECORDING_PATH);
        arguments.addProperty("outputDirectory", project.getBasePath() == null ? "." : project.getBasePath());
        arguments.addProperty("packageName", context.packageName());
        arguments.addProperty("className", context.className());
        arguments.addProperty("overwrite", false);
        arguments.addProperty("targetSourcePath", context.sourcePath());
        arguments.addProperty("insertAfter", context.methodName());
        arguments.addProperty("driverVariableName", "driver");

        JsonObject request = new JsonObject();
        request.addProperty("tool", "capture_record_at_target_code_blocks");
        request.add("arguments", arguments);
        CopyPasteManager.getInstance().setContents(new StringSelection(request.toString()));
        if (!ShaftSettingsState.getInstance().getState().advancedUiEnabled) {
            // prefillTool silently no-ops while advanced workflows are off (the default), so notifying
            // "prepared" here would claim the tool window opened when nothing actually did.
            ShaftNotifier.warn(project, "SHAFT", "Record-at-target request copied to the clipboard for "
                    + context.displayName() + ". Enable advanced workflows in Settings | SHAFT to open it there.");
            return;
        }
        openToolWindow(project, arguments);
        ShaftNotifier.info(project, "SHAFT", "Record-at-target tool prepared for " + context.displayName() + ".");
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        Editor editor = event.getData(CommonDataKeys.EDITOR);
        boolean available = false;
        if (project != null && editor != null && ShaftProjectDetector.isShaftProject(project)) {
            PsiFile file = PsiDocumentManager.getInstance(project).getPsiFile(editor.getDocument());
            available = JavaTargetContextResolver.resolve(file, editor.getCaretModel().getOffset()) != null;
        }
        event.getPresentation().setEnabledAndVisible(available);
    }

    private static void openToolWindow(Project project, JsonObject arguments) {
        ToolWindowManager.getInstance(project).invokeLater(() -> {
            ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow("SHAFT");
            if (toolWindow == null) {
                return;
            }
            toolWindow.show(() -> {
                Content content = toolWindow.getContentManager().getContent(0);
                if (content == null) {
                    return;
                }
                if (content.getComponent() instanceof ShaftToolWindowPanel panel) {
                    panel.prefillTool("capture_record_at_target_code_blocks", arguments);
                }
            });
        });
    }
}
