package com.shaft.intellij.actions;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.Editor;
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

/**
 * Resolves the current Java caret context and starts a SHAFT flow recording anchored there.
 */
public final class RecordShaftFlowHereAction extends AnAction implements DumbAware {
    private static final String NOTIFICATION_TITLE = "Flow recording";

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
            ShaftNotifier.warn(project, NOTIFICATION_TITLE, "Open a Java file and place the caret inside a class or method.");
            return;
        }

        if (!ShaftSettingsState.getInstance().getState().advancedUiEnabled) {
            // Default mode: route to the Assistant with an equivalent plain-language request instead
            // of copying raw MCP JSON to the clipboard and leaving the user to paste it somewhere
            // (issue #3552) -- prefillTool() only exists on the raw Tools panel that stays hidden here.
            openAssistantPrompt(project, recordFlowPrompt(context));
            ShaftNotifier.info(project, NOTIFICATION_TITLE,
                    "Record-at-target request ready in the Assistant for " + context.displayName() + ".");
            return;
        }
        // Advanced mode (issue #3661): start a live capture_start recording anchored at the resolved
        // caret target directly, instead of copying a capture_record_at_target_code_blocks request to
        // the clipboard for the user to run manually after recording elsewhere -- RecorderToolPanel
        // #startRecordingAtTarget owns the live status indicator and routes into its review/insert
        // flow once the user stops, collapsing caret -> live recording -> review/insert into this one
        // action.
        startLiveRecording(project, context);
        ShaftNotifier.info(project, NOTIFICATION_TITLE,
                "Live SHAFT recording starting, anchored at " + context.displayName() + ".");
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

    /**
     * Plain-language Assistant equivalent of the {@code capture_record_at_target_code_blocks} MCP
     * request. Package-private for {@code RecordShaftFlowHereActionTest}.
     */
    static String recordFlowPrompt(JavaTargetContext context) {
        return "Record a SHAFT flow at " + context.methodName() + " in " + context.className();
    }

    private static void openAssistantPrompt(Project project, String text) {
        ToolWindowManager.getInstance(project).invokeLater(() -> {
            ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow("SHAFT");
            if (toolWindow == null) {
                return;
            }
            toolWindow.show(() -> {
                Content content = toolWindow.getContentManager().getContent(0);
                if (content != null && content.getComponent() instanceof ShaftToolWindowPanel panel) {
                    panel.prefillAssistantPrompt(text);
                }
            });
        });
    }

    private static void startLiveRecording(Project project, JavaTargetContext context) {
        ToolWindowManager.getInstance(project).invokeLater(() -> {
            ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow("SHAFT");
            if (toolWindow == null) {
                return;
            }
            toolWindow.show(() -> {
                Content content = toolWindow.getContentManager().getContent(0);
                if (content != null && content.getComponent() instanceof ShaftToolWindowPanel panel) {
                    panel.startRecordingAtTarget(context);
                }
            });
        });
    }
}
