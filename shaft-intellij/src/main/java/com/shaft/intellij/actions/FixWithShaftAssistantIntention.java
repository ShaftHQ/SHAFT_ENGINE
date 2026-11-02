package com.shaft.intellij.actions;

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiJavaFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.shaft.intellij.notifications.ShaftToolWorkflowLauncher;
import com.shaft.intellij.project.ShaftProjectDetector;
import com.shaft.intellij.testindex.LastRunResults;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Path;

/**
 * Alt+Enter on a line where the last run failed: sends the failure to the SHAFT Assistant composer
 * (issue #6636), next to the inline failure hint from {@code LastRunInlayHintsProvider}.
 */
public final class FixWithShaftAssistantIntention implements IntentionAction {
    static final String TEXT = "Fix with SHAFT Assistant";

    @Override
    public @NotNull String getFamilyName() {
        return TEXT;
    }

    @Override
    public @NotNull String getText() {
        return TEXT;
    }

    @Override
    public boolean startInWriteAction() {
        return false;
    }

    @Override
    public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
        return failureAtCaret(project, editor, file) != null;
    }

    @Override
    public void invoke(@NotNull Project project, Editor editor, PsiFile file) {
        LastRunResults.Result failure = failureAtCaret(project, editor, file);
        if (failure != null) {
            ShaftToolWorkflowLauncher.prefillAssistant(project, prompt(failure));
        }
    }

    /** Assistant request for one failure; routes to code fixing through the "fix this code" intent. */
    static String prompt(LastRunResults.Result failure) {
        LastRunResults.Frame frame = failure.frame();
        String where = frame == null ? "" : " at " + frame.className() + "." + frame.method() + " line " + frame.line();
        String message = failure.message() == null || failure.message().isBlank()
                ? failure.status() : failure.message().strip();
        return "Fix this code: the SHAFT test " + failure.fullName() + " failed" + where + " with:\n" + message
                + "\nExplain the root cause and propose the smallest fix.";
    }

    private static LastRunResults.Result failureAtCaret(Project project, Editor editor, PsiFile file) {
        if (editor == null || project.getBasePath() == null || !(file instanceof PsiJavaFile)
                || !ShaftProjectDetector.isShaftProject(project)) {
            return null;
        }
        int offset = editor.getCaretModel().getOffset();
        PsiElement element = file.findElementAt(offset);
        if (element == null && offset > 0) {
            element = file.findElementAt(offset - 1);
        }
        PsiClass owner = PsiTreeUtil.getParentOfType(element, PsiClass.class, false);
        if (owner == null || owner.getQualifiedName() == null) {
            return null;
        }
        int line = editor.getDocument().getLineNumber(offset) + 1;
        return LastRunResults.failuresAt(LastRunResults.read(Path.of(project.getBasePath())), owner.getQualifiedName())
                .get(line);
    }
}
