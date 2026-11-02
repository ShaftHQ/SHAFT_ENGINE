package com.shaft.intellij.java;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;

/**
 * IntelliJ platform wiring for {@link InsertionAnchor#choose}: resolves the caret's enclosing
 * Java method via {@link JavaTargetContextResolver}, falls back to whatever file is selected in
 * the editor tab strip.
 */
public final class InsertionAnchorResolver {
    private InsertionAnchorResolver() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Resolves the shared insertion anchor for {@code project}/{@code editor}.
     *
     * @param project current project, or null
     * @param editor editor holding the caret, or null when none is focused
     * @return the chosen anchor, or null when neither a resolvable caret nor an open file exists
     */
    public static InsertionAnchor resolve(Project project, Editor editor) {
        if (project == null) {
            return null;
        }
        JavaTargetContext caretContext = null;
        if (editor != null) {
            PsiFile file = PsiDocumentManager.getInstance(project).getPsiFile(editor.getDocument());
            caretContext = JavaTargetContextResolver.resolve(file, editor.getCaretModel().getOffset());
        }
        return InsertionAnchor.choose(caretContext, openEditorFilePath(project));
    }

    private static String openEditorFilePath(Project project) {
        try {
            var selectedFiles = FileEditorManager.getInstance(project).getSelectedFiles();
            return selectedFiles.length == 0 ? "" : selectedFiles[0].getPath();
        } catch (RuntimeException | Error headlessTestEnvironment) {
            return "";
        }
    }
}
