package com.shaft.intellij.testindex;

import com.intellij.codeInsight.hints.declarative.EndOfLinePosition;
import com.intellij.codeInsight.hints.declarative.HintFormat;
import com.intellij.codeInsight.hints.declarative.InlayHintsCollector;
import com.intellij.codeInsight.hints.declarative.InlayHintsProvider;
import com.intellij.codeInsight.hints.declarative.InlineInlayPosition;
import com.intellij.codeInsight.hints.declarative.SharedBypassCollector;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiMethod;
import com.shaft.intellij.project.ShaftProjectDetector;
import kotlin.Unit;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Path;
import java.util.Map;

/**
 * Shows the last-run status and duration after each test method name (issue #6424), and the failure
 * message at the end of each failing line (issue #6636), read from the newest allure-results.
 * Toggle it under Settings, Editor, Inlay Hints.
 */
public final class LastRunInlayHintsProvider implements InlayHintsProvider {
    public static final String PROVIDER_ID = "shaft.lastRun";

    @Override
    public InlayHintsCollector createCollector(@NotNull PsiFile file, @NotNull Editor editor) {
        String base = file.getProject().getBasePath();
        if (base == null || !ShaftProjectDetector.isShaftProject(file.getProject())) {
            return null;
        }
        Map<String, LastRunResults.Result> results = LastRunResults.read(Path.of(base));
        int lineCount = editor.getDocument().getLineCount();
        return results.isEmpty() ? null : (SharedBypassCollector) (element, sink) -> {
            if (element instanceof PsiClass owner && owner.getQualifiedName() != null) {
                failureHints(results, owner.getQualifiedName(), lineCount).forEach((line, text) ->
                        sink.addPresentation(new EndOfLinePosition(line), null, null,
                                HintFormat.Companion.getDefault(), builder -> {
                                    builder.text(text, null);
                                    return Unit.INSTANCE;
                                }));
                return;
            }
            String label = label(results, element);
            if (label != null) {
                PsiElement name = ((PsiMethod) element).getNameIdentifier();
                sink.addPresentation(new InlineInlayPosition(name.getTextRange().getEndOffset(), true, 0),
                        null, null, HintFormat.Companion.getDefault(), builder -> {
                            builder.text(label, null);
                            return Unit.INSTANCE;
                        });
            }
        };
    }

    /** End-of-line failure text keyed by 0-based line, for lines inside a {@code lineCount}-line file (#6636). */
    static Map<Integer, String> failureHints(Map<String, LastRunResults.Result> results, String qualifiedClass,
                                             int lineCount) {
        Map<Integer, String> hints = new java.util.TreeMap<>();
        LastRunResults.failuresAt(results, qualifiedClass).forEach((line, result) -> {
            if (line >= 1 && line <= lineCount) {
                hints.put(line - 1, result.failureHint());
            }
        });
        return hints;
    }

    static String label(Map<String, LastRunResults.Result> results, PsiElement element) {
        if (!(element instanceof PsiMethod method) || method.getNameIdentifier() == null) {
            return null;
        }
        PsiClass owner = method.getContainingClass();
        String qualified = owner == null ? null : owner.getQualifiedName();
        LastRunResults.Result result = qualified == null ? null : results.get(qualified + "#" + method.getName());
        return result == null ? null : result.label();
    }
}
