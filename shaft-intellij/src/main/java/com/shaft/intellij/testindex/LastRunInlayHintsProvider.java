package com.shaft.intellij.testindex;

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
 * Shows the last-run status and duration after each test method name (issue #6424), read from the
 * newest allure-results. Toggle it under Settings, Editor, Inlay Hints.
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
        return results.isEmpty() ? null : (SharedBypassCollector) (element, sink) -> {
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
