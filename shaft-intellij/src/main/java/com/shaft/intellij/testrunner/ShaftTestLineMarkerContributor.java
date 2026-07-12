package com.shaft.intellij.testrunner;

import com.intellij.execution.lineMarker.RunLineMarkerContributor;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiAnnotation;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiIdentifier;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.PsiModifierList;
import com.shaft.intellij.project.ShaftProjectDetector;

import java.util.ArrayList;
import java.util.List;

/**
 * Adds gutter run/debug icons on SHAFT test methods (TestNG or JUnit {@code @Test}) inside a
 * SHAFT project. Decision logic lives in {@link ShaftTestMethodAnnotations} (plain strings, unit
 * tested); this class is thin PSI glue: it resolves the method identifier under the caret,
 * extracts annotation fully-qualified names, and gates on {@link ShaftProjectDetector} the same
 * way {@code RecordShaftFlowHereAction} does.
 */
public final class ShaftTestLineMarkerContributor extends RunLineMarkerContributor {
    @Override
    public Info getInfo(PsiElement element) {
        if (!(element instanceof PsiIdentifier) || !(element.getParent() instanceof PsiMethod method)) {
            return null;
        }
        if (!element.equals(method.getNameIdentifier())) {
            return null;
        }
        Project project = element.getProject();
        if (!ShaftProjectDetector.isShaftProject(project)) {
            return null;
        }
        if (!ShaftTestMethodAnnotations.isShaftRunnableTestMethod(annotationQualifiedNames(method))) {
            return null;
        }
        return RunLineMarkerContributor.withExecutorActions(AllIcons.RunConfigurations.TestState.Run);
    }

    private static List<String> annotationQualifiedNames(PsiMethod method) {
        PsiModifierList modifiers = method.getModifierList();
        List<String> qualifiedNames = new ArrayList<>();
        for (PsiAnnotation annotation : modifiers.getAnnotations()) {
            String qualifiedName = annotation.getQualifiedName();
            if (qualifiedName != null) {
                qualifiedNames.add(qualifiedName);
            }
        }
        return qualifiedNames;
    }
}
