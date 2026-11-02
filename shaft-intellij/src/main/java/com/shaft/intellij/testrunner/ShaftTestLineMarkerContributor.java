package com.shaft.intellij.testrunner;

import com.intellij.psi.PsiElement;
import org.jetbrains.uast.UAnnotation;
import org.jetbrains.uast.UElement;
import org.jetbrains.uast.UMethod;
import org.jetbrains.uast.UastUtils;

import com.intellij.execution.lineMarker.RunLineMarkerContributor;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.project.ShaftProjectDetector;

import java.util.ArrayList;
import java.util.List;

/**
 * Adds gutter run/debug icons on SHAFT test methods (TestNG or JUnit {@code @Test}) inside a
 * SHAFT project, for Java and (through UAST, #6427) Kotlin. Decision logic lives in {@link ShaftTestMethodAnnotations} (plain strings, unit
 * tested); this class is thin PSI glue: it resolves the method identifier under the caret,
 * extracts annotation fully-qualified names, and gates on {@link ShaftProjectDetector} the same
 * way {@code RecordShaftFlowHereAction} does.
 */
public final class ShaftTestLineMarkerContributor extends RunLineMarkerContributor {
    @Override
    public Info getInfo(PsiElement element) {
        UElement parent = UastUtils.getUParentForIdentifier(element);
        if (!(parent instanceof UMethod method) || method.getUastAnchor() == null
                || !element.equals(method.getUastAnchor().getSourcePsi())) {
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

    private static List<String> annotationQualifiedNames(UMethod method) {
        List<String> qualifiedNames = new ArrayList<>();
        for (UAnnotation annotation : method.getUAnnotations()) {
            String qualifiedName = annotation.getQualifiedName();
            if (qualifiedName != null) {
                qualifiedNames.add(qualifiedName);
            }
        }
        return qualifiedNames;
    }
}
