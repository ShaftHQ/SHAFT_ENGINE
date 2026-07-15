package com.shaft.intellij.testrunner;

import com.intellij.execution.PsiLocation;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.LazyRunConfigurationProducer;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.junit.JUnitConfiguration;
import com.intellij.execution.junit.JUnitConfigurationType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.util.PsiTreeUtil;
import com.shaft.intellij.project.ShaftProjectDetector;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

/**
 * Creates IntelliJ's own {@link JUnitConfiguration} from a SHAFT test method/class context, so
 * the gutter run/debug icons ({@link ShaftTestLineMarkerContributor}) and "Run Context
 * Configuration" actions work for SHAFT tests without a bespoke SHAFT run-configuration type.
 * <p>
 * Registered only when the bundled JUnit plugin is enabled (see
 * {@code io.github.shafthq.shaft-withJUnit.xml}), since {@link JUnitConfiguration} classes live
 * in that plugin, not in {@code com.intellij.java} itself.
 */
public final class ShaftJUnitRunConfigurationProducer extends LazyRunConfigurationProducer<JUnitConfiguration> {
    @Override
    public @NotNull ConfigurationFactory getConfigurationFactory() {
        return JUnitConfigurationType.getInstance().getFactory();
    }

    @Override
    protected boolean setupConfigurationFromContext(@NotNull JUnitConfiguration configuration,
                                                     @NotNull ConfigurationContext context,
                                                     @NotNull Ref<PsiElement> sourceElement) {
        if (!ShaftProjectDetector.isShaftProject(context.getProject())) {
            return false;
        }
        PsiElement element = context.getPsiLocation();
        PsiMethod method = PsiTreeUtil.getParentOfType(element, PsiMethod.class, false);
        if (method != null && ShaftTestMethodAnnotations.isShaftRunnableTestMethod(annotationQualifiedNames(method))) {
            Module module = configuration.getPersistentData().setTestMethod(PsiLocation.fromPsiElement(method));
            if (module != null) {
                configuration.setModule(module);
            }
            configuration.setName(configuration.suggestedName());
            sourceElement.set(method);
            return true;
        }
        PsiClass psiClass = PsiTreeUtil.getParentOfType(element, PsiClass.class, false);
        if (psiClass != null && psiClass.getName() != null && hasRunnableMethod(psiClass)) {
            configuration.beClassConfiguration(psiClass);
            sourceElement.set(psiClass);
            return true;
        }
        return false;
    }

    @Override
    public boolean isConfigurationFromContext(@NotNull JUnitConfiguration configuration,
                                              @NotNull ConfigurationContext context) {
        PsiElement element = context.getPsiLocation();
        return element != null && configuration.isConfiguredByElement(element);
    }

    private static boolean hasRunnableMethod(PsiClass psiClass) {
        List<List<String>> methodAnnotations = new ArrayList<>();
        for (PsiMethod method : psiClass.getMethods()) {
            methodAnnotations.add(annotationQualifiedNames(method));
        }
        return ShaftTestMethodAnnotations.hasRunnableTestMethod(methodAnnotations);
    }

    private static List<String> annotationQualifiedNames(PsiMethod method) {
        List<String> qualifiedNames = new ArrayList<>();
        for (com.intellij.psi.PsiAnnotation annotation : method.getModifierList().getAnnotations()) {
            String qualifiedName = annotation.getQualifiedName();
            if (qualifiedName != null) {
                qualifiedNames.add(qualifiedName);
            }
        }
        return qualifiedNames;
    }
}
