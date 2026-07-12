package com.shaft.intellij.testrunner;

import com.intellij.execution.PsiLocation;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.RunConfigurationProducer;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.util.PsiTreeUtil;
import com.shaft.intellij.project.ShaftProjectDetector;
import com.theoryinpractice.testng.configuration.TestNGConfiguration;
import com.theoryinpractice.testng.configuration.TestNGConfigurationType;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

/**
 * Creates IntelliJ's own {@link TestNGConfiguration} from a SHAFT test method/class context,
 * mirroring {@link ShaftJUnitRunConfigurationProducer} for TestNG {@code @Test} methods.
 * <p>
 * Registered only when the bundled TestNG-J plugin is enabled (see
 * {@code io.github.shafthq.shaft-withTestNG.xml}).
 */
public final class ShaftTestNgRunConfigurationProducer extends RunConfigurationProducer<TestNGConfiguration> {
    public ShaftTestNgRunConfigurationProducer() {
        super((ConfigurationFactory) TestNGConfigurationType.getInstance());
    }

    @Override
    protected boolean setupConfigurationFromContext(@NotNull TestNGConfiguration configuration,
                                                     @NotNull ConfigurationContext context,
                                                     @NotNull Ref<PsiElement> sourceElement) {
        if (!ShaftProjectDetector.isShaftProject(context.getProject())) {
            return false;
        }
        PsiElement element = context.getPsiLocation();
        PsiMethod method = PsiTreeUtil.getParentOfType(element, PsiMethod.class, false);
        if (method != null && ShaftTestMethodAnnotations.isShaftRunnableTestMethod(annotationQualifiedNames(method))) {
            configuration.setMethodConfiguration(PsiLocation.fromPsiElement(method));
            sourceElement.set(method);
            return true;
        }
        PsiClass psiClass = PsiTreeUtil.getParentOfType(element, PsiClass.class, false);
        if (psiClass != null && psiClass.getName() != null && hasRunnableMethod(psiClass)) {
            configuration.setClassConfiguration(psiClass);
            sourceElement.set(psiClass);
            return true;
        }
        return false;
    }

    @Override
    public boolean isConfigurationFromContext(@NotNull TestNGConfiguration configuration,
                                              @NotNull ConfigurationContext context) {
        PsiElement element = context.getPsiLocation();
        return element != null && configuration.isConfiguredByElement(element);
    }

    private static boolean hasRunnableMethod(PsiClass psiClass) {
        for (PsiMethod method : psiClass.getMethods()) {
            if (ShaftTestMethodAnnotations.isShaftRunnableTestMethod(annotationQualifiedNames(method))) {
                return true;
            }
        }
        return false;
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
