package com.shaft.intellij.testrunner;

import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiMethod;
import org.jetbrains.uast.UClass;
import org.jetbrains.uast.UElement;
import org.jetbrains.uast.UMethod;
import org.jetbrains.uast.UastContextKt;
import org.jetbrains.uast.UastUtils;

/**
 * Resolves the enclosing test method or class through UAST, so run-configuration producers
 * work for Java and Kotlin alike (Kotlin yields light Java PSI that JUnit and TestNG accept).
 */
final class ShaftTestContext {
    private ShaftTestContext() {
    }

    static PsiMethod method(PsiElement element) {
        UMethod method = UastUtils.getParentOfType(uElement(element), UMethod.class, false);
        return method == null ? null : method.getJavaPsi();
    }

    static PsiClass testClass(PsiElement element) {
        UClass psiClass = UastUtils.getParentOfType(uElement(element), UClass.class, false);
        return psiClass == null ? null : psiClass.getJavaPsi();
    }

    private static UElement uElement(PsiElement element) {
        for (PsiElement current = element; current != null; current = current.getParent()) {
            UElement uElement = UastContextKt.toUElement(current);
            if (uElement != null) {
                return uElement;
            }
        }
        return null;
    }
}
