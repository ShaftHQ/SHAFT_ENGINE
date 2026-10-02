package com.shaft.intellij.inspections;

import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.JavaElementVisitor;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.PsiMethodCallExpression;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

/**
 * Flags {@code Thread.sleep} in SHAFT test classes (issue #6419): fixed sleeps are the top source
 * of slow, flaky UI tests, and SHAFT already waits for elements and lazy loading.
 */
public final class ShaftThreadSleepInspection extends LocalInspectionTool {
    static final String MESSAGE = "Thread.sleep in a SHAFT test: rely on SHAFT's built-in element and lazy-loading waits, "
            + "or wait for an explicit condition";

    @Override
    public @NotNull PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly) {
        if (!ShaftJavaFiles.usesShaft(holder.getFile())) {
            return PsiElementVisitor.EMPTY_VISITOR;
        }
        return new JavaElementVisitor() {
            @Override
            public void visitMethodCallExpression(@NotNull PsiMethodCallExpression call) {
                super.visitMethodCallExpression(call);
                if (!"sleep".equals(call.getMethodExpression().getReferenceName())) {
                    return;
                }
                PsiMethod method = call.resolveMethod();
                PsiClass owner = method == null ? null : method.getContainingClass();
                if (owner != null && "java.lang.Thread".equals(owner.getQualifiedName()) && inTestClass(call)) {
                    holder.registerProblem(call, MESSAGE);
                }
            }
        };
    }

    private static boolean inTestClass(PsiMethodCallExpression call) {
        PsiClass type = PsiTreeUtil.getParentOfType(call, PsiClass.class);
        if (type == null) {
            return false;
        }
        for (PsiMethod method : type.getMethods()) {
            if (method.hasAnnotation("org.testng.annotations.Test") || method.hasAnnotation("org.junit.jupiter.api.Test")
                    || method.hasAnnotation("org.junit.Test")) {
                return true;
            }
        }
        return type.getName() != null && type.getName().endsWith("Test");
    }
}
