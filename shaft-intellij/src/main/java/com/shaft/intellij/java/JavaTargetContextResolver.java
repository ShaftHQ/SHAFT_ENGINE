package com.shaft.intellij.java;

import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiJavaFile;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;

/**
 * Resolves IntelliJ PSI context for SHAFT Java source actions.
 */
public final class JavaTargetContextResolver {
    private JavaTargetContextResolver() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Resolves the nearest class and method around a caret offset.
     *
     * @param file PSI file
     * @param offset editor offset
     * @return Java context, or null when the file is not Java
     */
    public static JavaTargetContext resolve(PsiFile file, int offset) {
        if (!(file instanceof PsiJavaFile javaFile) || file.getVirtualFile() == null) {
            return null;
        }
        PsiElement element = file.findElementAt(Math.max(0, Math.min(offset, file.getTextLength() - 1)));
        PsiClass psiClass = PsiTreeUtil.getParentOfType(element, PsiClass.class, false);
        if (psiClass == null && javaFile.getClasses().length > 0) {
            psiClass = javaFile.getClasses()[0];
        }
        if (psiClass == null || psiClass.getName() == null) {
            return null;
        }
        PsiMethod method = PsiTreeUtil.getParentOfType(element, PsiMethod.class, false);
        String methodName = method == null || method.getName() == null ? psiClass.getName() : method.getName();
        return new JavaTargetContext(
                file.getVirtualFile().getPath(),
                javaFile.getPackageName(),
                psiClass.getName(),
                methodName);
    }
}
