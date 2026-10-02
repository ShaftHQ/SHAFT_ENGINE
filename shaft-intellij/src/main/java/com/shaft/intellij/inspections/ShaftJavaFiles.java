package com.shaft.intellij.inspections;

import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiImportStatementBase;
import com.intellij.psi.PsiJavaFile;

/** Java inspections run only in files that use SHAFT (issue #6419). */
final class ShaftJavaFiles {
    private ShaftJavaFiles() {
    }

    static boolean usesShaft(PsiFile file) {
        if (!(file instanceof PsiJavaFile javaFile) || javaFile.getImportList() == null) {
            return false;
        }
        for (PsiImportStatementBase statement : javaFile.getImportList().getAllImportStatements()) {
            String text = statement.getText();
            if (text.contains(" com.shaft.") || text.contains(" static com.shaft.")) {
                return true;
            }
        }
        return false;
    }
}
