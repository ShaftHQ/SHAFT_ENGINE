package com.shaft.intellij.properties;

import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.lang.properties.psi.Property;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import org.jetbrains.annotations.NotNull;

/**
 * Flags likely-misspelled SHAFT keys (a typo silently falls back to the default) and values of
 * the wrong type (issue #6417). Keys with no close SHAFT match are left alone: custom.properties
 * legitimately holds project-specific keys too.
 */
public final class ShaftPropertyInspection extends LocalInspectionTool {
    @Override
    public @NotNull PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly) {
        if (!ShaftPropertiesFiles.applies(holder.getFile())) {
            return PsiElementVisitor.EMPTY_VISITOR;
        }
        return new PsiElementVisitor() {
            @Override
            public void visitElement(@NotNull PsiElement element) {
                if (element instanceof Property property) {
                    check(property, holder);
                }
            }
        };
    }

    private static void check(Property property, ProblemsHolder holder) {
        String key = property.getUnescapedKey();
        PsiElement keyNode = property.getFirstChild();
        if (key == null || keyNode == null) {
            return;
        }
        ShaftProperty known = ShaftPropertyCatalog.find(key);
        if (known == null) {
            String suggestion = ShaftPropertyCatalog.suggest(key);
            if (suggestion != null) {
                holder.registerProblem(keyNode, "Unknown SHAFT property '" + key + "'. Did you mean '" + suggestion + "'?",
                        ProblemHighlightType.WARNING, new RenameKeyFix(suggestion));
            }
            return;
        }
        String value = property.getUnescapedValue();
        String problem = value == null ? null : ShaftPropertyCatalog.valueProblem(known, value);
        if (problem != null && property.getLastChild() != null) {
            holder.registerProblem(property.getLastChild(), problem + " for '" + key + "'", ProblemHighlightType.WARNING);
        }
    }

    private static final class RenameKeyFix implements LocalQuickFix {
        private final String key;

        private RenameKeyFix(String key) {
            this.key = key;
        }

        @Override
        public @NotNull String getName() {
            return "Change to '" + key + "'";
        }

        @Override
        public @NotNull String getFamilyName() {
            return "Fix SHAFT property key";
        }

        @Override
        public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
            if (descriptor.getPsiElement().getParent() instanceof Property property) {
                property.setName(key);
            }
        }
    }
}
