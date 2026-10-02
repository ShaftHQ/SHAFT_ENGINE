package com.shaft.intellij.inspections;

import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.project.Project;
import com.intellij.psi.JavaElementVisitor;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.PsiMethodCallExpression;
import com.intellij.psi.PsiReferenceExpression;
import com.intellij.psi.javadoc.PsiDocComment;
import com.intellij.psi.javadoc.PsiDocTag;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Flags calls to deprecated SHAFT APIs and, when the {@code @deprecated} Javadoc links a
 * same-class replacement, offers a one-click migration (issue #6419).
 */
public final class ShaftDeprecatedApiInspection extends LocalInspectionTool {
    private static final Pattern LINKED_METHOD = Pattern.compile("\\{@link\\s+[\\w.]*#(\\w+)\\s*(?:\\(|\\})");

    @Override
    public @NotNull PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly) {
        if (!ShaftJavaFiles.usesShaft(holder.getFile())) {
            return PsiElementVisitor.EMPTY_VISITOR;
        }
        return new JavaElementVisitor() {
            @Override
            public void visitMethodCallExpression(@NotNull PsiMethodCallExpression call) {
                super.visitMethodCallExpression(call);
                PsiMethod method = call.resolveMethod();
                PsiClass owner = method == null ? null : method.getContainingClass();
                String ownerName = owner == null ? null : owner.getQualifiedName();
                if (ownerName == null || !ownerName.startsWith("com.shaft.") || !method.isDeprecated()) {
                    return;
                }
                PsiElement name = call.getMethodExpression().getReferenceNameElement();
                String replacement = replacement(method);
                if (name == null) {
                    return;
                }
                if (replacement == null) {
                    holder.registerProblem(name, "Deprecated SHAFT API '" + method.getName() + "'",
                            ProblemHighlightType.LIKE_DEPRECATED);
                } else {
                    holder.registerProblem(name, "Deprecated SHAFT API '" + method.getName() + "': use '" + replacement + "'",
                            ProblemHighlightType.LIKE_DEPRECATED, new UseReplacementFix(replacement));
                }
            }
        };
    }

    /** The replacement method named by {@code @deprecated {@link Owner#name}}, when it is in the same class. */
    static @Nullable String replacement(PsiMethod method) {
        PsiDocComment doc = method.getDocComment();
        PsiDocTag tag = doc == null ? null : doc.findTagByName("deprecated");
        if (tag == null || method.getContainingClass() == null) {
            return null;
        }
        Matcher matcher = LINKED_METHOD.matcher(tag.getText());
        if (!matcher.find()) {
            return null;
        }
        String name = matcher.group(1);
        return !name.equals(method.getName()) && method.getContainingClass().findMethodsByName(name, true).length > 0
                ? name : null;
    }

    private static final class UseReplacementFix implements LocalQuickFix {
        private final String replacement;

        private UseReplacementFix(String replacement) {
            this.replacement = replacement;
        }

        @Override
        public @NotNull String getName() {
            return "Replace with '" + replacement + "'";
        }

        @Override
        public @NotNull String getFamilyName() {
            return "Migrate deprecated SHAFT API";
        }

        @Override
        public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
            if (descriptor.getPsiElement().getParent() instanceof PsiReferenceExpression reference) {
                reference.handleElementRename(replacement);
            }
        }
    }
}
