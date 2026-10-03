package com.shaft.intellij.locators;

import com.google.gson.JsonObject;
import com.intellij.codeInsight.daemon.LineMarkerInfo;
import com.intellij.codeInsight.daemon.LineMarkerProvider;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.markup.GutterIconRenderer;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiExpression;
import com.intellij.psi.PsiIdentifier;
import com.intellij.psi.PsiLiteralExpression;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.PsiMethodCallExpression;
import com.intellij.psi.PsiReferenceExpression;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.notifications.ShaftNotifier;
import com.shaft.intellij.project.ShaftProjectDetector;
import org.jetbrains.annotations.NotNull;

/**
 * Gutter action on {@code By.id/cssSelector/xpath/name/tagName/className("...")} that reports how
 * many elements the locator matches in the live SHAFT session (issue #6421).
 */
public final class LocatorMatchCountLineMarkerProvider implements LineMarkerProvider {
    @Override
    public LineMarkerInfo<?> getLineMarkerInfo(@NotNull PsiElement element) {
        if (!(element instanceof PsiIdentifier)
                || !(element.getParent() instanceof PsiReferenceExpression reference)
                || !(reference.getParent() instanceof PsiMethodCallExpression call)
                || !LocatorMatchCount.supports(reference.getReferenceName())
                || literal(call) == null
                || !ShaftProjectDetector.isShaftProject(element.getProject())) {
            return null;
        }
        PsiMethod method = call.resolveMethod();
        PsiClass owner = method == null ? null : method.getContainingClass();
        if (owner == null || !"org.openqa.selenium.By".equals(owner.getQualifiedName())) {
            return null;
        }
        return new LineMarkerInfo<>(element, element.getTextRange(), AllIcons.Actions.Find,
                com.intellij.util.FunctionUtil.constant("Count live matches for this locator"),
                (mouseEvent, identifier) -> check(identifier, reference.getReferenceName(), literal(call)),
                GutterIconRenderer.Alignment.LEFT, () -> "Count live matches for this locator");
    }

    private static String literal(PsiMethodCallExpression call) {
        PsiExpression[] arguments = call.getArgumentList().getExpressions();
        return arguments.length == 1 && arguments[0] instanceof PsiLiteralExpression literal
                && literal.getValue() instanceof String value ? value : null;
    }

    private static void check(PsiElement identifier, String factory, String value) {
        var project = identifier.getProject();
        JsonObject arguments = LocatorMatchCount.arguments(factory, value);
        ShaftMcpInvocationService.getInstance(project).startTool(LocatorMatchCount.TOOL_NAME, arguments).future()
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() ->
                        ShaftNotifier.info(project, "Locator check", "By." + factory + "(\"" + value + "\"): "
                                + LocatorMatchCount.message(error == null ? result : null))));
    }
}
