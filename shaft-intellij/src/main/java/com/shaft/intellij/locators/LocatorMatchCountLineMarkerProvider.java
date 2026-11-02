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
 * many elements the locator matches in the live SHAFT session (issue #6421) and outlines them in the
 * live browser (issue #6640), and the same for {@code Locator.hasTagName(..)...build()} chains with
 * literal arguments (issue #6445).
 */
public final class LocatorMatchCountLineMarkerProvider implements LineMarkerProvider {
    static final String TOOLTIP = "Highlight and count live matches for this locator";
    private static final String BUILDER = "com.shaft.gui.internal.locator.LocatorBuilder";
    private static final java.util.Set<String> STARTS = java.util.Set.of("hasTagName", "hasAnyTagName");

    @Override
    public LineMarkerInfo<?> getLineMarkerInfo(@NotNull PsiElement element) {
        if (!(element instanceof PsiIdentifier)
                || !(element.getParent() instanceof PsiReferenceExpression reference)
                || !(reference.getParent() instanceof PsiMethodCallExpression call)) {
            return null;
        }
        String name = reference.getReferenceName();
        JsonObject arguments;
        String label;
        if ("build".equals(name)) {
            java.util.List<java.util.List<String>> steps = builderSteps(call);
            if (steps == null) {
                return null;
            }
            arguments = LocatorMatchCount.chainArguments(steps);
            label = "Locator." + call.getMethodExpression().getQualifierExpression().getText().replaceFirst("^.*?Locator\\.", "");
        } else if (LocatorMatchCount.supports(name) && literal(call) != null && ownedBy(call, "org.openqa.selenium.By")) {
            arguments = LocatorMatchCount.arguments(name, literal(call));
            label = "By." + name + "(\"" + literal(call) + "\")";
        } else {
            return null;
        }
        if (!ShaftProjectDetector.isShaftProject(element.getProject())) {
            return null;
        }
        return new LineMarkerInfo<>(element, element.getTextRange(), AllIcons.Actions.Find,
                com.intellij.util.FunctionUtil.constant(TOOLTIP),
                (mouseEvent, identifier) -> check(identifier, label, arguments),
                GutterIconRenderer.Alignment.LEFT, () -> TOOLTIP);
    }

    /**
     * Steps of a SHAFT {@code Locator.hasTagName(..)...build()} chain with literal arguments (#6445),
     * or null when {@code build} is not such a chain.
     */
    public static java.util.List<java.util.List<String>> builderSteps(PsiMethodCallExpression build) {
        if (!ownedBy(build, BUILDER)) {
            return null;
        }
        java.util.LinkedList<java.util.List<String>> steps = new java.util.LinkedList<>();
        PsiExpression qualifier = build.getMethodExpression().getQualifierExpression();
        while (qualifier instanceof PsiMethodCallExpression step) {
            java.util.List<String> parts = new java.util.ArrayList<>();
            parts.add(step.getMethodExpression().getReferenceName());
            for (PsiExpression argument : step.getArgumentList().getExpressions()) {
                if (!(argument instanceof PsiLiteralExpression literal) || literal.getValue() == null) {
                    return null;
                }
                parts.add(String.valueOf(literal.getValue()));
            }
            steps.addFirst(parts);
            qualifier = step.getMethodExpression().getQualifierExpression();
        }
        return steps.isEmpty() || !STARTS.contains(steps.getFirst().get(0)) ? null : steps;
    }

    private static boolean ownedBy(PsiMethodCallExpression call, String className) {
        PsiMethod method = call.resolveMethod();
        PsiClass owner = method == null ? null : method.getContainingClass();
        return owner != null && className.equals(owner.getQualifiedName());
    }

    private static String literal(PsiMethodCallExpression call) {
        PsiExpression[] arguments = call.getArgumentList().getExpressions();
        return arguments.length == 1 && arguments[0] instanceof PsiLiteralExpression literal
                && literal.getValue() instanceof String value ? value : null;
    }

    private static void check(PsiElement identifier, String label, JsonObject arguments) {
        var project = identifier.getProject();
        ShaftMcpInvocationService.getInstance(project).startTool(LocatorMatchCount.TOOL_NAME, arguments).future()
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() ->
                        ShaftNotifier.info(project, "Locator check", label + ": "
                                + LocatorMatchCount.message(error == null ? result : null))));
    }
}
