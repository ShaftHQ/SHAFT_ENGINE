package com.shaft.intellij.ui.firstrun;

import com.intellij.openapi.project.Project;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Proxy;

import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FirstRunUxScoreTest {
    @Test
    void failingInspectionScoresUnderEight() {
        assertTrue(FirstRunUxScore.outOfTen(WizardInspection.failing()) < 8);
        assertTrue(FirstRunUxScore.outOfTen(WizardInspection.of(
                true, false, true, false, false, false, false, false, false, false)) < 8);
    }

    @Test
    void realWizardScoresAtLeastEightAndIsNotAConstant() {
        FirstRunWizardPanel wizard = new FirstRunWizardPanel(project(), new ShaftSettingsState.Settings(), () -> { },
                (client, runtime) -> ShaftMcpToolResult.failure("exit code 9"),
                () -> new PrerequisitePlan.Snapshot(true, "3.9.9", true));
        wizard.primary().doClick();
        wizard.primary().doClick();
        wizard.primary().doClick();
        wizard.primary().doClick();
        WizardInspection inspection = WizardInspection.capture(wizard);
        int score = FirstRunUxScore.outOfTen(inspection);
        assertTrue(score >= 8, "score=" + score);
        assertNotEquals(score, FirstRunUxScore.outOfTen(WizardInspection.failing()));
    }

    private static Project project() {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> switch (method.getName()) {
                    case "equals" -> proxy == (arguments == null ? null : arguments[0]);
                    case "hashCode" -> System.identityHashCode(proxy);
                    case "toString" -> "score";
                    case "getBasePath" -> "";
                    case "getName" -> "score";
                    case "isDisposed" -> false;
                    default -> method.getReturnType() == boolean.class ? false : (method.getReturnType() == int.class ? 0 : null);
                });
    }
}
