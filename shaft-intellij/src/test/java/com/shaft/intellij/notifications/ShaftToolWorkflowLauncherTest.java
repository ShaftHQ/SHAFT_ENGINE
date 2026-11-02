package com.shaft.intellij.notifications;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Covers the plain-language Assistant prompt {@code ShaftToolWorkflowLauncher} routes to in
 * default mode (advancedUiEnabled=false) instead of the old dead-end warning that told the user
 * to retype the request themselves (issue #3552). The rest of {@link ShaftToolWorkflowLauncher}
 * drives {@code ToolWindowManager}/{@code Content}, which need a live IntelliJ platform and are
 * not exercised by this lightweight Gradle unit test JVM (same gap {@code
 * FailedRunDoctorNotifierTest} documents for the sibling notifier).
 */
class ShaftToolWorkflowLauncherTest {
    @Test
    void doctorToolNameMapsToADiagnoseRequest() {
        assertEquals("Diagnose my last failed test run",
                ShaftToolWorkflowLauncher.assistantPromptFor("doctor_analyze_failed_allure"));
    }

    @Test
    void healerToolNameMapsToAHealRequest() {
        assertEquals("Heal my last failed test run",
                ShaftToolWorkflowLauncher.assistantPromptFor("healer_run_failed_test"));
    }
}
