package com.shaft.doctor.internal;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class DoctorProviderDiagnosisTest {

    @Test
    void firstFailedLocatorDoesNotSwallowLaterGetByRoleAfterByCssSelectorOnSameLine() {
        String evidence = "checklist: By.cssSelector: #submit-canary getByRole(\"button\", { name: \"Submit canary\" })";
        String truncatedSwallow = "By.cssSelector: #submit-canary getByRole(\"button\"";

        String locator = DoctorProviderDiagnosis.firstFailedLocator(evidence);

        assertFalse(locator.contains(truncatedSwallow),
                "Same-line By.cssSelector: must not swallow into truncated getByRole(\"button\": " + locator);
        assertEquals("getByRole(\"button\", { name: \"Submit canary\" })", locator);
    }
}
