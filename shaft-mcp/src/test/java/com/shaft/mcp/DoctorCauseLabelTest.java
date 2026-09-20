package com.shaft.mcp;

import com.shaft.doctor.label.ConfirmedCauseLabelStore;
import com.shaft.doctor.model.CauseCategory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DoctorCauseLabelTest {
    @Test
    void confirmThenSuggestReturnsStoredEnvironment(@TempDir Path workspace) {
        DoctorService service = new DoctorService(McpWorkspacePolicy.of(workspace), new McpDoctorRemediationService());
        McpCauseLabelResult confirmed = service.causeLabel("confirm", "sig-grid", "ENVIRONMENT", null);
        assertEquals("ok", confirmed.status());
        assertEquals(CauseCategory.ENVIRONMENT_CONFIGURATION, confirmed.causeCategory());
        assertTrue(java.nio.file.Files.isRegularFile(workspace.resolve(ConfirmedCauseLabelStore.RELATIVE)));

        McpCauseLabelResult suggested = service.causeLabel("suggest", "sig-grid", null, null);
        assertTrue(suggested.matched());
        assertEquals("ENVIRONMENT", suggested.displayAlias());
        assertEquals(CauseCategory.ENVIRONMENT_CONFIGURATION, suggested.causeCategory());
    }

    @Test
    void evidencePathIsNotPersisted(@TempDir Path workspace) throws Exception {
        DoctorService service = new DoctorService(McpWorkspacePolicy.of(workspace), new McpDoctorRemediationService());
        service.causeLabel(
                "confirm",
                "sig-safe",
                "LOCATOR",
                workspace.resolve("allure-results/secret-result.json").toString());
        String raw = java.nio.file.Files.readString(workspace.resolve(ConfirmedCauseLabelStore.RELATIVE));
        assertFalse(raw.contains("allure-results"));
        assertFalse(raw.contains("secret-result"));
    }
}
