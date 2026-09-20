package com.shaft.mcp;

import com.shaft.doctor.history.LocalMuteModels;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportMuteTest {

    @Test
    void reportMuteMutesListsObservesAndRecoversWithoutSurefire(@TempDir Path workspace) {
        TraceService service = new TraceService(
                McpWorkspacePolicy.of(workspace), new McpDoctorRemediationService());

        LocalMuteModels.MuteTable muted = service.reportMute(
                "mute",
                "com.example.FlakyTest#testFlip",
                "timeout flake",
                null,
                2,
                null,
                true);
        assertFalse(muted.empty());
        assertFalse(muted.surefireExcludeWritten());
        assertFalse(muted.writeSurefireExcludes());
        assertTrue(muted.warnings().stream().anyMatch(w -> w.toLowerCase().contains("surefire")));

        assertEquals(1, service.reportMute("list", null, null, null, null, null, null).entries().size());

        service.reportMute("observe", "com.example.FlakyTest#testFlip", null, null, null, true, null);
        LocalMuteModels.MuteTable recovered = service.reportMute(
                "observe", "com.example.FlakyTest#testFlip", null, null, null, true, null);
        assertTrue(recovered.entries().stream()
                .anyMatch(e -> e.status() == LocalMuteModels.MuteStatus.RECOVERED));
        assertTrue(service.reportMute("list", null, null, null, null, null, null).empty());
    }
}
