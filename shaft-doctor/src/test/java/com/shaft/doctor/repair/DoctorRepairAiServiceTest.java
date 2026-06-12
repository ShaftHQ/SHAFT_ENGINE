package com.shaft.doctor.repair;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.doctor.model.CauseCategory;
import com.shaft.doctor.model.Confidence;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.AiUsage;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceCategory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.EnumSet;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DoctorRepairAiServiceTest {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final ApprovalPolicy APPROVED =
            new ApprovalPolicy(false, true, EnumSet.allOf(EvidenceCategory.class));

    @Test
    void providerReturnsOnlyStructuredAllowlistedFilePatches(@TempDir Path temp) throws Exception {
        Path source = Files.createDirectories(temp.resolve("src")).resolve("Example.java");
        Files.writeString(source, "final class Example { int value() { return 1; } }\n");
        AtomicReference<AiRequest> captured = new AtomicReference<>();
        DoctorRepairAiService service = new DoctorRepairAiService(request -> {
            captured.set(request);
            var payload = JSON.createObjectNode();
            payload.put("schemaVersion", "1.0");
            payload.putArray("patches").addObject()
                    .put("path", "src/Example.java")
                    .put("operation", "REPLACE")
                    .put("content", "final class Example { int value() { return 2; } }\n")
                    .put("rationale", "Correct the diagnosed behavior.")
                    .putArray("evidenceIds").add("src/Example.java");
            return AiResponse.success("test-provider", "test-model", payload,
                    Duration.ofMillis(1), AiUsage.empty(), request.deterministicFallback());
        });

        DoctorRepairPatchResult result = service.generate(
                temp, diagnosis(), List.of("src/Example.java"),
                DoctorRepairAiRequest.defaults(APPROVED));

        assertTrue(result.successful());
        assertEquals(List.of("src/Example.java"),
                captured.get().evidence().stream().map(value -> value.id()).toList());
        assertEquals("src/Example.java", result.patches().getFirst().path());
        assertTrue(captured.get().text().contains("Never return shell commands"));
    }

    @Test
    void inventedPathsAndSecretBearingOutputReturnNoPatch(@TempDir Path temp) throws Exception {
        Path source = Files.createDirectories(temp.resolve("src")).resolve("Example.java");
        Files.writeString(source, "final class Example {}\n");
        DoctorRepairAiService invented = new DoctorRepairAiService(request -> response(
                request, "../outside.java", "final class Outside {}"));
        DoctorRepairAiService secret = new DoctorRepairAiService(request -> response(
                request, "src/Example.java", "Authorization: Bearer provider-secret"));

        DoctorRepairPatchResult inventedResult = invented.generate(
                temp, diagnosis(), List.of("src/Example.java"),
                DoctorRepairAiRequest.defaults(APPROVED));
        DoctorRepairPatchResult secretResult = secret.generate(
                temp, diagnosis(), List.of("src/Example.java"),
                DoctorRepairAiRequest.defaults(APPROVED));

        assertEquals(AiResponseStatus.INVALID_RESPONSE, inventedResult.status());
        assertTrue(inventedResult.patches().isEmpty());
        assertEquals(AiResponseStatus.INVALID_RESPONSE, secretResult.status());
        assertTrue(secretResult.patches().isEmpty());
    }

    @Test
    void missingSourceConsentDoesNotCallProvider(@TempDir Path temp) throws Exception {
        Path source = Files.createDirectories(temp.resolve("src")).resolve("Example.java");
        Files.writeString(source, "final class Example {}\n");
        DoctorRepairAiService service = new DoctorRepairAiService(request -> {
            throw new AssertionError("Provider must not be called without source consent.");
        });
        ApprovalPolicy textOnly = new ApprovalPolicy(false, true, EnumSet.of(EvidenceCategory.TEXT));

        DoctorRepairPatchResult result = service.generate(
                temp, diagnosis(), List.of("src/Example.java"),
                DoctorRepairAiRequest.defaults(textOnly));

        assertEquals(AiResponseStatus.CONSENT_REQUIRED, result.status());
        assertTrue(result.patches().isEmpty());
    }

    private static AiResponse response(AiRequest request, String path, String content) {
        var payload = JSON.createObjectNode();
        payload.put("schemaVersion", "1.0");
        payload.putArray("patches").addObject()
                .put("path", path)
                .put("operation", "REPLACE")
                .put("content", content)
                .put("rationale", "Repair")
                .putArray("evidenceIds");
        return AiResponse.success("test-provider", "test-model", payload,
                Duration.ofMillis(1), AiUsage.empty(), request.deterministicFallback());
    }

    private static Diagnosis diagnosis() {
        return new Diagnosis(
                Diagnosis.CURRENT_SCHEMA_VERSION,
                CauseCategory.TEST,
                List.of(),
                Confidence.HIGH,
                "A deterministic defect was identified.",
                "The submitted evidence points to the test implementation.",
                List.of(),
                List.of(),
                List.of());
    }
}
