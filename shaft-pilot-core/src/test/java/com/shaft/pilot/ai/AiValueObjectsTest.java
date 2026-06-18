package com.shaft.pilot.ai;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.Duration;
import java.util.EnumSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiValueObjectsTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test
    void aiImageDefensivelyCopiesDataAndDefaultsMediaType() {
        byte[] bytes = {1, 2, 3};
        AiImage image = new AiImage(EvidenceCategory.SCREENSHOT, null, bytes);

        bytes[0] = 9;
        byte[] returned = image.data();
        returned[1] = 8;

        assertEquals("image/png", image.mediaType());
        assertArrayEquals(new byte[]{1, 2, 3}, image.data());
    }

    @Test
    void availabilityFactoriesNormalizeReasons() {
        assertTrue(AiProviderAvailability.ready().available());
        assertEquals("", AiProviderAvailability.ready().reason());
        assertFalse(AiProviderAvailability.unavailable(null).available());
        assertEquals("Provider unavailable", AiProviderAvailability.unavailable(null).reason());
    }

    @Test
    void disabledProviderReturnsDeterministicFallback() {
        var fallback = JSON.createObjectNode().put("answer", "deterministic");
        AiRequest request = AiRequest.builder("disabled-provider", JSON.createObjectNode())
                .deterministicFallback(fallback)
                .build();

        AiResponse response = new DisabledAiProvider().execute(request);

        assertEquals("none", new DisabledAiProvider().id());
        assertFalse(new DisabledAiProvider().capabilities().structuredOutput());
        assertFalse(new DisabledAiProvider().availability().available());
        assertEquals(AiResponseStatus.DISABLED, response.status());
        assertEquals("deterministic", response.structuredPayload().path("answer").asText());
    }

    @Test
    void approvalPolicyRequiresLocationAndAllCategories() {
        ApprovalPolicy policy = new ApprovalPolicy(true, true, false, EnumSet.of(EvidenceCategory.TEXT));

        assertTrue(policy.allows(ProcessingLocation.LOCAL, Set.of(EvidenceCategory.TEXT)));
        assertTrue(policy.allows(ProcessingLocation.ON_PREM, Set.of(EvidenceCategory.TEXT)));
        assertFalse(policy.allows(ProcessingLocation.REMOTE, Set.of(EvidenceCategory.TEXT)));
        assertFalse(policy.allows(ProcessingLocation.NONE, Set.of()));
        assertFalse(policy.allows(ProcessingLocation.LOCAL, Set.of(EvidenceCategory.TEXT, EvidenceCategory.DOM)));
    }

    @Test
    void requestBuilderCopiesEvidenceAndCollectsCategories() {
        var schema = JSON.createObjectNode().put("type", "object");
        var fallback = JSON.createObjectNode().put("answer", "deterministic");
        AiRequest request = AiRequest.builder("request-builder", schema)
                .requestId("req-1")
                .text("body")
                .evidence(new EvidenceReference("e1", EvidenceCategory.DOM, "text/html", "<main/>"))
                .image(new AiImage(EvidenceCategory.SCREENSHOT, "image/png", new byte[]{1}))
                .timeout(Duration.ofSeconds(2))
                .budget(new AiBudget(10, 1, BigDecimal.ONE))
                .approvalPolicy(new ApprovalPolicy(true, false, false, EnumSet.allOf(EvidenceCategory.class)))
                .deterministicFallback(fallback)
                .build();

        schema.put("extra", true);
        fallback.put("answer", "mutated");

        assertEquals("req-1", request.requestId());
        assertEquals(Set.of(EvidenceCategory.TEXT, EvidenceCategory.DOM, EvidenceCategory.SCREENSHOT),
                request.evidenceCategories());
        assertFalse(request.desiredResponseSchema().has("extra"));
        assertEquals("deterministic", request.deterministicFallback().path("answer").asText());
        assertThrows(UnsupportedOperationException.class,
                () -> request.evidence().add(new EvidenceReference("x", EvidenceCategory.TEXT, "text/plain", "x")));
    }

    @Test
    void requestRejectsBlankPurposeAndNonPositiveTimeout() {
        assertThrows(IllegalArgumentException.class,
                () -> AiRequest.builder(" ", JSON.createObjectNode()).build());
        assertThrows(IllegalArgumentException.class,
                () -> AiRequest.builder("bad-timeout", JSON.createObjectNode())
                        .timeout(Duration.ZERO)
                        .build());
    }

    @Test
    void responseDefaultsAndCopiesPayloads() {
        var payload = JSON.createObjectNode().put("answer", "ok");
        AiResponse response = AiResponse.success("provider", "model", payload, null, null, payload);

        payload.put("answer", "mutated");

        assertTrue(response.successful());
        assertEquals("ok", response.structuredPayload().path("answer").asText());
        assertEquals(Duration.ZERO, response.duration());
        assertEquals(AiUsage.empty(), response.usage());
        assertThrows(UnsupportedOperationException.class, () -> response.warnings().add("x"));
    }

    @Test
    void enumValueOfAndNamesRemainStable() {
        assertEquals(AiResponseStatus.SUCCESS, AiResponseStatus.valueOf("SUCCESS"));
        assertEquals("REMOTE", ProcessingLocation.REMOTE.name());
    }
}
