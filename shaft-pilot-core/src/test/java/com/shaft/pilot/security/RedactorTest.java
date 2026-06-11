package com.shaft.pilot.security;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceCategory;
import com.shaft.pilot.ai.EvidenceReference;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RedactorTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test
    void removesHeadersSecretsPasswordFieldsAndConfiguredDomContent() {
        String secret = "do-not-log-this";
        AiRequest request = AiRequest.builder("redaction-test", JSON.createObjectNode())
                .text("Authorization: Bearer " + secret + "\nCookie: session=" + secret
                        + "\napi_key=" + secret + "\n"
                        + "<input type='password' value='" + secret + "'>"
                        + "<div data-private='" + secret + "'>" + secret + "</div>")
                .evidence(new EvidenceReference("log", EvidenceCategory.LOG, "text/plain",
                        "password=" + secret))
                .approvalPolicy(ApprovalPolicy.denyAll())
                .build();
        RedactionPolicy policy = new RedactionPolicy(
                Set.of("value", "data-private"),
                List.of("input[type=password]", "div[data-private]"),
                List.of());

        RedactionResult result = Redactor.redact(request, policy);
        String combined = result.redactedText() + result.redactedEvidence().getFirst().content();

        assertFalse(combined.contains(secret));
        assertTrue(combined.contains(Redactor.REPLACEMENT));
        assertTrue(result.appliedRules().contains("dom-selector"));
        assertTrue(result.removedFields().contains("data-private"));
    }

    @Test
    void ignoresMalformedCustomPatternsWithoutLeakingBuiltInSecrets() {
        String secret = "do-not-log-this";
        AiRequest request = AiRequest.builder("redaction-test", JSON.createObjectNode())
                .text("password=" + secret)
                .approvalPolicy(ApprovalPolicy.denyAll())
                .build();
        RedactionPolicy policy = new RedactionPolicy(Set.of(), List.of(), List.of("["));

        RedactionResult result = Redactor.redact(request, policy);

        assertFalse(result.redactedText().contains(secret));
        assertTrue(result.appliedRules().contains("invalid-custom-pattern"));
        assertTrue(result.appliedRules().contains("secret-assignment"));
    }
}
