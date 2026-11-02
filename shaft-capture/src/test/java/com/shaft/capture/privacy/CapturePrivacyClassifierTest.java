package com.shaft.capture.privacy;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.storage.ExternalTestDataWriter;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CapturePrivacyClassifierTest {
    private static final String CANARY = "CANARY-ALPHA";

    @Test
    void passwordAndConfiguredSecretsNeverReachSessionJson() {
        CapturePrivacyClassifier classifier = classifier();
        ClassifiedValue secret = classifier.classifyValue(
                "password", CANARY, "input[type=password]", Map.of("autocomplete", "current-password"));
        ElementSnapshot target = CaptureFixtures.target();
        CaptureSession session = CaptureSession.start("secret-session", CaptureFixtures.STARTED,
                        CaptureFixtures.browser())
                .withDataReferences(List.of(secret.reference()), secret.summary())
                .append(new CaptureEvent.TypeEvent(CaptureFixtures.context(1), target, secret.reference()));

        String json = new CaptureJsonCodec().write(session);

        assertNull(secret.externalizedValue());
        assertFalse(json.contains(CANARY));
        assertTrue(json.contains("\"classification\" : \"SECRET\""));
        assertTrue(json.contains("\"logicalName\" : \"password\""));
    }

    @Test
    void passwordAttributeValuesAreSecretEvenWithGenericFieldNames() {
        CapturePrivacyClassifier classifier = classifier();

        ClassifiedValue secret = classifier.classifyValue(
                "field", CANARY, "#checkout-field", Map.of("type", "password"));

        assertNull(secret.externalizedValue());
        assertTrue(secret.summary().appliedRules().contains("sensitive-attribute-value"));
    }

    @Test
    void urlAttributesAndUploadFilenameAreSanitizedBeforePersistence() {
        CapturePrivacyClassifier classifier = classifier();

        var url = classifier.sanitizeUrl(
                "https://example.test/path?token=" + CANARY + "&view=summary");
        var attributes = classifier.sanitizeAttributes(
                Map.of("data-token", CANARY, "title", CANARY, "role", "button"));
        var upload = classifier.classifyUpload("evidence", "C:\\private\\" + CANARY + ".txt",
                "text/plain", 12);

        assertFalse(url.value().contains(CANARY));
        assertTrue(url.value().contains("data%3Atoken"));
        assertFalse(attributes.attributes().toString().contains(CANARY));
        assertFalse(upload.safeFileName().contains(CANARY));
        assertFalse(classifier.sanitizeArtifactFilename("evidence-" + CANARY + ".png").contains(CANARY));
        assertFalse(upload.reference().relativePath().contains("C:"));
        assertEquals(1, attributes.summary().removedAttributeCount());
    }

    @Test
    void ordinaryValuesAreExternalizedButSecretValuesAreExcluded(@TempDir Path temp) throws IOException {
        CapturePrivacyClassifier classifier = classifier();
        ClassifiedValue ordinary = classifier.classifyValue(
                "username", "alice", "#username", Map.of("name", "username"));
        ClassifiedValue secret = classifier.classifyValue(
                "api-token", CANARY, "#token", Map.of());
        Path destination = temp.resolve("capture-data.json");

        new ExternalTestDataWriter().write(destination, List.of(secret, ordinary));
        String json = Files.readString(destination);

        assertEquals("alice", ordinary.externalizedValue());
        assertTrue(json.contains("\"data.username\" : \"alice\""));
        assertTrue(json.contains("\"redactionSummary\""));
        assertTrue(json.contains("\"redactedValueCount\" : 2"));
        assertFalse(json.contains(CANARY));
        assertFalse(json.contains("data.api-token"));
    }

    private static CapturePrivacyClassifier classifier() {
        return new CapturePrivacyClassifier(new CapturePrivacyPolicy(
                Set.of("password", "api-token"),
                List.of("input[type=password]"),
                Set.of("data-token", "value"),
                Set.of("token"),
                List.of("CANARY-[A-Z]+"),
                "capture-data.json"));
    }
}
