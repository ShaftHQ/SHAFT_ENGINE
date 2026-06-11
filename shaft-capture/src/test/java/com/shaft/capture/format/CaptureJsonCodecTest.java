package com.shaft.capture.format;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureJsonCodecTest {
    private final CaptureJsonCodec codec = new CaptureJsonCodec();

    @Test
    void representativeJourneyRoundTripsEveryEventTypeWithoutSemanticLoss() {
        CaptureSession original = CaptureFixtures.representativeSession();

        String first = codec.write(original);
        CaptureSession restored = codec.read(first);
        String second = codec.write(restored);

        assertEquals(original, restored);
        assertEquals(first, second);
        assertEquals(14, restored.events().size());
        assertInstanceOf(CaptureEvent.NavigationEvent.class, restored.events().getFirst());
        assertInstanceOf(CaptureEvent.VerificationEvent.class, restored.events().getLast());
        assertTrue(first.contains("\"toggle\""));
        assertTrue(first.contains("\"checked\" : false"));
    }

    @Test
    void canonicalGoldenFileRemainsStable() throws IOException {
        String golden = resource("golden-session-1.0.json");

        CaptureSession session = codec.read(golden);

        assertEquals(golden, codec.write(session));
    }

    @Test
    void syntheticLegacyFixtureMigratesToCurrentSchema() throws IOException {
        CaptureSession migrated = codec.read(resource("legacy-session-0.9.json"));

        assertEquals(CaptureSession.CURRENT_SCHEMA_VERSION, migrated.schemaVersion());
        assertEquals(CaptureSession.SessionStatus.INCOMPLETE, migrated.status());
        assertEquals(1, migrated.events().size());
        assertInstanceOf(CaptureEvent.NavigationEvent.class, migrated.events().getFirst());
        assertEquals("NOT_REPLAYED", migrated.events().getFirst().context().replayStatus().name());
    }

    @Test
    void unknownEventTypeFailsSchemaValidationWithActionableMessage() {
        String invalid = codec.write(CaptureFixtures.representativeSession())
                .replaceFirst("\"navigation\"", "\"unsupported\"");

        CaptureFormatException exception = assertThrows(CaptureFormatException.class, () -> codec.read(invalid));

        assertTrue(exception.getMessage().contains("schema validation"));
        assertTrue(exception.getMessage().contains("enum"));
    }

    @Test
    void malformedAndTruncatedRecordingsFailWithoutEchoingContents() {
        String canary = "DO-NOT-ECHO-CANARY";
        List<String> malformed = List.of(
                "",
                "{",
                "{\"schemaVersion\":\"1.0\",\"secret\":\"" + canary + "\"",
                "[]",
                "{\"schemaVersion\":\"2.0\"}");

        for (String value : malformed) {
            CaptureFormatException exception = assertThrows(CaptureFormatException.class, () -> codec.read(value));
            assertFalse(exception.getMessage().contains(canary));
        }

        String valid = codec.write(CaptureFixtures.representativeSession());
        for (int length = 1; length < valid.length(); length += Math.max(1, valid.length() / 17)) {
            String truncated = valid.substring(0, length);
            assertThrows(CaptureFormatException.class, () -> codec.read(truncated));
        }
    }

    @Test
    void invalidVersionCannotPartiallyOverwriteExistingRecording(@TempDir Path temp) throws IOException {
        Path destination = temp.resolve("session.json");
        Files.writeString(destination, "sentinel", StandardCharsets.UTF_8);
        CaptureSession invalid = new CaptureSession(
                "2.0",
                "future",
                CaptureSession.SessionStatus.INCOMPLETE,
                CaptureFixtures.STARTED,
                null,
                CaptureFixtures.browser(),
                List.of(),
                List.of(),
                List.of(),
                null,
                java.util.Map.of());

        CaptureFormatException exception = assertThrows(CaptureFormatException.class,
                () -> codec.write(destination, invalid));

        assertTrue(exception.getMessage().contains("current capture schema version"));
        assertEquals("sentinel", Files.readString(destination, StandardCharsets.UTF_8));
    }

    private static String resource(String name) throws IOException {
        try (var input = CaptureJsonCodecTest.class.getResourceAsStream("/fixtures/" + name)) {
            if (input == null) {
                throw new IOException("Missing fixture: " + name);
            }
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        }
    }
}
