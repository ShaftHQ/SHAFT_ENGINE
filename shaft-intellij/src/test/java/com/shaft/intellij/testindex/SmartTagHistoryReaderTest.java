package com.shaft.intellij.testindex;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SmartTagHistoryReaderTest {
    @TempDir
    Path temp;

    @Test
    void firstSeenFailureIsNewNotRegressedOrFlaky() throws Exception {
        Path history = write("""
                {"uuid":"1","timestamp":1,"testResults":{"h-new":{"name":"firstSeen","fullName":"demo.New.firstSeen","status":"failed"}}}
                """);
        List<String> tags = SmartTagHistoryReader.read(history).get("demo.New#firstSeen");
        assertEquals(List.of("New"), tags);
        assertFalse(tags.contains("Regressed"));
        assertFalse(tags.contains("Flaky"));
    }

    @Test
    void passedThenFailedIsRegressed() throws Exception {
        Path history = write("""
                {"uuid":"1","timestamp":1,"testResults":{"h-r":{"name":"regressed","fullName":"demo.Regressed.test","status":"passed"}}}
                {"uuid":"2","timestamp":2,"testResults":{"h-r":{"name":"regressed","fullName":"demo.Regressed.test","status":"failed"}}}
                """);
        List<String> tags = SmartTagHistoryReader.read(history).get("demo.Regressed#test");
        assertTrue(tags.contains("Regressed"));
        assertFalse(tags.contains("New"));
        assertFalse(tags.contains("Flaky"));
    }

    @Test
    void missingHistoryIsUnknownAndDoesNotInventFlaky() {
        Map<String, List<String>> tags = SmartTagHistoryReader.read(temp.resolve("missing.jsonl"));
        assertTrue(tags.isEmpty());
    }

    private Path write(String content) throws Exception {
        Path history = temp.resolve("history.jsonl");
        Files.writeString(history, content);
        return history;
    }
}
