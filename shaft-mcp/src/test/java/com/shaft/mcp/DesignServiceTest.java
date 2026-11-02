package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesignServiceTest {
    @Test
    void pasteYieldsNumberedCriteriaWithoutWritingFiles() {
        DesignService service = new DesignService(McpWorkspacePolicy.of(Path.of(".")));
        String story = """
                As a shopper
                I want to check out
                so that I can buy items
                - Cart updates in real time
                - User can change quantities
                - User can remove items
                """;

        McpDesignPack pack = service.ingest(story, "", "");

        assertEquals("ok", pack.status());
        assertEquals("shopper", pack.actor());
        assertTrue(pack.outcome().contains("check out"));
        assertEquals(List.of("AC-01", "AC-02", "AC-03"),
                pack.acceptanceCriteria().stream().map(McpDesignAcceptanceCriterion::id).toList());
        assertFalse(pack.wroteFiles());
    }

    @Test
    void emptyPasteFailsClosed() {
        DesignService service = new DesignService(McpWorkspacePolicy.of(Path.of(".")));
        McpDesignPack pack = service.ingest("  ", "", "");
        assertEquals("error", pack.status());
        assertTrue(pack.acceptanceCriteria().isEmpty());
        assertFalse(pack.wroteFiles());
    }

    @Test
    void urlWithoutTextDoesNotFetch() {
        DesignService service = new DesignService(McpWorkspacePolicy.of(Path.of(".")));
        McpDesignPack pack = service.ingest("", "", "https://example.com/story");
        assertEquals("error", pack.status());
        assertEquals("url", pack.sourceKind());
        assertTrue(pack.message().toLowerCase().contains("does not fetch"));
    }

    @Test
    void secretsAndHomePathsAreRedacted(@TempDir Path temp) throws Exception {
        Path file = temp.resolve("story.md");
        Files.writeString(file, """
                As a shopper
                - password=hunter2
                - token: abc
                - notes in /home/someone/secret.txt
                """, StandardCharsets.UTF_8);
        DesignService service = new DesignService(McpWorkspacePolicy.of(temp));

        McpDesignPack pack = service.ingest("", "story.md", "");

        assertEquals("ok", pack.status());
        String joined = pack.acceptanceCriteria().stream()
                .map(McpDesignAcceptanceCriterion::text)
                .reduce("", (left, right) -> left + " " + right);
        assertFalse(joined.contains("hunter2"));
        assertFalse(joined.contains("abc"));
        assertFalse(joined.contains("/home/someone"));
        assertTrue(joined.contains("[redacted]"));
        assertTrue(joined.contains("[redacted-path]"));
        assertFalse(pack.wroteFiles());
    }
}
