package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesignLexiconTest {
    @Test
    void emptyCatalogAllowsNewSteps(@TempDir Path temp) {
        DesignService service = new DesignService(McpWorkspacePolicy.of(temp));
        McpDesignLexicon lexicon = service.lexicon("suggest", "log in", "");
        assertEquals("ok", lexicon.status());
        assertTrue(lexicon.suggestions().isEmpty());
        assertFalse(lexicon.wroteFiles());
    }

    @Test
    void acceptedLoginPhraseIsSuggestedForLogInQuery(@TempDir Path temp) throws Exception {
        DesignService service = new DesignService(McpWorkspacePolicy.of(temp));
        McpDesignLexicon accepted = service.lexicon("accept", "", "the shopper is authenticated");
        assertTrue(accepted.wroteFiles());
        Path catalog = temp.resolve(DesignLexiconStore.RELATIVE);
        assertTrue(java.nio.file.Files.isRegularFile(catalog));
        String raw = java.nio.file.Files.readString(catalog);
        assertTrue(raw.strip().startsWith("{"), raw);
        assertTrue(raw.contains("\"phrases\""), raw);

        McpDesignLexicon suggested = service.lexicon("suggest", "log in", "");
        assertTrue(suggested.suggestions().contains("the shopper is authenticated"), suggested.suggestions().toString());
        assertFalse(suggested.wroteFiles());
    }

    @Test
    void locatorPhrasesAreRejected(@TempDir Path temp) {
        DesignService service = new DesignService(McpWorkspacePolicy.of(temp));
        McpDesignLexicon lexicon = service.lexicon("accept", "", "click xpath=//button");
        assertEquals("error", lexicon.status());
        assertFalse(lexicon.wroteFiles());
    }
}
