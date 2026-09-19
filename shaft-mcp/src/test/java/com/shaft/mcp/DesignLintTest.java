package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesignLintTest {
    private final DesignService service = new DesignService(McpWorkspacePolicy.of(Path.of(".")));

    @Test
    void clickXpathWhenIsErrorAndBlocksAccept() throws Exception {
        String gherkin = Files.readString(Path.of("src/test/resources/fixtures/design/click-xpath.feature"));
        McpDesignLint lint = service.lint("", "", "", "", gherkin, "");
        assertEquals(McpDesignLint.STATUS_BLOCKED, lint.status(), lint.message());
        assertTrue(lint.acceptBlocked());
        assertTrue(lint.findings().stream().anyMatch(finding -> "click_xpath".equals(finding.rule())
                && "error".equals(finding.level())), lint.findings().toString());
        assertFalse(lint.wroteFiles());
    }

    @Test
    void declarativeCheckoutIsClean() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/complete-checkout.txt"));
        McpDesignLint lint = service.lint(story, "", "", "", "", "");
        assertEquals(McpDesignLint.STATUS_OK, lint.status(), lint.message());
        assertFalse(lint.acceptBlocked());
        assertTrue(lint.findings().isEmpty(), lint.findings().toString());
        assertFalse(lint.wroteFiles());
    }

    @Test
    void waivingErrorUnblocksAccept() throws Exception {
        String gherkin = Files.readString(Path.of("src/test/resources/fixtures/design/click-xpath.feature"));
        McpDesignLint first = service.lint("", "", "", "", gherkin, "");
        String id = first.findings().get(0).id();
        McpDesignLint waived = service.lint("", "", "", "", gherkin, id + ":accepted residual click");
        assertFalse(waived.acceptBlocked(), waived.message());
        assertEquals(McpDesignLint.STATUS_OK, waived.status());
        assertFalse(waived.wroteFiles());
    }
}
