package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpResultRecordsTest {
    @TempDir
    Path temp;

    @Test
    void mobileResultRecordsNormalizeNullsAndCopyLists() {
        List<String> warnings = new ArrayList<>(List.of("one"));
        McpCodeBlock block = new McpCodeBlock(
                " id ", " title ", null, " ", warnings, " code ", " placement ",
                true, warnings, warnings);

        McpMobileAccessibilityTree tree = new McpMobileAccessibilityTree(" WEBVIEW ", null, 5, false, warnings);
        McpMobileActionResult action = new McpMobileActionResult(" tap ", true, block, warnings);
        McpMobileContextSnapshot context = new McpMobileContextSnapshot(" NATIVE_APP ", warnings, null, 0,
                false, warnings);
        McpMobileSessionResult session = new McpMobileSessionResult(" web ", null, " device ", null,
                true, List.of(block), warnings);

        warnings.add("two");

        assertEquals("id", block.id());
        assertEquals(McpCodeBlock.Kind.INVESTIGATION, block.kind());
        assertEquals("java", block.language());
        assertEquals("WEBVIEW", tree.currentContext());
        assertEquals("", tree.source());
        assertEquals("tap", action.action());
        assertEquals("NATIVE_APP", context.currentContext());
        assertEquals("", context.pageSource());
        assertEquals("web", session.mode());
        assertEquals("", session.platformName());
        assertEquals("", session.browserName());
        assertEquals(List.of("one"), session.warnings());
        assertThrows(UnsupportedOperationException.class, () -> session.warnings().add("three"));
    }

    @Test
    void browserAndNaturalResultsKeepBoundedValues() {
        McpNaturalActionResult natural = new McpNaturalActionResult(-1, -2, 120, " pilot ", false, " web ");
        McpPageDomSnapshot dom = new McpPageDomSnapshot("url", "title", "<html/>", 7, false, List.of());
        McpScreenshotResult screenshot = new McpScreenshotResult("image/png", 4, "abcd", "out.png", List.of());

        assertEquals(0, natural.intentLength());
        assertEquals(0, natural.argumentCount());
        assertEquals(100, natural.minimumTrustPercentage());
        assertEquals("pilot", natural.planner());
        assertEquals("web", natural.allowedActions());
        assertEquals("<html/>", dom.dom());
        assertEquals("image/png", screenshot.mediaType());
    }

    @Test
    void captureReplayResultCopiesLists() {
        List<String> warnings = new ArrayList<>(List.of("warn"));
        McpCaptureReplayResult result = new McpCaptureReplayResult(
                temp.resolve("GeneratedTest.java"),
                temp.resolve("test-data.json"),
                temp.resolve("report.json"),
                temp.resolve("review.json"),
                true,
                null,
                null,
                warnings);

        warnings.add("later");

        assertTrue(result.successful());
        assertEquals(List.of(), result.codeBlocks());
        assertEquals(List.of("warn"), result.warnings());
        assertThrows(UnsupportedOperationException.class, () -> result.warnings().add("x"));
    }

    @Test
    void workspacePolicyResolvesOutputsListsAndSourceAllowlist() throws Exception {
        Path root = Files.createDirectories(temp.resolve("workspace"));
        Path repository = Files.createDirectories(root.resolve("repo"));
        Path existing = Files.writeString(repository.resolve("build.log"), "ok");
        McpWorkspacePolicy policy = McpWorkspacePolicy.of(root);

        assertEquals(existing.toRealPath(), policy.existing("repo/build.log", "Evidence"));
        assertEquals(root.resolve("repo/out/report.json").toAbsolutePath().normalize(),
                policy.output("repo/out/report.json", "Report").toAbsolutePath().normalize());
        assertEquals(List.of(existing.toRealPath()), policy.existingList(List.of("repo/build.log"), "Evidence"));
        assertEquals(List.of("src/test/AppTest.java"),
                policy.sourceAllowlist(repository, List.of("src\\test\\AppTest.java")));
        assertThrows(IllegalArgumentException.class, () -> policy.existing(null, "Evidence"));
        assertThrows(IllegalArgumentException.class, () -> policy.output("../outside.txt", "Report"));
        assertThrows(IllegalArgumentException.class,
                () -> policy.sourceAllowlist(repository, List.of("../outside.java")));
    }

}
