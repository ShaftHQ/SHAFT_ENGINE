package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers issue #3895: a live-vs-bundled tool-catalog diff surfaced on explicit "Refresh tools",
 * deferred from #3883(b). {@link ToolCatalogDiff#diff(Set, Set)} is the pure, UI-free seam:
 * {@code Optional.empty()} when the live {@code tools/list} response and the bundled
 * {@link ToolCatalogIndex#toolNames()} agree, else exactly one deterministic line naming the
 * difference so a user hitting "Refresh tools" learns the bundled index is stale without a scary
 * false positive on every routine refresh.
 */
class ToolCatalogDiffTest {

    @Test
    void returnsEmptyWhenLiveAndBundledSetsMatchExactly() {
        Set<String> live = Set.of("browser_navigate", "element_click");
        Set<String> bundled = Set.of("browser_navigate", "element_click");

        assertEquals(Optional.empty(), ToolCatalogDiff.diff(live, bundled));
    }

    @Test
    void reportsToolsPresentLiveButNotInTheBundledIndex() {
        Set<String> live = new LinkedHashSet<>(Set.of("browser_navigate", "element_click", "brand_new_tool"));
        Set<String> bundled = Set.of("browser_navigate", "element_click");

        Optional<String> result = ToolCatalogDiff.diff(live, bundled);

        assertTrue(result.isPresent());
        assertEquals("Live MCP catalog differs from bundled index: +1 not in bundle (brand_new_tool)",
                result.get());
    }

    @Test
    void reportsToolsInTheBundledIndexButMissingFromLive() {
        Set<String> live = Set.of("browser_navigate");
        Set<String> bundled = new LinkedHashSet<>(Set.of("browser_navigate", "element_click"));

        Optional<String> result = ToolCatalogDiff.diff(live, bundled);

        assertTrue(result.isPresent());
        assertEquals("Live MCP catalog differs from bundled index: -1 missing live (element_click)",
                result.get());
    }

    @Test
    void reportsBothSidesTogetherWithExtraFirstThenMissing() {
        Set<String> live = Set.of("browser_navigate", "brand_new_tool");
        Set<String> bundled = Set.of("browser_navigate", "element_click");

        Optional<String> result = ToolCatalogDiff.diff(live, bundled);

        assertEquals(
                "Live MCP catalog differs from bundled index: +1 not in bundle (brand_new_tool); "
                        + "-1 missing live (element_click)",
                result.get());
    }

    @Test
    void ordersNamesDeterministicallyRegardlessOfInputSetIteration() {
        Set<String> live = new LinkedHashSet<>(Set.of("zeta_tool", "alpha_tool", "common_tool"));
        Set<String> bundled = Set.of("common_tool");

        Optional<String> result = ToolCatalogDiff.diff(live, bundled);

        assertEquals("Live MCP catalog differs from bundled index: +2 not in bundle (alpha_tool, zeta_tool)",
                result.get());
    }

    @Test
    void capsNamesShownAtFivePerSideWithAnEllipsis() {
        Set<String> live = new LinkedHashSet<>(Set.of(
                "common", "tool_a", "tool_b", "tool_c", "tool_d", "tool_e", "tool_f"));
        Set<String> bundled = Set.of("common");

        Optional<String> result = ToolCatalogDiff.diff(live, bundled);

        assertEquals(
                "Live MCP catalog differs from bundled index: +6 not in bundle "
                        + "(tool_a, tool_b, tool_c, tool_d, tool_e, …)",
                result.get());
    }

    @Test
    void returnsEmptyWhenTheLiveSetIsEmptyInsteadOfReportingTheWholeBundledCatalogAsMissing() {
        // An empty live set means the tools/list response failed to parse -- must never produce a
        // scary false diff claiming every bundled tool is "missing live".
        Set<String> bundled = Set.of("browser_navigate", "element_click");

        assertEquals(Optional.empty(), ToolCatalogDiff.diff(Set.of(), bundled));
        assertEquals(Optional.empty(), ToolCatalogDiff.diff(null, bundled));
    }

    @Test
    void returnsEmptyWhenTheBundledSetIsEmptyInsteadOfReportingEveryLiveToolAsUnbundled() {
        // Symmetric guard: an empty bundled set means the bundled resource is missing/unreadable
        // (ToolCatalogIndex degrades to empty rather than throwing) -- not real evidence every live
        // tool is new.
        Set<String> live = Set.of("browser_navigate", "element_click");

        assertEquals(Optional.empty(), ToolCatalogDiff.diff(live, Set.of()));
        assertEquals(Optional.empty(), ToolCatalogDiff.diff(live, null));
    }
}
