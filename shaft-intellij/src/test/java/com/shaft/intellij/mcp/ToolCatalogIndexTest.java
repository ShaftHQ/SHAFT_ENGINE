package com.shaft.intellij.mcp;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link ToolCatalogIndex}: the lazy-holder reader for the bundled build-time copy of
 * shaft-mcp's canonical {@code META-INF/shaft-mcp/tool-index.json} (design doc Decision 4/5,
 * amendment A7), which feeds {@link com.shaft.intellij.ui.AssistantCommand}'s deterministic
 * routing (issue #3870/#3866 T4) instead of a second hand-maintained keyword source.
 *
 * <p>Asserted against the real bundled resource (not a fake), so this suite also proves the
 * build actually copies the file where the plugin classloader can find it.</p>
 */
class ToolCatalogIndexTest {

    @Test
    void toolNamesIncludesRealSurvivingToolsAndExcludesDeletedOnes() {
        Set<String> names = ToolCatalogIndex.toolNames();

        assertTrue(names.contains("capture_start"), "a real, surviving tool must be present");
        assertTrue(names.contains("browser_navigate"), "a real, surviving tool must be present");
        assertTrue(names.contains("element_click"), "a real, surviving tool must be present");
        assertTrue(names.contains("driver_initialize"), "a real, surviving tool must be present");
        assertFalse(names.contains("natural_act"), "natural_act was deleted outright (owner mandate)");
        assertFalse(names.contains("mobile_natural_act"), "mobile_natural_act was deleted outright");
        assertFalse(names.contains("playwright_browser_navigate"),
                "playwright_browser_navigate was absorbed into browser_navigate's engine dispatch");
    }

    @Test
    void toolNamesMatchesTheCanonical89ToolCatalog() {
        assertEquals(89, ToolCatalogIndex.toolNames().size(),
                "the bundled index must track the canonical 89-tool catalog (#3868); a mismatch means "
                        + "the build-time copy is stale or the resource wasn't regenerated");
    }

    @Test
    void intentKeywordsReturnsCuratedOverlayEntriesForARealTool() {
        // shaft_project_upgrade's curated overlay entries (tool-index-overlay.json) include two
        // phrases not present in AssistantCommand's hand-maintained INTENT_KEYWORDS list -- real,
        // observable proof the index contributes real data, not just an empty pass-through.
        List<String> keywords = ToolCatalogIndex.intentKeywords("shaft_project_upgrade");

        assertTrue(keywords.contains("upgrade shaft project"), keywords.toString());
        assertTrue(keywords.contains("upgrade this shaft project"), keywords.toString());
    }

    @Test
    void intentKeywordsReturnsEmptyForAnUnknownToolNameInsteadOfThrowing() {
        assertEquals(List.of(), ToolCatalogIndex.intentKeywords("not_a_real_tool_xyz"));
    }

    @Test
    void intentKeywordsReturnsEmptyForAToolWithNoCuratedKeywordsYet() {
        // Most of the 89-tool catalog has no curated intentKeywords overlay entries yet (only 8/89
        // are curated as of #3868/#3869) -- must degrade to an empty list, never null or an exception.
        assertEquals(List.of(), ToolCatalogIndex.intentKeywords("element_click"));
    }

    /**
     * Covers issue #3883(a): {@code slashAlias}-driven {@code /mcp} autocomplete. The curated
     * subset (tool-index-overlay.json) added by this task -- a real, observable sample, not a
     * synthetic fixture.
     */
    @Test
    void slashAliasesMapsCuratedAliasesToTheirCanonicalToolNames() {
        Map<String, String> aliases = ToolCatalogIndex.slashAliases();

        assertEquals("driver_initialize", aliases.get("init"));
        assertEquals("driver_quit", aliases.get("quit"));
        assertEquals("browser_navigate", aliases.get("nav"));
        assertEquals("browser_take_screenshot", aliases.get("screenshot"));
        assertEquals("capture_start", aliases.get("start"));
        assertEquals("capture_stop", aliases.get("stop"));
        assertEquals("element_click", aliases.get("click"));
        assertEquals("element_type", aliases.get("type"));
        assertEquals("generate_test_report", aliases.get("report"));
        assertEquals("healer_run_failed_test", aliases.get("heal"));
        assertEquals("test_plan_explore", aliases.get("explore"));
        assertEquals(15, aliases.size(), aliases.toString());
    }

    @Test
    void slashAliasesOmitsToolsWithNoCuratedAliasYet() {
        // element_hover has no curated slashAlias overlay entry -- its tool name must never appear
        // as a slashAliases() value.
        assertFalse(ToolCatalogIndex.slashAliases().containsValue("element_hover"));
    }

    @Test
    void toolNameForAliasReturnsNullForAnUnknownAlias() {
        assertNull(ToolCatalogIndex.slashAliases().get("not_a_real_alias_xyz"));
    }
}
