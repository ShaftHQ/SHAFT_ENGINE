package com.shaft.intellij.ui;

import com.shaft.intellij.mcp.ToolCatalogIndex;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers issue #3870/#3866 T4: {@link AssistantCommand}'s routing wired to the canonical
 * {@link ToolCatalogIndex} instead of a second hand-maintained keyword table (design doc
 * Decision 5, amendment A7), plus the deterministic-first "explicit tool mention" routing this
 * task adds so every one of the 89 surviving tools -- not just the handful with a bespoke
 * natural-language recognizer -- is directly addressable without falling back to the local-AI
 * agent (design doc Decision 5 / goal 3-4).
 */
class AssistantCommandToolIndexRoutingTest {

    // ---- keywordsFor: index-fed augmentation of the built-in fallback lists ----

    @Test
    void keywordsForAugmentsBuiltinListWithRealCuratedIndexEntries() {
        // shaft_project_upgrade's curated overlay (tool-index-overlay.json) carries two phrases
        // that AssistantCommand's own built-in list does not -- real data, not a synthetic fixture.
        List<String> builtin = List.of("preview shaft upgrade", "preview upgrade", "dry run shaft upgrade");

        List<String> merged = AssistantCommand.keywordsFor("shaft_project_upgrade", builtin);

        assertTrue(merged.containsAll(builtin), merged.toString());
        assertTrue(merged.contains("upgrade shaft project"), merged.toString());
        assertTrue(merged.contains("upgrade this shaft project"), merged.toString());
        assertEquals(5, merged.size(), "must not duplicate any entry: " + merged);
    }

    @Test
    void keywordsForDeduplicatesWhenIndexRepeatsABuiltinEntry() {
        // mobile_toolchain_status's curated overlay entries ("toolchain", "appium", "adb", "sdk")
        // are all already present in AssistantCommand's much larger built-in list -- the merge must
        // not double them up.
        List<String> builtin = List.of("toolchain", "appium", "adb", "sdk", "emulator");

        List<String> merged = AssistantCommand.keywordsFor("mobile_toolchain_status", builtin);

        assertEquals(builtin.size(), merged.size(), merged.toString());
        assertEquals(builtin, merged);
    }

    @Test
    void keywordsForReturnsBuiltinUnchangedWhenTheKeyIsNotARealIndexedTool() {
        // "mobile_record_start" is an internal INTENT_KEYWORDS lookup key, not a live @Tool name
        // (it was absorbed into capture_start by the W1 tool sweep) -- the index has nothing under
        // that name, so the merge must be a no-op, never throw.
        List<String> builtin = List.of("mobile", "android", "emulator");

        assertEquals(builtin, AssistantCommand.keywordsFor("mobile_record_start", builtin));
    }

    // ---- Deterministic-first: an explicit tool-name mention resolves directly ----

    @Test
    void explicitToolMentionResolvesDeterministicallyForElementFamily() {
        assertEquals("element_click", command("call element_click").toolName());
        AssistantCommand.Invocation typed = command("element_type {\"text\": \"hello\"}");
        assertEquals("element_type", typed.toolName());
        assertEquals("hello", typed.arguments().get("text").getAsString());
    }

    @Test
    void explicitToolMentionResolvesDeterministicallyForBrowserFamily() {
        // No URL/verb context at all -- isBrowserControlIntent alone could never resolve this, so a
        // pass here is real, new coverage, not an accidental double route to an existing recognizer.
        assertEquals("browser_get_title", command("call browser_get_title").toolName());
    }

    @Test
    void explicitToolMentionResolvesDeterministicallyForMobileFamily() {
        assertEquals("mobile_swipe", command("run mobile_swipe").toolName());
        assertEquals("mobile_toolchain_status", command("invoke mobile_toolchain_status").toolName());
    }

    @Test
    void explicitToolMentionResolvesDeterministicallyForCaptureAndCodegenFamily() {
        assertEquals("capture_generate_replay", command("call capture_generate_replay").toolName());
        assertEquals("capture_pick_locator", command("use capture_pick_locator").toolName());
    }

    @Test
    void explicitToolMentionWinsEvenWhenTheToolNameCoincidentallyContainsACodegenTriggerWord() {
        // capture_codegen_features's name literally contains the substring "codegen", which is one
        // of isCodeGenerationRequest's trigger words -- that earlier, higher-precedence gate in
        // fromPrompt used to swallow the whole prompt into the free-form code-generation/local-agent
        // path before directIntent (and this task's new explicit-tool-mention check inside it) was
        // ever reached. An explicit tool-name mention must win regardless of what substrings happen
        // to appear inside the tool's own name.
        assertEquals("capture_codegen_features", command("call capture_codegen_features").toolName());
    }

    @Test
    void explicitToolMentionResolvesDeterministicallyForDoctorAndHealerFamily() {
        assertEquals("healer_run_failed_test", command("call healer_run_failed_test").toolName());
        assertEquals("doctor_propose_healed_locator", command("call doctor_propose_healed_locator").toolName());
    }

    @Test
    void explicitToolMentionResolvesDeterministicallyForProjectAndGuideFamily() {
        assertEquals("shaft_project_init_agents", command("call shaft_project_init_agents").toolName());
        assertEquals("test_plan_explore", command("call test_plan_explore").toolName());
    }

    @Test
    void explicitToolMentionNeverFallsBackToTheLocalAgent() {
        AssistantCommand.Invocation invocation = command("call element_click");
        assertFalse("autobot_local_agent_run".equals(invocation.toolName())
                || "autobot_provider_chat".equals(invocation.toolName()));
    }

    // ---- Routing-accuracy suite (design doc Decision 5 last bullet): parameterized from the real
    // bundled index, not a hand-picked sample, across every surviving tool in the 89-tool catalog. ----

    @Test
    void atLeast80PercentOfTheCatalogRoutesDeterministicallyViaAnExplicitToolMention() {
        Set<String> toolNames = ToolCatalogIndex.toolNames();
        assertFalse(toolNames.isEmpty(), "the bundled tool-index resource must be present for this suite to mean anything");

        List<String> misrouted = new ArrayList<>();
        for (String toolName : toolNames) {
            String routed = command("call " + toolName).toolName();
            if (!toolName.equals(routed)) {
                misrouted.add(toolName + " -> " + (routed.isBlank() ? "(local response)" : routed));
            }
        }

        double passRate = 1.0 - ((double) misrouted.size() / toolNames.size());
        // Printed unconditionally (not just on failure) so CI logs and local runs both carry the
        // measured percentage as evidence, not just a pass/fail bit -- design doc Decision 5's
        // routing-accuracy suite is meant to be inspectable, not just green.
        System.out.println("[routing-accuracy] " + (toolNames.size() - misrouted.size()) + "/" + toolNames.size()
                + " tools (" + Math.round(passRate * 100) + "%) routed deterministically via an explicit "
                + "tool mention; misrouted: " + misrouted);
        assertTrue(passRate >= 0.80,
                "expected >=80% deterministic routing across the " + toolNames.size() + "-tool catalog, got "
                        + Math.round(passRate * 100) + "%; misrouted: " + misrouted);
    }

    // ---- /codegen regression (design doc's explicit ask): must still resolve correctly ----

    @Test
    void codegenSlashCommandStillResolvesCorrectlyAfterIndexWiring() {
        assertEquals("capture_generate_replay",
                command("/codegen recordings/capture-session.json").toolName(),
                "an explicit recording path must still deterministically drive the replay generator");
        AssistantCommand.Invocation liveCodegen =
                command("/codegen navigate to https://example.com and click login");
        assertFalse(liveCodegen.toolName().equals("element_click"),
                "a free-text /codegen flow description must not be hijacked by the new explicit-tool-mention "
                        + "route just because it happens to describe an element action");
    }

    private static AssistantCommand.Invocation command(String prompt) {
        return AssistantCommand.fromPrompt(prompt, "CODEX", "ASK", ".", "", false);
    }
}
