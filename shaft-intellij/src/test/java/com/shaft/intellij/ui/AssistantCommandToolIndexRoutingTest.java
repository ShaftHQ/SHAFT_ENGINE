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

    @Test
    void keywordsForDropsOverlayEntryThatOnlyDiffersFromABuiltinEntryByTrailingWhitespace() {
        // PR #3882 review finding: shaft_coding_partner_plan's built-in list carries "find reuse
        // for " with a deliberate trailing space (a word-boundary marker -- see
        // matchesWholeWordPrefix's javadoc); the bundled overlay (tool-index.json) carries the same
        // five phrases WITHOUT the trailing space. A plain LinkedHashSet string-dedup lets both the
        // boundary-safe builtin and the boundary-less overlay copy survive, and startsWithAny's raw
        // String.startsWith then matches the boundary-less copy against unrelated text (proven false
        // positive: "find reuse format issues in this class"). The merge must dedupe on the trimmed
        // value, keeping only the builtin's boundary-safe variant.
        List<String> builtin = List.of(
                "plan coding partner work for ",
                "plan partner work for ",
                "coding partner plan for ",
                "partner plan for ",
                "find reuse for ");

        List<String> merged = AssistantCommand.keywordsFor("shaft_coding_partner_plan", builtin);

        assertTrue(merged.containsAll(builtin), merged.toString());
        assertFalse(merged.contains("find reuse for"),
                "boundary-less overlay duplicate of 'find reuse for ' must be dropped: " + merged);
        assertEquals(builtin.size(), merged.size(),
                "every overlay entry here is a trim-duplicate of a builtin phrase and must not survive: " + merged);
    }

    @Test
    void keywordsForStillAddsAGenuinelyNewOverlayEntryAlongsideATrimDuplicate() {
        // The trimmed dedup must not become subtractive: a real overlay phrase that is NOT a
        // trim-duplicate of any builtin entry (shaft_project_upgrade's "upgrade shaft project") is
        // still additive, same as before this fix.
        List<String> builtin = List.of("preview shaft upgrade", "preview upgrade", "dry run shaft upgrade");

        List<String> merged = AssistantCommand.keywordsFor("shaft_project_upgrade", builtin);

        assertTrue(merged.containsAll(builtin), merged.toString());
        assertTrue(merged.contains("upgrade shaft project"), merged.toString());
        assertTrue(merged.contains("upgrade this shaft project"), merged.toString());
        assertEquals(5, merged.size(), "must not duplicate any entry: " + merged);
    }

    // ---- Proven false-positive routing (PR #3882 review) ----

    @Test
    void findReuseFormatIssuesDoesNotMisrouteToCodingPartnerPlan() {
        assertFalse("shaft_coding_partner_plan".equals(command("find reuse format issues in this class").toolName()),
                "must not misroute on the boundary-less overlay duplicate of 'find reuse for '");
    }

    @Test
    void findReuseForgeDoesNotMisrouteToCodingPartnerPlan() {
        assertFalse("shaft_coding_partner_plan".equals(command("find reuse forge").toolName()),
                "must not misroute on the boundary-less overlay duplicate of 'find reuse for '");
    }

    @Test
    void guardrailsChecklistDoesNotMisrouteToGuardrailsCheck() {
        assertFalse("test_code_guardrails_check".equals(command("guardrails checklist for this PR").toolName()),
                "must not misroute on the boundary-less overlay duplicate of 'guardrails check '");
    }

    @Test
    void recordPrefixedPromptWithEmbeddedStartRecordingPhraseDoesNotMisrouteToCaptureStart() {
        // PR #3882 review finding: capture_start's overlay adds "start recording"/"start a
        // recording"/"start recorder"/"start capture" -- phrases already handled deterministically
        // and exactly by isStartRecording/isRecordScenarioIntent as standalone commands. Merging them
        // into isBrowserRecordingIntent's containsAny check (reached whenever the *whole* prompt
        // starts with "record ") would let a "record ..." prompt that merely *contains* one of those
        // phrases later newly match capture_start, which it did not pre-PR (no builtin capture_start
        // keyword -- "browser", "web flow", "webdriver", etc. -- is present in this prompt).
        assertFalse("capture_start".equals(command("record my start recording of the checkout demo").toolName()),
                "must route the same as pre-PR: no builtin capture_start keyword is present");
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

    // Named exception allowlist for any future, deliberately-accepted exclusion from 100% explicit
    // -tool-mention routing (PR #3882 review, Finding 2). Empty today: the full 89-tool catalog
    // routes deterministically with no exceptions. Add a tool name here only with a comment
    // explaining why it cannot route deterministically -- never to silence a real regression.
    private static final Set<String> TOOL_MENTION_ROUTING_EXCEPTIONS = Set.of();

    @Test
    void everyToolInTheCatalogRoutesDeterministicallyViaAnExplicitToolMention() {
        Set<String> toolNames = ToolCatalogIndex.toolNames();
        assertFalse(toolNames.isEmpty(), "the bundled tool-index resource must be present for this suite to mean anything");

        List<String> misrouted = new ArrayList<>();
        int checked = 0;
        for (String toolName : toolNames) {
            if (TOOL_MENTION_ROUTING_EXCEPTIONS.contains(toolName)) {
                continue;
            }
            checked++;
            String routed = command("call " + toolName).toolName();
            if (!toolName.equals(routed)) {
                misrouted.add(toolName + " -> " + (routed.isBlank() ? "(local response)" : routed));
            }
        }

        // Printed unconditionally (not just on failure) so CI logs and local runs both carry the
        // measured count as evidence, not just a pass/fail bit -- design doc Decision 5's
        // routing-accuracy suite is meant to be inspectable, not just green.
        System.out.println("[routing-accuracy] " + (checked - misrouted.size()) + "/" + checked
                + " tools (100% required) routed deterministically via an explicit tool mention; "
                + "allowlisted exceptions: " + TOOL_MENTION_ROUTING_EXCEPTIONS + "; misrouted: " + misrouted);
        assertTrue(misrouted.isEmpty(),
                "expected 100% deterministic routing across the " + checked + "-tool catalog (allowlisted "
                        + "exceptions: " + TOOL_MENTION_ROUTING_EXCEPTIONS + "), got " + misrouted.size()
                        + " misrouted: " + misrouted);
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
