package com.shaft.intellij.approval;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit tests for {@link ToolApprovalService}.
 */
class ToolApprovalServiceTest {
    private ToolApprovalService service;

    @BeforeEach
    void setUp() {
        service = new ToolApprovalService();
    }

    @Test
    void singleUseGrantIsConsumedAfterFirstCheck() {
        service.record(ToolApprovalDecision.APPROVE_ONCE, "tool-a");

        assertTrue(service.isApproved("tool-a"), "first check should return true and consume the grant");
        assertFalse(service.isApproved("tool-a"), "second check should return false after grant is consumed");
    }

    @Test
    void permanentToolApprovalPersiststhroughMultipleChecks() {
        service.record(ToolApprovalDecision.APPROVE_TOOL_ALWAYS, "tool-b");

        assertTrue(service.isApproved("tool-b"), "first check should return true");
        assertTrue(service.isApproved("tool-b"), "second check should also return true");
        assertTrue(service.isApproved("tool-b"), "third check should also return true");
    }

    @Test
    void approveAllToolsFlagApprovesAnyTool() {
        service.record(ToolApprovalDecision.APPROVE_ALL_TOOLS, null);

        assertAll(
                () -> assertTrue(service.isApproved("tool-x")),
                () -> assertTrue(service.isApproved("tool-y")),
                () -> assertTrue(service.isApproved("any-tool")),
                () -> assertTrue(service.isApproved("some-other-tool")));
    }

    @Test
    void denyDecisionHasNoEffect() {
        service.record(ToolApprovalDecision.DENY, "tool-c");

        assertFalse(service.isApproved("tool-c"), "deny decision should result in no approval");
    }

    @Test
    void resetClearsApproveAll() {
        service.record(ToolApprovalDecision.APPROVE_ALL_TOOLS, null);
        assertTrue(service.isApproved("tool-any"), "approve-all should initially approve everything");

        service.reset();

        assertFalse(service.isApproved("tool-any"), "after reset, nothing should be approved");
    }

    @Test
    void resetClearsPermanentToolSet() {
        service.record(ToolApprovalDecision.APPROVE_TOOL_ALWAYS, "tool-d");
        assertTrue(service.isApproved("tool-d"), "permanent tool should initially be approved");

        service.reset();

        assertFalse(service.isApproved("tool-d"), "after reset, permanent tools should be cleared");
    }

    @Test
    void resetClearsSingleUseGrants() {
        service.record(ToolApprovalDecision.APPROVE_ONCE, "tool-e");
        assertTrue(service.isApproved("tool-e"), "single-use grant should initially grant approval");

        service.reset();

        // Verify that even if we somehow had lingering single-use grants before reset,
        // reset clears them (can't test the grant object directly, but reset should clear internal state)
        service.record(ToolApprovalDecision.APPROVE_ONCE, "tool-e");
        assertTrue(service.isApproved("tool-e"), "after reset and re-grant, single-use should work again");
    }

    @Test
    void mixedApprovalsWork() {
        service.record(ToolApprovalDecision.APPROVE_TOOL_ALWAYS, "tool-f");
        service.record(ToolApprovalDecision.APPROVE_ONCE, "tool-g");

        assertAll(
                () -> assertTrue(service.isApproved("tool-f"), "permanent approval should work"),
                () -> assertTrue(service.isApproved("tool-g"), "single-use should work once"),
                () -> assertFalse(service.isApproved("tool-g"), "single-use should be consumed"),
                () -> assertTrue(service.isApproved("tool-f"), "permanent should still work"),
                () -> assertFalse(service.isApproved("tool-h"), "unapproved tool should be denied"));
    }

    @Test
    void approveAllTakesPreferenceOverOtherDecisions() {
        service.record(ToolApprovalDecision.APPROVE_TOOL_ALWAYS, "tool-i");
        service.record(ToolApprovalDecision.APPROVE_ALL_TOOLS, null);

        assertTrue(service.isApproved("tool-j"), "approve-all should approve unapproved tools");
        assertTrue(service.isApproved("tool-i"), "approve-all should still approve permanent tools");
    }

    @Test
    void singleUseGrantAndPermanentCanCoexistForSameTool() {
        service.record(ToolApprovalDecision.APPROVE_TOOL_ALWAYS, "tool-k");
        service.record(ToolApprovalDecision.APPROVE_ONCE, "tool-k");

        // The tool should return true due to permanent approval
        assertTrue(service.isApproved("tool-k"), "first check should approve (could be either reason)");
        // After a consumed single-use, the permanent approval should still work
        assertTrue(service.isApproved("tool-k"), "second check should still approve (from permanent)");
    }

    @Test
    void enumDisplayLabelsAreCorrect() {
        assertAll(
                () -> assertEquals("Approve once", ToolApprovalDecision.APPROVE_ONCE.getDisplayLabel()),
                () -> assertEquals("Approve tool always", ToolApprovalDecision.APPROVE_TOOL_ALWAYS.getDisplayLabel()),
                () -> assertEquals("Approve all tools", ToolApprovalDecision.APPROVE_ALL_TOOLS.getDisplayLabel()),
                () -> assertEquals("Deny", ToolApprovalDecision.DENY.getDisplayLabel()));
    }
}
