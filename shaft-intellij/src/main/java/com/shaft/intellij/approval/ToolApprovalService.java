package com.shaft.intellij.approval;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.components.StoragePathMacros;
import com.intellij.openapi.project.Project;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashSet;
import java.util.Set;

/**
 * Project-level service for managing tool approval decisions.
 *
 * Persists the approve-all flag and the set of permanently approved tool names to the project's
 * workspace file, so a grant made in one project never authorizes tools in another project opened
 * in the same IDE session. In-memory single-use grants are tracked separately and cleared on reset.
 */
@State(name = "ToolApprovalService", storages = @Storage(StoragePathMacros.WORKSPACE_FILE))
public final class ToolApprovalService implements PersistentStateComponent<ToolApprovalService.ApprovalState> {
    private ApprovalState state = new ApprovalState();
    private final Set<String> singleUseGrants = new HashSet<>();

    /**
     * Returns the project-level tool approval service.
     *
     * @param project the project to scope approvals to, or {@code null} for a fresh, unpersisted instance
     * @return tool approval service
     */
    public static ToolApprovalService getInstance(@Nullable Project project) {
        if (project == null) {
            return new ToolApprovalService();
        }
        ToolApprovalService service = project.getService(ToolApprovalService.class);
        return service == null ? new ToolApprovalService() : service;
    }

    @Override
    public ApprovalState getState() {
        return state;
    }

    @Override
    public void loadState(@NotNull ApprovalState state) {
        XmlSerializerUtil.copyBean(state, this.state);
    }

    /**
     * Checks whether a tool invocation is approved.
     *
     * Returns true if any of the following conditions hold:
     * - The approve-all flag is set
     * - The tool is in the permanent approval set
     * - A single-use grant exists for this tool (and consumes it on return)
     *
     * @param toolName the name of the tool to check
     * @return true if approved, false otherwise
     */
    public boolean isApproved(String toolName) {
        if (state.approveAllTools) {
            return true;
        }

        if (state.permanentlyApprovedTools.contains(toolName)) {
            return true;
        }

        if (singleUseGrants.contains(toolName)) {
            singleUseGrants.remove(toolName);
            return true;
        }

        return false;
    }

    /**
     * Records an approval decision for a tool.
     *
     * - APPROVE_ONCE: adds a single-use grant (consumed on next isApproved call)
     * - APPROVE_TOOL_ALWAYS: adds the tool to the permanent approval set
     * - APPROVE_ALL_TOOLS: sets the approve-all flag
     * - DENY: does nothing
     *
     * @param decision the approval decision
     * @param toolName the name of the tool
     */
    public void record(ToolApprovalDecision decision, String toolName) {
        switch (decision) {
            case APPROVE_ONCE:
                singleUseGrants.add(toolName);
                break;
            case APPROVE_TOOL_ALWAYS:
                state.permanentlyApprovedTools.add(toolName);
                break;
            case APPROVE_ALL_TOOLS:
                state.approveAllTools = true;
                break;
            case DENY:
                // No action needed
                break;
        }
    }

    /**
     * Resets all approval state: clears the approve-all flag, permanent tool set,
     * and in-memory single-use grants.
     */
    public void reset() {
        state.approveAllTools = false;
        state.permanentlyApprovedTools.clear();
        singleUseGrants.clear();
    }

    /**
     * Mutable XML-serializable approval state bean.
     */
    public static final class ApprovalState {
        public boolean approveAllTools = false;
        public Set<String> permanentlyApprovedTools = new HashSet<>();
    }
}
