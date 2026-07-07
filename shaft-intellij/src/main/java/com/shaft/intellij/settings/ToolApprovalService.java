package com.shaft.intellij.settings;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import org.jetbrains.annotations.NotNull;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Remembers which MCP tool calls the user has approved to run without a repeated confirmation
 * prompt. Persisted at the application level so a factory reset (see
 * {@code ShaftPluginResetService}) can return every tool to its default, unapproved state.
 */
@State(name = "ShaftToolApproval", storages = @Storage("shaft.xml"))
public final class ToolApprovalService implements PersistentStateComponent<ToolApprovalService.State> {
    private final Set<String> approvedTools = new LinkedHashSet<>();

    /**
     * Returns the application-level tool approval service.
     *
     * @return tool approval service
     */
    public static ToolApprovalService getInstance() {
        return ApplicationManager.getApplication().getService(ToolApprovalService.class);
    }

    /**
     * Records that the given tool id no longer requires a confirmation prompt.
     *
     * @param toolId MCP tool id
     */
    public void approve(String toolId) {
        if (toolId != null && !toolId.isBlank()) {
            approvedTools.add(toolId);
        }
    }

    /**
     * Indicates whether the given tool id has been approved.
     *
     * @param toolId MCP tool id
     * @return true if the tool was previously approved
     */
    public boolean isApproved(String toolId) {
        return toolId != null && approvedTools.contains(toolId);
    }

    /**
     * Revokes a previously granted approval.
     *
     * @param toolId MCP tool id
     */
    public void revoke(String toolId) {
        approvedTools.remove(toolId);
    }

    /**
     * Indicates whether no tools are currently approved.
     *
     * @return true if the approval store is empty
     */
    public boolean isEmpty() {
        return approvedTools.isEmpty();
    }

    /**
     * Clears every recorded approval, returning this service to a fresh-install baseline.
     */
    public void clearAll() {
        approvedTools.clear();
    }

    @Override
    public State getState() {
        State state = new State();
        state.approvedTools.addAll(approvedTools);
        return state;
    }

    @Override
    public void loadState(@NotNull State state) {
        approvedTools.clear();
        if (state.approvedTools != null) {
            approvedTools.addAll(state.approvedTools);
        }
    }

    /**
     * Mutable XML-serializable approval state.
     */
    public static final class State {
        public Set<String> approvedTools = new LinkedHashSet<>();
    }
}
