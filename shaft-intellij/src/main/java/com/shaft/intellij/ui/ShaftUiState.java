package com.shaft.intellij.ui;

import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.project.Project;

/**
 * Thin wrapper over {@link PropertiesComponent} for the two pieces of tool-window UI state that
 * should survive an IDE restart (issue #3636): the last-selected workflow view and the Advanced
 * tools split pane's divider position. Deliberately narrow -- exactly these two keys, no general
 * persistence framework.
 */
final class ShaftUiState {
    static final String WORKFLOW_VIEW_KEY = "shaft.ui.workflowView";
    static final String FEATURE_SPLIT_DIVIDER_KEY = "shaft.ui.featureSplitDivider";

    private final PropertiesComponent properties;

    /** Package-private so tests can inject a fake {@link PropertiesComponent} directly. */
    ShaftUiState(PropertiesComponent properties) {
        this.properties = properties;
    }

    /**
     * Returns the state wrapper for {@code project}, or a defaults-only wrapper (nothing persisted,
     * nothing restored) when {@code project} is {@code null}. {@link PropertiesComponent#getInstance(Project)}
     * itself rejects a null project, and {@code ShaftFeaturePanel}/{@code ShaftToolWindowPanel} are
     * both already constructed with {@code project == null} in headless tests and pre-project-open
     * states, so this mirrors the same null-project fallback {@link ShaftAssistantChatState} uses.
     */
    static ShaftUiState getInstance(Project project) {
        return new ShaftUiState(project == null ? null : PropertiesComponent.getInstance(project));
    }

    /** Last-persisted workflow view key, or {@code null} if nothing is stored yet. */
    String workflowView() {
        return properties == null ? null : properties.getValue(WORKFLOW_VIEW_KEY);
    }

    void setWorkflowView(String value) {
        if (properties != null) {
            properties.setValue(WORKFLOW_VIEW_KEY, value);
        }
    }

    /** Last-persisted Advanced-tools split pane divider location, or {@code defaultValue} if unset. */
    int featureSplitDivider(int defaultValue) {
        return properties == null ? defaultValue : properties.getInt(FEATURE_SPLIT_DIVIDER_KEY, defaultValue);
    }

    void setFeatureSplitDivider(int value) {
        if (properties != null) {
            // PropertiesComponent#setValue(name, value, default) only persists when value != default
            // (it treats "equals default" as "nothing to store"). A real divider location can never
            // equal Integer.MIN_VALUE, so every real change is written, not just ones that happen to
            // differ from an arbitrary baseline.
            properties.setValue(FEATURE_SPLIT_DIVIDER_KEY, value, Integer.MIN_VALUE);
        }
    }
}
