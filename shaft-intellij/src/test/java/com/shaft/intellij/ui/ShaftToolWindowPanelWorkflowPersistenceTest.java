package com.shaft.intellij.ui;

import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.swing.JComboBox;
import java.lang.reflect.Proxy;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Covers issue #3636's workflow-selector persistence: the last-selected workflow view survives an
 * IDE restart (a fresh {@link ShaftToolWindowPanel} restores it), selecting a different view
 * persists the new choice, and a stored key that no longer matches any current view (e.g. after a
 * plugin update changes the available workflows) falls back silently to the existing default (the
 * first item).
 *
 * <p>Kept in its own file rather than added to {@link ShaftToolWindowPanelTest}, whose javadoc scopes
 * it specifically to issue #3619's dispose-cascade regression, an unrelated concern.</p>
 */
class ShaftToolWindowPanelWorkflowPersistenceTest {

    @Test
    void aFreshPanelWithNothingStoredDefaultsToTheFirstWorkflowView() {
        ShaftToolWindowPanel panel = newPanel(new FakePropertiesComponent());

        assertEquals("Assistant", selectedLabel(panel),
                "with nothing persisted yet, the existing first-item default must still apply");
    }

    @Test
    void selectingADifferentWorkflowPersistsItsKey() {
        FakePropertiesComponent properties = new FakePropertiesComponent();
        ShaftToolWindowPanel panel = newPanel(properties);
        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = panel.workflowSelector();

        selectItemLabeled(selector, "Recorder");

        assertEquals("Recorder", properties.getValue(ShaftUiState.WORKFLOW_VIEW_KEY),
                "selecting a different workflow view must persist its key immediately");
    }

    @Test
    void aFreshPanelRestoresThePreviouslyPersistedWorkflowView() {
        FakePropertiesComponent properties = new FakePropertiesComponent();
        properties.setValue(ShaftUiState.WORKFLOW_VIEW_KEY, "Inspector");

        ShaftToolWindowPanel panel = newPanel(properties);

        assertEquals("Inspector", selectedLabel(panel),
                "a fresh panel must restore the last-selected workflow view, not default back to Assistant");
    }

    @Test
    void aStoredKeyThatNoLongerMatchesAnyViewFallsBackToTheDefaultInsteadOfThrowing() {
        FakePropertiesComponent properties = new FakePropertiesComponent();
        // Simulates a plugin update that renamed/removed a workflow view since this key was stored.
        properties.setValue(ShaftUiState.WORKFLOW_VIEW_KEY, "No Longer Exists");

        ShaftToolWindowPanel panel = newPanel(properties);

        assertEquals("Assistant", selectedLabel(panel),
                "an unmatched stored key must fall back silently to the default first item");
    }

    private static void selectItemLabeled(JComboBox<ShaftToolWindowPanel.WorkflowView> selector, String label) {
        for (int i = 0; i < selector.getItemCount(); i++) {
            if (label.equals(selector.getItemAt(i).label())) {
                selector.setSelectedItem(selector.getItemAt(i));
                return;
            }
        }
        throw new AssertionError("No workflow view labeled '" + label + "' in the selector");
    }

    private static String selectedLabel(ShaftToolWindowPanel panel) {
        return panel.workflowSelector().getSelectedItem() == null
                ? null
                : ((ShaftToolWindowPanel.WorkflowView) panel.workflowSelector().getSelectedItem()).label();
    }

    private static ShaftToolWindowPanel newPanel(PropertiesComponent properties) {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        settings.mcpCommand = "shaft-mcp";
        settings.advancedUiEnabled = true;
        Project project = fakeProject(properties);
        return new ShaftToolWindowPanel(project, settings, (client, runtime) -> null,
                ShaftAssistantChatState.getInstance(project));
    }

    /**
     * A {@link Project} stub whose {@code getService} returns {@code properties} for a
     * {@link PropertiesComponent} request (mirroring {@code PropertiesComponent.getInstance(project)}'s
     * real implementation: {@code project.getService(PropertiesComponent.class)}, verified empirically
     * via its bytecode) and {@code null} otherwise -- matching this suite's established
     * {@code trapProject}/{@code fakeProject} pattern (see {@code ShaftFeaturePanelCatalogTest}).
     */
    private static Project fakeProject(PropertiesComponent properties) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> {
                    switch (method.getName()) {
                        case "equals":
                            return proxy == (arguments == null || arguments.length == 0 ? null : arguments[0]);
                        case "hashCode":
                            return System.identityHashCode(proxy);
                        case "getBasePath":
                            return "";
                        case "getName":
                            return "shaft-tool-window-panel-workflow-persistence-test-project";
                        case "getService":
                            return arguments != null && arguments.length > 0
                                    && arguments[0] == PropertiesComponent.class
                                    ? properties
                                    : null;
                        default:
                            return defaultValue(method.getReturnType());
                    }
                });
    }

    private static Object defaultValue(Class<?> returnType) {
        if (!returnType.isPrimitive()) {
            return null;
        }
        if (returnType == boolean.class) {
            return false;
        }
        return 0;
    }

    /** Minimal in-memory {@link PropertiesComponent} fake: this module has no real one to reuse. */
    private static final class FakePropertiesComponent extends PropertiesComponent {
        private final Map<String, String> values = new HashMap<>();

        @Override
        public void unsetValue(String name) {
            values.remove(name);
        }

        @Override
        public boolean isValueSet(String name) {
            return values.containsKey(name);
        }

        @Override
        public String getValue(String name) {
            return values.get(name);
        }

        @Override
        public void setValue(String name, String value) {
            if (value == null) {
                values.remove(name);
            } else {
                values.put(name, value);
            }
        }

        @Override
        public void setValue(String name, String value, String defaultValue) {
            if (Objects.equals(value, defaultValue)) {
                values.remove(name);
            } else {
                setValue(name, value);
            }
        }

        @Override
        public void setValue(String name, float value, float defaultValue) {
            if (value == defaultValue) {
                values.remove(name);
            } else {
                values.put(name, Float.toString(value));
            }
        }

        @Override
        public void setValue(String name, int value, int defaultValue) {
            if (value == defaultValue) {
                values.remove(name);
            } else {
                values.put(name, Integer.toString(value));
            }
        }

        @Override
        public void setValue(String name, boolean value, boolean defaultValue) {
            if (value == defaultValue) {
                values.remove(name);
            } else {
                values.put(name, Boolean.toString(value));
            }
        }

        @Override
        @SuppressWarnings("deprecation")
        public String[] getValues(String name) {
            return null;
        }

        @Override
        @SuppressWarnings("deprecation")
        public void setValues(String name, String[] values) {
            // Unused by ShaftUiState.
        }

        @Override
        public List<String> getList(String name) {
            return null;
        }

        @Override
        public void setList(String name, Collection<String> values) {
            // Unused by ShaftUiState.
        }

        @Override
        public boolean updateValue(String name, boolean value) {
            return false;
        }
    }
}
