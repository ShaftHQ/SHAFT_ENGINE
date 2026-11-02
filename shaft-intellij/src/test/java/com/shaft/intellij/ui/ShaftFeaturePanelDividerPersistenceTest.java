package com.shaft.intellij.ui;

import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Proxy;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Covers issue #3636's Advanced-tools split pane divider persistence: a valid stored divider
 * location is restored on construction, a negative or implausibly large one is rejected instead of
 * being handed to Swing (verified empirically: {@code JSplitPane#setDividerLocation(int)} does not
 * clamp an out-of-range value on its own -- it just stores whatever it is given), and moving the
 * divider persists the new location immediately.
 *
 * <p>Kept in its own file rather than added to {@link ShaftFeaturePanelCatalogTest}/
 * {@link ShaftFeaturePanelRecoveryTest}, whose javadoc scopes each of those to a different, unrelated
 * concern (matching this suite's own established one-concern-per-file convention).</p>
 */
class ShaftFeaturePanelDividerPersistenceTest {

    @Test
    void aValidStoredDividerLocationIsRestoredOnConstruction() {
        FakePropertiesComponent properties = new FakePropertiesComponent();
        properties.setValue(ShaftUiState.FEATURE_SPLIT_DIVIDER_KEY, 240, Integer.MIN_VALUE);

        ShaftFeaturePanel panel = new ShaftFeaturePanel(fakeProject(properties), blankMcpSettings());

        assertEquals(240, panel.splitPane.getDividerLocation(),
                "a valid persisted divider location must be restored on construction");
    }

    @Test
    void aNegativeStoredDividerLocationFallsBackToTheDefaultInsteadOfBeingApplied() {
        FakePropertiesComponent properties = new FakePropertiesComponent();
        properties.setValue(ShaftUiState.FEATURE_SPLIT_DIVIDER_KEY, -5, Integer.MIN_VALUE);

        ShaftFeaturePanel panel = new ShaftFeaturePanel(fakeProject(properties), blankMcpSettings());

        assertEquals(defaultDividerLocation(), panel.splitPane.getDividerLocation(),
                "a negative stored divider location must be rejected, leaving the resizeWeight default "
                        + "in charge instead of being handed straight to setDividerLocation()");
    }

    @Test
    void anImplausiblyLargeStoredDividerLocationFallsBackToTheDefaultInsteadOfBeingApplied() {
        FakePropertiesComponent properties = new FakePropertiesComponent();
        // A stale value from a much bigger monitor/window, or plainly corrupt data.
        properties.setValue(ShaftUiState.FEATURE_SPLIT_DIVIDER_KEY, 5_000_000, Integer.MIN_VALUE);

        ShaftFeaturePanel panel = new ShaftFeaturePanel(fakeProject(properties), blankMcpSettings());

        assertEquals(defaultDividerLocation(), panel.splitPane.getDividerLocation(),
                "an implausibly large stored divider location must be rejected, not misplace the divider");
    }

    @Test
    void movingTheDividerPersistsTheNewLocationImmediately() {
        FakePropertiesComponent properties = new FakePropertiesComponent();
        ShaftFeaturePanel panel = new ShaftFeaturePanel(fakeProject(properties), blankMcpSettings());

        panel.splitPane.setDividerLocation(180);

        assertEquals(180, properties.getInt(ShaftUiState.FEATURE_SPLIT_DIVIDER_KEY, -1),
                "moving the divider must persist the new location on every change, not just on dispose");
    }

    @Test
    void aNullProjectSkipsPersistenceEntirelyWithoutThrowing() {
        ShaftFeaturePanel panel = new ShaftFeaturePanel(null, blankMcpSettings());

        panel.splitPane.setDividerLocation(150);

        assertEquals(150, panel.splitPane.getDividerLocation(),
                "the live UI must still respond to a divider move even with no project to persist against");
    }

    /**
     * The natural, un-restored divider location a fresh {@link ShaftFeaturePanel} produces when
     * nothing is persisted (project == null, so {@link ShaftUiState} is a defaults-only no-op) --
     * used instead of hardcoding an assumed Swing constant, since that natural value is an
     * implementation detail of {@link javax.swing.JSplitPane} before it is ever laid out.
     */
    private static int defaultDividerLocation() {
        return new ShaftFeaturePanel(null, blankMcpSettings()).splitPane.getDividerLocation();
    }

    private static ShaftSettingsState.Settings blankMcpSettings() {
        return new ShaftSettingsState.Settings();
    }

    /**
     * A {@link Project} stub whose {@code getService} returns {@code properties} for a
     * {@link PropertiesComponent} request (mirroring {@code PropertiesComponent.getInstance(project)}'s
     * real implementation: {@code project.getService(PropertiesComponent.class)}, verified empirically
     * via its bytecode) and {@code null} otherwise -- matching this suite's established
     * {@code trapProject}/{@code fakeProject} pattern (see {@link ShaftFeaturePanelCatalogTest}).
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
                            return "shaft-feature-panel-divider-persistence-test-project";
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
