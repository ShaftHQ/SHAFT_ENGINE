package com.shaft.intellij.ui;

import com.intellij.ide.util.PropertiesComponent;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Covers {@link ShaftUiState}'s round trip over a fake {@link PropertiesComponent}. This module has
 * no {@code BasePlatformTestCase} fixture to back a real one (see
 * {@code PickLocatorAtCaretActionTest}'s javadoc for the same documented gap), so a minimal
 * in-memory fake stands in. Also pins the {@code null}-project fallback used by headless/
 * pre-project-open callers ({@code ShaftFeaturePanel}, {@code ShaftToolWindowPanel} are both already
 * constructed with {@code project == null} elsewhere in this test suite), so it degrades to defaults
 * instead of throwing.
 */
class ShaftUiStateTest {

    @Test
    void workflowViewRoundTripsThroughAFakePropertiesComponent() {
        ShaftUiState state = new ShaftUiState(new FakePropertiesComponent());

        state.setWorkflowView("Recorder");

        assertEquals("Recorder", state.workflowView());
    }

    @Test
    void workflowViewIsNullWhenNothingIsStoredYet() {
        ShaftUiState state = new ShaftUiState(new FakePropertiesComponent());

        assertNull(state.workflowView(), "no prior setWorkflowView call must read back as null, not a blank default");
    }

    @Test
    void featureSplitDividerRoundTripsThroughAFakePropertiesComponent() {
        ShaftUiState state = new ShaftUiState(new FakePropertiesComponent());

        state.setFeatureSplitDivider(240);

        assertEquals(240, state.featureSplitDivider(-1));
    }

    @Test
    void featureSplitDividerReturnsTheGivenDefaultWhenNothingIsStoredYet() {
        ShaftUiState state = new ShaftUiState(new FakePropertiesComponent());

        assertEquals(-1, state.featureSplitDivider(-1));
        assertEquals(777, state.featureSplitDivider(777), "the caller's default overload must be honored, not a hardcoded one");
    }

    @Test
    void getInstanceWithANullProjectDegradesToDefaultsInsteadOfThrowing() {
        // PropertiesComponent.getInstance(project) itself rejects a null project (@NotNull-annotated,
        // verified empirically via its bytecode); ShaftUiState.getInstance(null) must short-circuit
        // before ever reaching that call, mirroring ShaftAssistantChatState's own null-project fallback.
        ShaftUiState state = ShaftUiState.getInstance(null);

        assertNull(state.workflowView());
        assertEquals(-1, state.featureSplitDivider(-1));

        state.setWorkflowView("Guided");
        state.setFeatureSplitDivider(99);

        assertNull(state.workflowView(), "setters on the null-project fallback must be no-ops, not throw");
        assertEquals(-1, state.featureSplitDivider(-1));
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
