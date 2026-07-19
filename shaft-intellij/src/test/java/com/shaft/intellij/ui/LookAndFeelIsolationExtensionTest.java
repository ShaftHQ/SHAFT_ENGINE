package com.shaft.intellij.ui;

import com.intellij.ui.JBColor;
import org.junit.jupiter.api.Test;

import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import java.awt.Color;
import java.lang.reflect.InvocationTargetException;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

/**
 * Issue #3782: proves {@link LookAndFeelIsolationExtension} actually restores the ambient Swing
 * state it snapshots -- the {@link LookAndFeel} instance, the {@link JBColor} dark/bright flag, and
 * a declared set of {@link UIManager} keys -- instead of just clearing or ignoring it. Calls the
 * extension's {@code beforeEach}/{@code afterEach} hooks directly with a {@code null}
 * {@link org.junit.jupiter.api.extension.ExtensionContext}: neither hook dereferences that
 * argument (state lives in the extension instance's own fields, matching PR #3781's proven
 * finally-block mechanism this extension extracts), so this exercises the real production
 * snapshot/restore logic without standing up the full JUnit Platform engine to test a JUnit
 * extension.
 */
class LookAndFeelIsolationExtensionTest {
    private static final String KEY = "Button.background";
    private static final String INTELLIJ_LAF = "com.intellij.ide.ui.laf.IntelliJLaf";

    @Test
    void restoresLookAndFeelJbColorAndDeclaredUiManagerKeysAfterATestMutatesThem()
            throws Exception {
        AtomicReference<LookAndFeel> originalLookAndFeel = new AtomicReference<>();
        AtomicReference<Boolean> originalIsBright = new AtomicReference<>();
        AtomicReference<Object> originalKeyValue = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() -> {
            originalLookAndFeel.set(UIManager.getLookAndFeel());
            originalIsBright.set(JBColor.isBright());
            originalKeyValue.set(UIManager.get(KEY));
        });

        LookAndFeelIsolationExtension extension = new LookAndFeelIsolationExtension(KEY);
        extension.beforeEach(null);
        try {
            mutateSwingState();
            // Sanity-check the mutation actually took hold, or the restore assertions below would
            // be vacuously green for the wrong reason (nothing to restore in the first place).
            SwingUtilities.invokeAndWait(() -> {
                assertNotEquals(originalIsBright.get(), JBColor.isBright(),
                        "test setup should have flipped the JBColor dark flag");
                assertEquals(new Color(0x45494A), UIManager.get(KEY),
                        "test setup should have overridden the declared UIManager key");
            });
        } finally {
            extension.afterEach(null);
        }

        SwingUtilities.invokeAndWait(() -> {
            assertEquals(originalLookAndFeel.get(), UIManager.getLookAndFeel(),
                    "the exact pre-test LookAndFeel instance should be restored");
            assertEquals(originalIsBright.get(), JBColor.isBright(),
                    "the JBColor dark/bright flag should be restored");
            assertEquals(originalKeyValue.get(), UIManager.get(KEY),
                    "the declared UIManager key should be restored to its pre-test value");
        });
    }

    private static void mutateSwingState() throws InterruptedException, InvocationTargetException {
        SwingUtilities.invokeAndWait(() -> {
            try {
                UIManager.setLookAndFeel(INTELLIJ_LAF);
            } catch (ClassNotFoundException | InstantiationException | IllegalAccessException
                     | UnsupportedLookAndFeelException exception) {
                throw new IllegalStateException("Unable to install " + INTELLIJ_LAF + " for the test", exception);
            }
            JBColor.setDark(true);
            // The same near-gray dark override that leaked into ToolApprovalPromptPanelTest per the
            // memory gotcha from PR #3781/#3777.
            UIManager.put(KEY, new Color(0x45494A));
        });
    }
}
