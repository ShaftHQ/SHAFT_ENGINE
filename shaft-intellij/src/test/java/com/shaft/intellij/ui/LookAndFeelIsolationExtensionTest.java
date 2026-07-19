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
 *
 * <p>All Swing state capture happens inside {@link #captureSwingState()} (on the EDT, as Swing
 * requires); every assertion runs directly in the {@code @Test} method body on the captured
 * {@link SwingSnapshot} values, not inside an {@code invokeAndWait} lambda.
 */
class LookAndFeelIsolationExtensionTest {
    private static final String KEY = "Button.background";
    private static final String INTELLIJ_LAF = "com.intellij.ide.ui.laf.IntelliJLaf";
    private static final Color MUTATED_KEY_VALUE = new Color(0x45494A);

    @Test
    void restoresLookAndFeelJbColorAndDeclaredUiManagerKeysAfterATestMutatesThem() throws Exception {
        SwingSnapshot original = captureSwingState();

        LookAndFeelIsolationExtension extension = new LookAndFeelIsolationExtension(KEY);
        extension.beforeEach(null);
        SwingSnapshot mutated;
        try {
            mutateSwingState();
            mutated = captureSwingState();
        } finally {
            extension.afterEach(null);
        }
        SwingSnapshot restored = captureSwingState();

        // Sanity-check the mutation actually took hold, or the restore assertions below would be
        // vacuously green for the wrong reason (nothing to restore in the first place).
        assertNotEquals(original.isBright(), mutated.isBright(),
                "test setup should have flipped the JBColor dark flag");
        assertEquals(MUTATED_KEY_VALUE, mutated.keyValue(),
                "test setup should have overridden the declared UIManager key");

        assertEquals(original.lookAndFeel(), restored.lookAndFeel(),
                "the exact pre-test LookAndFeel instance should be restored");
        assertEquals(original.isBright(), restored.isBright(),
                "the JBColor dark/bright flag should be restored");
        assertEquals(original.keyValue(), restored.keyValue(),
                "the declared UIManager key should be restored to its pre-test value");
    }

    private record SwingSnapshot(LookAndFeel lookAndFeel, boolean isBright, Object keyValue) {
    }

    private static SwingSnapshot captureSwingState() throws InterruptedException, InvocationTargetException {
        AtomicReference<SwingSnapshot> snapshot = new AtomicReference<>();
        SwingUtilities.invokeAndWait(() ->
                snapshot.set(new SwingSnapshot(UIManager.getLookAndFeel(), JBColor.isBright(), UIManager.get(KEY))));
        return snapshot.get();
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
            UIManager.put(KEY, MUTATED_KEY_VALUE);
        });
    }
}
