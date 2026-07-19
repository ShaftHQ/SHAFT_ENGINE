package com.shaft.intellij.ui;

import com.intellij.ui.JBColor;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;

import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import java.lang.reflect.InvocationTargetException;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Snapshots and restores the ambient Swing {@link UIManager}/{@link LookAndFeel} state (issue
 * #3782) that any test in this module installing a real platform L&F (e.g. {@code IntelliJLaf} /
 * {@code DarculaLaf}) would otherwise leak into every other test sharing the same JVM: the
 * {@link LookAndFeel} instance, the {@link JBColor} dark/bright flag, and a declared set of
 * {@link UIManager} keys. All snapshot/restore work runs on the EDT via
 * {@link SwingUtilities#invokeAndWait}, matching how Swing state must always be touched.
 *
 * <p>Root cause (issue #3782, and the memory gotcha recorded from PR #3781): {@code
 * UIManager.put()} overrides persist across {@code UIManager.setLookAndFeel()} switches by Swing
 * design instead of resetting with them, and IntelliJ's {@code DefaultTreeUI} only becomes the
 * active {@code TreeUI} delegate (with assertions that do not tolerate a L&F swap mid-suite) once
 * a platform L&F has been installed at least once in the JVM. Restoring the exact pre-test
 * {@link LookAndFeel} <em>instance</em> (not just its class name) plus every touched key -- not
 * just clearing them -- is what PR #3781's inline finally block proved sufficient (5/5 consecutive
 * green full-suite runs); this extension extracts that proven mechanism so every test that installs
 * an L&F can reuse it instead of hand-rolling its own try/finally.
 *
 * <p>Register with {@code @RegisterExtension} (a plain {@code @ExtendWith(...)} cannot pass the
 * declared key set, since JUnit would need a no-arg constructor for that).
 */
final class LookAndFeelIsolationExtension implements BeforeEachCallback, AfterEachCallback {
    private final String[] uiManagerKeys;
    private LookAndFeel originalLookAndFeel;
    private Boolean originalIsBright;
    private final Map<String, Object> originalUiManagerValues = new LinkedHashMap<>();

    LookAndFeelIsolationExtension(String... uiManagerKeys) {
        this.uiManagerKeys = uiManagerKeys;
    }

    @Override
    public void beforeEach(ExtensionContext context) throws InterruptedException, InvocationTargetException {
        runOnEdt(() -> {
            originalLookAndFeel = UIManager.getLookAndFeel();
            originalIsBright = JBColor.isBright();
            originalUiManagerValues.clear();
            for (String key : uiManagerKeys) {
                originalUiManagerValues.put(key, UIManager.get(key));
            }
        });
    }

    @Override
    public void afterEach(ExtensionContext context) throws InterruptedException, InvocationTargetException {
        runOnEdt(() -> {
            try {
                if (originalLookAndFeel != null) {
                    UIManager.setLookAndFeel(originalLookAndFeel);
                }
            } catch (UnsupportedLookAndFeelException exception) {
                throw new IllegalStateException("Unable to restore the pre-test look and feel", exception);
            }
            if (originalIsBright != null) {
                JBColor.setDark(!originalIsBright);
            }
            // UIManager.put(key, null) removes the override entirely, so keys that had no prior
            // value go back to falling through to the (now-restored) L&F's own default.
            originalUiManagerValues.forEach(UIManager::put);
        });
    }

    private static void runOnEdt(Runnable action) throws InterruptedException, InvocationTargetException {
        SwingUtilities.invokeAndWait(action::run);
    }
}
