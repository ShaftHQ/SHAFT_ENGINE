package com.shaft.gui;

import org.openqa.selenium.bidi.BiDi;
import org.openqa.selenium.bidi.Handle;

/** Test-only bridge from a mock BiDi transport to Selenium's supported opaque handle. */
public final class BidiTestSupport {
    private BidiTestSupport() {
        throw new IllegalStateException("Utility class");
    }

    @SuppressWarnings("PMD.AvoidAccessibilityAlteration")
    public static Handle handleFor(BiDi bidi) {
        try {
            var constructor = Handle.class.getDeclaredConstructor(BiDi.class);
            constructor.setAccessible(true);
            return constructor.newInstance(bidi);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Could not create a test BiDi handle.", exception);
        }
    }
}
