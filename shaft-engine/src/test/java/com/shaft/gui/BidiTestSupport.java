package com.shaft.gui;

import org.mockito.Mockito;
import org.openqa.selenium.bidi.BiDi;
import org.openqa.selenium.bidi.Handle;
import org.openqa.selenium.bidi.HasBiDi;

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

    /** Supplies both the supported handle and Selenium 4.47's current high-level-module transport. */
    @SuppressWarnings("PMD.AvoidAccessibilityAlteration")
    public static void connect(HasBiDi driver, BiDi bidi) {
        Mockito.when(driver.getHandle()).thenReturn(handleFor(bidi));
        try {
            var deprecatedTransport = HasBiDi.class.getMethod("getBiDi");
            deprecatedTransport.invoke(Mockito.doReturn(bidi).when(driver));
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Could not connect a test BiDi transport.", exception);
        }
    }
}
