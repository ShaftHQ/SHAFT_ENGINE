package com.shaft.sikulix;

import com.shaft.gui.element.SikuliActions;
import org.sikuli.script.Pattern;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.awt.GraphicsEnvironment;

/**
 * Smoke for OculiX 4.0.0 (#6160): OculiX API stays on the classpath, SHAFT's
 * {@code SikuliNativeLibraryStager} is gone, and on a headed display constructing
 * {@link Pattern} must not need the deleted stager (natives via Apertix).
 */
public class SikuliOpenCvNativeLoadTest {

    @Test
    public void patternConstructionLoadsOpenCvNativeLibrary() throws Exception {
        Assert.assertNotNull(Class.forName("org.sikuli.script.Pattern"));
        Assert.assertNotNull(Class.forName("org.sikuli.script.Screen"));
        try {
            Class.forName("com.shaft.sikulix.internal.SikuliNativeLibraryStager");
            Assert.fail("SikuliNativeLibraryStager must be deleted after OculiX migration (#6160)");
        } catch (ClassNotFoundException expected) {
            // expected — stager removed
        }
        if (GraphicsEnvironment.isHeadless()) {
            throw new SkipException("Pattern native smoke needs a display; classpath checks already passed");
        }
        new SikuliActions();
        Assert.assertNotNull(new Pattern());
    }
}
