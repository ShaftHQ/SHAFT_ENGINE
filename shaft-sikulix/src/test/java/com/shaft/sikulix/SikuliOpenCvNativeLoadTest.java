package com.shaft.sikulix;

import com.shaft.gui.element.SikuliActions;
import org.sikuli.script.Pattern;
import org.testng.annotations.Test;

import static org.testng.Assert.assertNotNull;

/**
 * Headless regression coverage for issue #3805: SikuliX's native OpenCV loader
 * ({@code org.sikuli.script.support.RunTime#libsLoad}) must be able to resolve
 * {@code opencv_java490.dll} (the native matching the {@code org.openpnp:opencv:4.9.0-0}
 * dependency pinned in shaft-sikulix/pom.xml) from its "libs folder"
 * ({@code %APPDATA%/Sikulix/SikulixLibs} on Windows). SikuliX 2.0.5's own bundled
 * manifest still references the older {@code opencv_java430.dll}, so without SHAFT
 * staging the correct native first, the very first {@link Pattern} construction throws
 * {@code ExceptionInInitializerError} wrapping
 * {@code SikuliXception: loadlib: opencv_java490.dll not in any libs folder} --
 * exactly the Windows_SikuliX_Local nightly failure (run 29673202709, job 88155817101).
 * <p>
 * This does not open any window, capture the desktop, or otherwise touch the GUI --
 * {@link Pattern}'s constructor only triggers the {@code Finder$Finder2} static
 * initializer that loads the native OpenCV matcher backend.
 */
public class SikuliOpenCvNativeLoadTest {

    @Test
    public void patternConstructionLoadsOpenCvNativeLibrary() {
        // Mirrors SikuliActions' constructor-time bootstrap, the same entry point
        // com.shaft.gui.element.SikuliActions#prepareElementPattern uses in the
        // failing CI flow (SikuliActions.java:353 -> Pattern.java:128 -> Finder2.<clinit>).
        new SikuliActions();
        Pattern pattern = new Pattern();
        assertNotNull(pattern);
    }
}
