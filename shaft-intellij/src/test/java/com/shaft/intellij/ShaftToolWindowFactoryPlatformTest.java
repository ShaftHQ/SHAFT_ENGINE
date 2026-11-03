package com.shaft.intellij;

import com.intellij.testFramework.fixtures.BasePlatformTestCase;

/** Platform coverage for W1.7 shouldBeAvailable. */
public class ShaftToolWindowFactoryPlatformTest extends BasePlatformTestCase {
    public void testShouldBeAvailableDelegatesToDetector() {
        ShaftToolWindowFactory factory = new ShaftToolWindowFactory();
        // Light fixture project has no SHAFT markers.
        assertFalse(factory.shouldBeAvailable(getProject()));
    }
}
