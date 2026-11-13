package com.shaft.gui.element.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

public class PageSourcePresenceTest {

    @Test
    public void nullXPathNodesAreRejected() {
        Assert.assertThrows(IllegalArgumentException.class, () -> PageSourcePresence.xpathNodeCount(null));
    }
}
