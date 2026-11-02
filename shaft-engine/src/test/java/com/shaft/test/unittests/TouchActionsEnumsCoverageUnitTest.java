package com.shaft.test.unittests;

import com.shaft.gui.element.TouchActions;

import org.testng.Assert;
import org.testng.annotations.Test;

public class TouchActionsEnumsCoverageUnitTest {
    @Test
    public void swipeZoomAndKeyboardEnumsExposeExpectedValues() {
        Assert.assertEquals(TouchActions.SwipeDirection.values().length, 4);
        Assert.assertEquals(TouchActions.SwipeDirection.valueOf("UP"), TouchActions.SwipeDirection.UP);
        Assert.assertEquals(TouchActions.SwipeDirection.valueOf("DOWN"), TouchActions.SwipeDirection.DOWN);
        Assert.assertEquals(TouchActions.SwipeDirection.valueOf("LEFT"), TouchActions.SwipeDirection.LEFT);
        Assert.assertEquals(TouchActions.SwipeDirection.valueOf("RIGHT"), TouchActions.SwipeDirection.RIGHT);

        Assert.assertEquals(TouchActions.ZoomDirection.values().length, 2);
        Assert.assertEquals(TouchActions.ZoomDirection.valueOf("IN"), TouchActions.ZoomDirection.IN);
        Assert.assertEquals(TouchActions.ZoomDirection.valueOf("OUT"), TouchActions.ZoomDirection.OUT);

        Assert.assertEquals(TouchActions.SwipeMovement.values().length, 2);
        Assert.assertEquals(TouchActions.SwipeMovement.HORIZONTAL.name(), "HORIZONTAL");
        Assert.assertEquals(TouchActions.SwipeMovement.VERTICAL.name(), "VERTICAL");

        Assert.assertTrue(TouchActions.KeyboardKeys.values().length >= 8);
        Assert.assertEquals(TouchActions.KeyboardKeys.valueOf("GO"), TouchActions.KeyboardKeys.GO);
        Assert.assertEquals(TouchActions.KeyboardKeys.valueOf("DONE"), TouchActions.KeyboardKeys.DONE);
        Assert.assertEquals(TouchActions.KeyboardKeys.valueOf("SEARCH"), TouchActions.KeyboardKeys.SEARCH);
        Assert.assertEquals(TouchActions.KeyboardKeys.valueOf("SEND"), TouchActions.KeyboardKeys.SEND);
        Assert.assertEquals(TouchActions.KeyboardKeys.valueOf("NEXT"), TouchActions.KeyboardKeys.NEXT);
        Assert.assertEquals(TouchActions.KeyboardKeys.valueOf("PREVIOUS"), TouchActions.KeyboardKeys.PREVIOUS);
        Assert.assertEquals(TouchActions.KeyboardKeys.valueOf("NORMAL"), TouchActions.KeyboardKeys.NORMAL);
        Assert.assertEquals(TouchActions.KeyboardKeys.valueOf("UNSPECIFIED"), TouchActions.KeyboardKeys.UNSPECIFIED);
        Assert.assertEquals(TouchActions.KeyboardKeys.valueOf("NONE"), TouchActions.KeyboardKeys.NONE);
    }

    @Test
    public void touchActionsFluentAnd() {
        TouchActions actions = new TouchActions();
        Assert.assertSame(actions.and(), actions);
    }
}
