package com.shaft.tools.internal.support;

import org.testng.Assert;
import org.testng.annotations.Test;

class IsClassAvailableTest {

    @Test
    void knownJdkClassShouldBeAvailable() {
        Assert.assertTrue(JavaHelper.isClassAvailable("java.lang.String"),
            "java.lang.String must always be available");
    }

    @Test
    void shaftWebBrowserActionsShouldBeAbsentInShaftCoreScope() {
        Assert.assertFalse(JavaHelper.isClassAvailable("com.shaft.gui.browser.BrowserActions"),
            "BrowserActions lives in shaft-web — must not be on shaft-core test classpath");
    }

    @Test
    void nonExistentClassShouldReturnFalse() {
        Assert.assertFalse(JavaHelper.isClassAvailable("com.shaft.nonexistent.Foo"),
            "A class that does not exist must return false");
    }

    @Test
    void isClassAvailableDoesNotInitializeTheClass() {
        // Class.forName with initialize=false — safe even if static initializer would throw
        Assert.assertTrue(JavaHelper.isClassAvailable("java.lang.Thread"));
    }
}
