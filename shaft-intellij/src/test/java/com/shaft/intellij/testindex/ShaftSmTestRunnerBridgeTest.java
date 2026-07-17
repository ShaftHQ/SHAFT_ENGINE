package com.shaft.intellij.testindex;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * {@link ShaftSmTestRunnerBridge#parseIndexKey}/{@link ShaftSmTestRunnerBridge#finishedStatus} are
 * the node-identity mapping and status-transition logic issue #3688 asked to be independently
 * testable: both are pure static methods over plain strings/booleans, so they are exercised here
 * without ever constructing a live {@code SMTestProxy} (see the class javadoc's "Untestable in this
 * headless Gradle test JVM" note for why the {@code SMTRunnerEventsListener} overrides themselves
 * are not exercised end-to-end).
 */
class ShaftSmTestRunnerBridgeTest {
    // ------------------------------------------------------------------
    // parseIndexKey: java:suite:// (class-level rows)
    // ------------------------------------------------------------------

    @Test
    void parseIndexKeyReturnsQualifiedClassNameForASuiteUrl() {
        assertEquals("com.example.SignInTest",
                ShaftSmTestRunnerBridge.parseIndexKey("java:suite://com.example.SignInTest"));
    }

    @Test
    void parseIndexKeyReturnsNullForABlankSuiteQualifiedName() {
        assertNull(ShaftSmTestRunnerBridge.parseIndexKey("java:suite://"));
    }

    // ------------------------------------------------------------------
    // parseIndexKey: java:test:// (method-level rows)
    // ------------------------------------------------------------------

    @Test
    void parseIndexKeyReturnsClassHashMethodForATestUrl() {
        assertEquals("com.example.SignInTest#testSignIn",
                ShaftSmTestRunnerBridge.parseIndexKey("java:test://com.example.SignInTest/testSignIn"));
    }

    @Test
    void parseIndexKeyReturnsNullForATestUrlWithNoMethodSegment() {
        assertNull(ShaftSmTestRunnerBridge.parseIndexKey("java:test://com.example.SignInTest"));
    }

    @Test
    void parseIndexKeyReturnsNullForATestUrlWithATrailingSlashAndNoMethodName() {
        assertNull(ShaftSmTestRunnerBridge.parseIndexKey("java:test://com.example.SignInTest/"));
    }

    // ------------------------------------------------------------------
    // parseIndexKey: unrecognized/absent locations
    // ------------------------------------------------------------------

    @Test
    void parseIndexKeyReturnsNullForNull() {
        assertNull(ShaftSmTestRunnerBridge.parseIndexKey(null));
    }

    @Test
    void parseIndexKeyReturnsNullForAnUnrecognizedProtocol() {
        assertNull(ShaftSmTestRunnerBridge.parseIndexKey("python:test://some_module.SomeTest"));
    }

    @Test
    void parseIndexKeyReturnsNullForABlankString() {
        assertNull(ShaftSmTestRunnerBridge.parseIndexKey(""));
    }

    // ------------------------------------------------------------------
    // finishedStatus
    // ------------------------------------------------------------------

    @Test
    void finishedStatusIsFailWhenTheNodeIsADefect() {
        assertEquals(ShaftTestIndex.Status.FAIL, ShaftSmTestRunnerBridge.finishedStatus(true));
    }

    @Test
    void finishedStatusIsPassWhenTheNodeIsNotADefect() {
        assertEquals(ShaftTestIndex.Status.PASS, ShaftSmTestRunnerBridge.finishedStatus(false));
    }

    // ------------------------------------------------------------------
    // Sanity: the method-key separator matches ShaftTestsPanel's lookup convention
    // ------------------------------------------------------------------

    @Test
    void methodKeySeparatorIsAHashNotUsableInAJavaIdentifierOrPackageName() {
        // Guards against silently changing the separator to something that could collide with a
        // real class/method name and corrupt a lookup, without needing package-private access to
        // ShaftTestsPanel's own key-building helper.
        char separator = ShaftSmTestRunnerBridge.METHOD_KEY_SEPARATOR;
        assertEquals('#', separator);
        assertFalse(Character.isJavaIdentifierPart(separator));
    }
}
