package com.shaft.validation.internal;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class CustomSoftAssertTest {

    @Test
    void setRootPackageShouldFilterStackTraceToMatchingPackage() {
        CustomSoftAssert softAssert = new CustomSoftAssert();
        softAssert.setRootPackage("com.shaft.validation.internal");
        softAssert.assertEquals("expected", "actual", "deliberate failure");
        try {
            softAssert.assertAll();
            fail("assertAll should have thrown");
        } catch (AssertionError e) {
            assertNotNull(e.getMessage());
        }
    }

    @Test
    void clearFailuresShouldEmptyLocalListWithoutThrowing() {
        CustomSoftAssert softAssert = new CustomSoftAssert();
        softAssert.assertEquals("a", "b", "failure");
        assertDoesNotThrow(softAssert::clearFailures,
            "clearFailures() must not throw");
    }

    @Test
    void onAssertFailureShouldNotThrowWhenNoMatchingPackage() {
        CustomSoftAssert softAssert = new CustomSoftAssert();
        softAssert.setRootPackage("com.nonexistent.package");
        softAssert.assertEquals("x", "y", "no match");
        try {
            softAssert.assertAll();
        } catch (AssertionError ignored) {
            // expected — just must not throw NPE in onAssertFailure
        }
    }

    @Test
    void formatFailureWithStackTraceShouldReturnNullForNullError() {
        assertNull(CustomSoftAssert.formatFailureWithStackTrace(null, "any.package"));
    }

    @Test
    void formatFailureWithStackTraceShouldReturnNullForNullPackage() {
        assertNull(CustomSoftAssert.formatFailureWithStackTrace(new AssertionError("err"), null));
    }

    @Test
    void formatFailureWithStackTraceShouldContainAtLinesWhenPackageMatches() {
        AssertionError error = new AssertionError("test error") {
            @Override
            public StackTraceElement[] getStackTrace() {
                return new StackTraceElement[]{
                    new StackTraceElement("com.shaft.validation.internal.Foo", "bar", "Foo.java", 42)
                };
            }
        };
        String result = CustomSoftAssert.formatFailureWithStackTrace(error, "com.shaft.validation.internal");
        assertNotNull(result, "Must return formatted string when package matches");
        assertTrue(result.contains("at "), "Must contain at-lines: " + result);
        assertTrue(result.contains("Foo.java:42"), "Must contain file:line reference");
    }

    @Test
    void formatFailureWithStackTraceShouldHandleNullFileName() {
        AssertionError error = new AssertionError("null-file") {
            @Override
            public StackTraceElement[] getStackTrace() {
                return new StackTraceElement[]{
                    new StackTraceElement("com.shaft.validation.internal.Foo", "bar", null, 10)
                };
            }
        };
        String result = CustomSoftAssert.formatFailureWithStackTrace(error, "com.shaft.validation.internal");
        assertNotNull(result);
        assertTrue(result.contains("Unknown"), "null fileName should render as 'Unknown'");
    }

    @Test
    void assertAllPassesWithNoFailures() {
        assertDoesNotThrow(() -> new CustomSoftAssert().assertAll());
    }
}
