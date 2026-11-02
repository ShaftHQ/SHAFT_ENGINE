package com.shaft.validation.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

public class AssertionFailureFormatterUnitTest {
    @Test
    public void formatFailureWithStackTraceCoversNullBlankAndMatchingFrames() {
        Assert.assertNull(AssertionFailureFormatter.formatFailureWithStackTrace(null, "com.shaft"));
        Assert.assertNull(AssertionFailureFormatter.formatFailureWithStackTrace(new AssertionError("x"), null));

        AssertionError noMatch = new AssertionError("no package match");
        noMatch.setStackTrace(new StackTraceElement[]{
                new StackTraceElement("org.other.Thing", "run", "Thing.java", 12)
        });
        Assert.assertNull(AssertionFailureFormatter.formatFailureWithStackTrace(noMatch, "com.shaft"));

        AssertionError match = new AssertionError("expected visible");
        match.setStackTrace(new StackTraceElement[]{
                new StackTraceElement("com.shaft.tests.LoginTest", "shouldLogin", "LoginTest.java", 42),
                new StackTraceElement("org.junit.Runner", "run", "Runner.java", 1)
        });
        String formatted = AssertionFailureFormatter.formatFailureWithStackTrace(match, "com.shaft");
        Assert.assertNotNull(formatted);
        Assert.assertTrue(formatted.contains("42"), formatted);
        Assert.assertTrue(formatted.contains("expected visible"), formatted);
        Assert.assertTrue(formatted.contains("LoginTest.java"), formatted);

        AssertionError zeroLine = new AssertionError("zero");
        zeroLine.setStackTrace(new StackTraceElement[]{
                new StackTraceElement("com.shaft.tests.X", "m", "X.java", -1)
        });
        Assert.assertNull(AssertionFailureFormatter.formatFailureWithStackTrace(zeroLine, "com.shaft"));
    }

}
