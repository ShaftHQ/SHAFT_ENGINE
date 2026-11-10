package com.shaft.tools.io;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.regex.Pattern;

/**
 * Whole-token classified failure ids (#7899).
 */
public class ClassifiedFailureIdTest {

    @Test
    public void acceptsDefaultAndDocumentedExtensionTokens() {
        Assert.assertTrue(ClassifiedFailureId.accepts("login failed DEF-42"));
        Assert.assertTrue(ClassifiedFailureId.accepts("locator gap G14"));
        Assert.assertTrue(ClassifiedFailureId.accepts("tracked as SHAFT#5812"));
        Assert.assertTrue(ClassifiedFailureId.accepts("cloud failure BS-4412"));
        Assert.assertTrue(ClassifiedFailureId.accepts("device ADB-OFFLINE"));
        Assert.assertTrue(ClassifiedFailureId.accepts("product block FUNC-J01-BUG-03"));
        Assert.assertTrue(ClassifiedFailureId.accepts("opened J01-BUG-03"));
    }

    @Test
    public void rejectsHostnameUnclassifiedAndNeverStartedSessions() {
        Assert.assertFalse(ClassifiedFailureId.accepts("Zephyrus-G14-GA401"));
        Assert.assertFalse(ClassifiedFailureId.accepts("skipped because the screen never loaded"));
        Assert.assertFalse(ClassifiedFailureId.accepts("BS-QUOTA"));
        Assert.assertFalse(ClassifiedFailureId.accepts("BS-QUOTA while naming DEF-9"));
        Assert.assertFalse(ClassifiedFailureId.accepts("SessionNotCreated: DEF-9 BS-4412"));
    }

    @Test
    public void additionalPatternExtendsTheContractWithoutMatchingInsideAToken() {
        Pattern ticket = Pattern.compile("(?<![A-Za-z0-9])TICKET-\\d+(?![A-Za-z0-9])");
        Assert.assertTrue(ClassifiedFailureId.accepts("see TICKET-15", ticket));
        Assert.assertFalse(ClassifiedFailureId.accepts("see TICKET-15"));
        Assert.assertFalse(ClassifiedFailureId.accepts("Zephyrus-G14-GA401", ticket));
        Assert.assertFalse(ClassifiedFailureId.accepts("SessionNotCreated TICKET-15", ticket));
    }

    @Test
    public void rejectsNullAndBlank() {
        Assert.assertFalse(ClassifiedFailureId.accepts(null));
        Assert.assertFalse(ClassifiedFailureId.accepts("  "));
    }
}
