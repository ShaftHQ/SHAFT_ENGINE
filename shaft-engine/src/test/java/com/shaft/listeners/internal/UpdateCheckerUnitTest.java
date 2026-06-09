package com.shaft.listeners.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

public class UpdateCheckerUnitTest {
    @Test
    public void newerReleaseCandidateShouldNotBeReportedAsOutdated() {
        Assert.assertFalse(UpdateChecker.isCurrentVersionOutdated("10.2.20260609", "10.2.20260605"));
    }

    @Test
    public void olderReleaseShouldBeReportedAsOutdated() {
        Assert.assertTrue(UpdateChecker.isCurrentVersionOutdated("10.2.20260601", "10.2.20260605"));
    }

    @Test
    public void matchingReleaseShouldNotBeReportedAsOutdated() {
        Assert.assertFalse(UpdateChecker.isCurrentVersionOutdated("10.2.20260605", "10.2.20260605"));
    }
}
