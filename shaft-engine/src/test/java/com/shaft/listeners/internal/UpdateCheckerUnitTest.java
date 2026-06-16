package com.shaft.listeners.internal;

import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;

public class UpdateCheckerUnitTest {
    @Test
    public void unavailableGitHubShouldOnlyLogAtDebugLevel() {
        RuntimeException lookupFailure = new RuntimeException("GitHub unavailable");

        try (MockedStatic<ReportManager> report = Mockito.mockStatic(ReportManager.class);
             MockedStatic<ReportManagerHelper> reportManager = Mockito.mockStatic(ReportManagerHelper.class)) {
            UpdateChecker.check(() -> {
                throw lookupFailure;
            });

            report.verify(() -> ReportManager.logDiscrete("Checking for SHAFT engine updates."));
            report.verifyNoMoreInteractions();
            reportManager.verify(() -> ReportManagerHelper.logDiscrete(
                    "Engine update check was not completed because GitHub is unavailable.", Level.DEBUG));
            reportManager.verify(() -> ReportManagerHelper.logDiscrete(lookupFailure, Level.DEBUG));
            reportManager.verifyNoMoreInteractions();
        }
    }

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
