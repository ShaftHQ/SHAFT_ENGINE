package com.shaft.intellij.ui;

import com.intellij.testFramework.fixtures.BasePlatformTestCase;

import java.nio.file.Path;

/** Platform coverage for Allure notifier seam. */
public class AllureReportActionsPlatformTest extends BasePlatformTestCase {
    public void testNotifyEvidenceOrDoctorUsesProjectNotifier() {
        Path root = Path.of(getProject().getBasePath());
        AllureReportActions.notifyEvidenceOrDoctor(getProject(), root);
    }
}
