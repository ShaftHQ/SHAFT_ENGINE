package com.shaft.intellij.ui;

import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #5976 / S3-10: unified Reporting canvas composes Allure/Doctor/flake/heal/summaries
 * as one Overview rather than five primary tabs.
 */
class ReportingStagePanelTest {
    @Test
    void overviewComposesAllureDoctorFlakeHealAndSummaries() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        ReportingStagePanel panel = new ReportingStagePanel(null, (tool, args) -> {
        }, settings);

        assertAll(
                () -> assertEquals(ReportingStagePanel.ACCESSIBLE_NAME,
                        panel.getAccessibleContext().getAccessibleName()),
                () -> assertEquals(ReportingStagePanel.OVERVIEW_TAB, panel.surfaces().getTitleAt(0)),
                () -> assertNotNull(panel.openAllureButton()),
                () -> assertNotNull(panel.generateReportButton()),
                () -> assertNotNull(panel.doctorPanel()),
                () -> assertNotNull(panel.flakePanel()),
                () -> assertNotNull(panel.healPanel()),
                () -> assertNotNull(panel.engineerSummaryArea()),
                () -> assertNotNull(panel.stakeholderSummaryArea()),
                () -> assertTrue(panel.allureStatusLabel().getText().toLowerCase().contains("allure")));
    }

    @Test
    void secondaryTabsKeepHistoryWithoutFivePrimaryReportingTabs() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpSetupComplete = true;
        ReportingStagePanel panel = new ReportingStagePanel(null, (tool, args) -> {
        }, settings);

        int tabs = panel.surfaces().getTabCount();
        assertAll(
                () -> assertTrue(tabs >= 6, "Overview plus secondary history/tags/clusters/labels/triage/visual/evidence"),
                () -> assertEquals(ReportingStagePanel.OVERVIEW_TAB, panel.surfaces().getTitleAt(0)),
                () -> assertFalse(containsTitle(panel, ReportingDoctorPanel.TAB_TITLE),
                        "Doctor is composed on Overview, not a sibling primary tab"),
                () -> assertFalse(containsTitle(panel, ReportingFlakePanel.TAB_TITLE),
                        "Flake is composed on Overview, not a sibling primary tab"),
                () -> assertFalse(containsTitle(panel, ReportingHealPanel.TAB_TITLE),
                        "Heal is composed on Overview, not a sibling primary tab"),
                () -> assertTrue(containsTitle(panel, ReportingHistoryPanel.TAB_TITLE)),
                () -> assertTrue(containsTitle(panel, "Triage")),
                () -> assertTrue(containsTitle(panel, "Evidence")));
    }

    private static boolean containsTitle(ReportingStagePanel panel, String title) {
        for (int index = 0; index < panel.surfaces().getTabCount(); index++) {
            if (title.equals(panel.surfaces().getTitleAt(index))) {
                return true;
            }
        }
        return false;
    }
}
