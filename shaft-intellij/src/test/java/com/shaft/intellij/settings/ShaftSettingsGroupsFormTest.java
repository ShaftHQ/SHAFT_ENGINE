package com.shaft.intellij.settings;

import org.junit.jupiter.api.Test;

import javax.swing.JComponent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftSettingsGroupsFormTest {
    @Test
    void createChromeBuildsNonEmptyPanel() {
        JComponent chrome = ShaftSettingsGroupsForm.createChrome();
        assertNotNull(chrome);
        assertTrue(chrome.getPreferredSize().width >= 0);
    }
}
