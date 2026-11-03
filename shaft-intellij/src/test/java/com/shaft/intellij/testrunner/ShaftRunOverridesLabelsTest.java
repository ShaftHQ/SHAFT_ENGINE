package com.shaft.intellij.testrunner;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ShaftRunOverridesLabelsTest {
    @Test
    void projectDefaultLabelMatchesConstant() {
        assertEquals("Project default", ShaftRunOverridesLabels.PROJECT_DEFAULT);
        assertEquals(ShaftRunOverridesLabels.PROJECT_DEFAULT, ShaftRunOverridesLabels.projectDefaultLabel());
    }
}
