package com.shaft.tools.io.internal;

import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

class ProjectStructureManagerTest {

    private String originalExecutionAddress;

    @BeforeMethod
    void overrideExecutionAddress() {
        originalExecutionAddress = System.getProperty("executionAddress");
        // "remote" skips all filesystem operations in ProjectStructureManager,
        // letting us test the dispatch logic without a real project directory.
        System.setProperty("executionAddress", "remote");
    }

    @AfterMethod
    void restoreExecutionAddress() {
        if (originalExecutionAddress == null) {
            System.clearProperty("executionAddress");
        } else {
            System.setProperty("executionAddress", originalExecutionAddress);
        }
    }

    @Test
    void shouldInitializeForTestNG() {
        ProjectStructureManager.initialize(ProjectStructureManager.RunType.TESTNG);
    }

    @Test
    void shouldInitializeForJUnit() {
        ProjectStructureManager.initialize(ProjectStructureManager.RunType.JUNIT);
    }

    @Test
    void shouldInitializeForCucumber() {
        ProjectStructureManager.initialize(ProjectStructureManager.RunType.CUCUMBER);
    }

    @Test
    void shouldInitializeForAiAgent() {
        ProjectStructureManager.initialize(ProjectStructureManager.RunType.AI_AGENT);
    }
}
