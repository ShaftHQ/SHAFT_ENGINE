package com.shaft.tools.io.internal;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

class ProjectStructureManagerTest {

    private String originalExecutionAddress;

    @BeforeEach
    void overrideExecutionAddress() {
        originalExecutionAddress = System.getProperty("executionAddress");
        // "remote" skips all filesystem operations in ProjectStructureManager,
        // letting us test the dispatch logic without a real project directory.
        System.setProperty("executionAddress", "remote");
    }

    @AfterEach
    void restoreExecutionAddress() {
        if (originalExecutionAddress == null) {
            System.clearProperty("executionAddress");
        } else {
            System.setProperty("executionAddress", originalExecutionAddress);
        }
    }

    @Test
    void shouldInitializeForTestNG() {
        assertDoesNotThrow(() -> ProjectStructureManager.initialize(ProjectStructureManager.RunType.TESTNG));
    }

    @Test
    void shouldInitializeForJUnit() {
        assertDoesNotThrow(() -> ProjectStructureManager.initialize(ProjectStructureManager.RunType.JUNIT));
    }

    @Test
    void shouldInitializeForCucumber() {
        assertDoesNotThrow(() -> ProjectStructureManager.initialize(ProjectStructureManager.RunType.CUCUMBER));
    }

    @Test
    void shouldInitializeForAiAgent() {
        assertDoesNotThrow(() -> ProjectStructureManager.initialize(ProjectStructureManager.RunType.AI_AGENT));
    }
}
