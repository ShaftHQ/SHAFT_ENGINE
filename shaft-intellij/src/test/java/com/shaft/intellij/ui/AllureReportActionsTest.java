package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class AllureReportActionsTest {
    @Test
    void findsAllureResultsDirectory(@TempDir Path root) throws Exception {
        assertNull(AllureReportActions.findAllureResults(root));
        Path results = root.resolve("target/allure-results");
        Files.createDirectories(results);
        assertEquals(results, AllureReportActions.findAllureResults(root));
    }
}
