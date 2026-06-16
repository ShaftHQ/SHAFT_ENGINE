package com.shaft.mcp;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class EngineServiceRuntimePathTest {
    private static final List<String> RUNTIME_PATH_PROPERTIES = List.of(
            "aiAgentWorkspaceRoot",
            "allureResultsFolderPath",
            "propertiesFolderPath",
            "downloadsFolderPath",
            "video.folder",
            "servicesFolderPath",
            "dynamicObjectRepositoryPath",
            "testDataFolderPath",
            "extentReportsFolderPath",
            "executionSummaryReportFolderPath",
            "PerformanceReportFolderPath");

    @TempDir
    Path temp;

    private Map<String, String> originalProperties;

    @BeforeEach
    void captureOriginalProperties() {
        originalProperties = new HashMap<>();
        RUNTIME_PATH_PROPERTIES.forEach(property -> originalProperties.put(property, System.getProperty(property)));
        RUNTIME_PATH_PROPERTIES.forEach(System::clearProperty);
    }

    @AfterEach
    void restoreOriginalProperties() {
        RUNTIME_PATH_PROPERTIES.forEach(System::clearProperty);
        originalProperties.forEach((property, value) -> {
            if (value != null) {
                System.setProperty(property, value);
            }
        });
    }

    @Test
    void configureRuntimePathsShouldResolveRelativePropertiesUnderRuntimeRoot() {
        Path runtimeRoot = temp.resolve("runtime-root");
        System.setProperty("allureResultsFolderPath", "custom-results");
        System.setProperty("downloadsFolderPath", "downloads");

        Path resolvedRoot = EngineService.configureRuntimePaths(runtimeRoot);

        assertEquals(runtimeRoot.toAbsolutePath().normalize(), resolvedRoot);
        assertEquals(resolvedRoot.toString(), System.getProperty("aiAgentWorkspaceRoot"));
        assertEquals(resolvedRoot.resolve("custom-results").toString(),
                System.getProperty("allureResultsFolderPath"));
        assertEquals(resolvedRoot.resolve("downloads").toString(),
                System.getProperty("downloadsFolderPath"));
        assertEquals(resolvedRoot.resolve("src/main/resources/properties").toString(),
                System.getProperty("propertiesFolderPath"));
        assertTrue(Files.isDirectory(resolvedRoot.resolve("custom-results")));
        assertTrue(Files.isDirectory(resolvedRoot.resolve("downloads")));
        assertTrue(Files.isDirectory(resolvedRoot.resolve("src/main/resources/properties")));
    }

    @Test
    void configureRuntimePathsShouldPreserveExplicitAbsoluteOverrides() {
        Path runtimeRoot = temp.resolve("runtime-root");
        Path absoluteResults = temp.resolve("absolute-results");
        Path absoluteServices = temp.resolve("absolute-services");
        System.setProperty("allureResultsFolderPath", absoluteResults.toString());
        System.setProperty("servicesFolderPath", absoluteServices.toString());

        EngineService.configureRuntimePaths(runtimeRoot);

        assertEquals(absoluteResults.toAbsolutePath().normalize().toString(),
                System.getProperty("allureResultsFolderPath"));
        assertEquals(absoluteServices.toAbsolutePath().normalize().toString(),
                System.getProperty("servicesFolderPath"));
        assertTrue(Files.isDirectory(absoluteResults));
        assertTrue(Files.isDirectory(absoluteServices));
    }
}
