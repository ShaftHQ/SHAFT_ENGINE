package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PlaywrightDriverExtractorTest {
    @Test
    void extractsOnlyTheVersionMatchedDriverUsingChildEnvironment(@TempDir Path temp) throws Exception {
        Path node = Files.writeString(temp.resolve("node.exe"), "owned node");
        Path destination = temp.resolve("driver");
        String propertyBefore = System.getProperty("playwright.nodejs.path");

        PlaywrightDriverExtractor.extract(node, destination);

        assertTrue(Files.isRegularFile(destination.resolve("package/cli.js")));
        String browsers = Files.readString(destination.resolve("package/browsers.json"));
        assertTrue(browsers.contains("\"revision\": \"1234\""));
        assertTrue(browsers.contains("\"revision\": \"1538\""));
        assertTrue(browsers.contains("\"revision\": \"2336\""));
        assertFalse(Files.exists(destination.resolve("node.exe")));
        assertEquals(propertyBefore, System.getProperty("playwright.nodejs.path"));
    }

    @Test
    void rejectsALinkedNodeBeforeExtracting(@TempDir Path temp) throws Exception {
        Path external = Files.writeString(temp.resolve("external.exe"), "external");
        Path linked = temp.resolve("linked.exe");
        try {
            Files.createSymbolicLink(linked, external);
        } catch (UnsupportedOperationException | java.io.IOException unsupported) {
            org.junit.jupiter.api.Assumptions.abort("Symbolic links unavailable: " + unsupported.getMessage());
        }

        assertThrows(java.io.IOException.class,
                () -> PlaywrightDriverExtractor.extract(linked, temp.resolve("driver")));
        assertTrue(Files.notExists(temp.resolve("driver")));
    }
}
