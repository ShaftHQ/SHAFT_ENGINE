package com.shaft.intellij.properties;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPropertiesProfileCardTest {
    @Test
    void masksSecretsAndReportsSource(@TempDir Path root) throws Exception {
        Path resources = root.resolve("src/main/resources");
        Files.createDirectories(resources);
        Files.writeString(resources.resolve("custom.properties"), "headlessExecution=true\n");
        Files.writeString(resources.resolve("secrets.properties"), "cloud.accessKey=super-secret-value\n");
        Map<String, ShaftPropertiesProfileCard.EffectiveProperty> effective =
                ShaftPropertiesProfileCard.loadEffective(root);
        assertEquals("true", effective.get("headlessExecution").displayValue());
        assertEquals("custom.properties", effective.get("headlessExecution").source());
        assertTrue(effective.get("cloud.accessKey").secret());
        assertTrue(effective.get("cloud.accessKey").displayValue().contains("****"));
    }
}
