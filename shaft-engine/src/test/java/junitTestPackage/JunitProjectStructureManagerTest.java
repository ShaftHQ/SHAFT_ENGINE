package junitTestPackage;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.ProjectStructureManager;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class JunitProjectStructureManagerTest {
    private Path tempServicesDirPath;
    private Path tempResourcesDirPath;
    private String savedExecutionAddress;
    private String savedServicesPath;

    @AfterEach
    void cleanup() throws IOException {
        if (savedExecutionAddress != null) {
            SHAFT.Properties.platform.set().executionAddress(savedExecutionAddress);
        }
        if (savedServicesPath != null) {
            SHAFT.Properties.paths.set().services(savedServicesPath);
        }
        deleteDirectory(tempServicesDirPath);
        deleteDirectory(tempResourcesDirPath);
    }

    @Test
    void generatesJunitListenerExtensionAndPreservesJunitPlatformProperties() throws IOException {
        savedExecutionAddress = SHAFT.Properties.platform.executionAddress();
        savedServicesPath = SHAFT.Properties.paths.services();

        tempResourcesDirPath = Files.createTempDirectory("shaft-junit-resources");
        tempServicesDirPath = tempResourcesDirPath.resolve("META-INF").resolve("services");
        SHAFT.Properties.platform.set().executionAddress("local");
        SHAFT.Properties.paths.set().services(tempServicesDirPath.toString() + File.separator);
        Path junitProperties = tempResourcesDirPath.resolve("junit-platform.properties");
        Files.createDirectories(tempResourcesDirPath);
        Files.writeString(junitProperties, "junit.jupiter.execution.parallel.enabled=false" + System.lineSeparator());

        ProjectStructureManager.initialize(ProjectStructureManager.RunType.JUNIT);

        Path launcherListenerFile = tempServicesDirPath.resolve("org.junit.platform.launcher.LauncherSessionListener");
        Path extensionFile = tempServicesDirPath.resolve("org.junit.jupiter.api.extension.Extension");
        String junitPropertiesContent = Files.readString(junitProperties);

        assertEquals("com.shaft.listeners.JunitListener", Files.readString(launcherListenerFile).trim());
        assertEquals("com.shaft.listeners.JunitExtension", Files.readString(extensionFile).trim());
        assertTrue(junitPropertiesContent.contains("junit.jupiter.execution.parallel.enabled=false"));
        assertTrue(junitPropertiesContent.contains("junit.jupiter.extensions.autodetection.enabled=true"));
    }

    private static void deleteDirectory(Path directory) throws IOException {
        if (directory == null || !Files.exists(directory)) {
            return;
        }
        try (var paths = Files.walk(directory)) {
            paths.sorted(Comparator.reverseOrder()).forEach(path -> {
                try {
                    Files.deleteIfExists(path);
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
            });
        }
    }
}
