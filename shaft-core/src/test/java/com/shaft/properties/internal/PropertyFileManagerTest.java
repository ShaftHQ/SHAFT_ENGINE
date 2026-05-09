package com.shaft.properties.internal;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.FileOutputStream;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertTrue;

class PropertyFileManagerTest {

    @TempDir
    Path tempDir;

    @Test
    void fileShouldBeDeleteableAfterPropertiesLoad() throws Exception {
        File propFile = tempDir.resolve("test.properties").toFile();
        try (FileOutputStream fos = new FileOutputStream(propFile)) {
            fos.write("key=value\n".getBytes());
        }
        assertTrue(propFile.exists(), "Temp file must exist before load");

        // Invoke private loadPropertiesFileIntoSystemProperties to simulate real usage
        Method m = PropertyFileManager.class.getDeclaredMethod(
            "loadPropertiesFileIntoSystemProperties", Properties.class, File.class);
        m.setAccessible(true);
        m.invoke(null, new Properties(), propFile);

        // The try-with-resources in loadPropertiesFileIntoSystemProperties must have
        // closed the FileInputStream — file must now be deleteable on Windows.
        assertTrue(propFile.delete(),
            "File must be deleteable after loading — try-with-resources must have released the lock");
    }
}
