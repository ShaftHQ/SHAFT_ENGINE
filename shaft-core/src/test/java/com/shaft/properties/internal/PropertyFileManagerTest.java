package com.shaft.properties.internal;

import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.Properties;

class PropertyFileManagerTest {

    private Path tempDir;

    @BeforeMethod
    void createTempDir() throws IOException {
        tempDir = Files.createTempDirectory("shaft-core-test-");
    }

    @AfterMethod
    void deleteTempDir() throws IOException {
        if (tempDir != null) {
            try (var stream = Files.walk(tempDir)) {
                stream.sorted(Comparator.reverseOrder())
                      .map(Path::toFile)
                      .forEach(File::delete);
            }
        }
    }

    @Test
    void fileShouldBeDeleteableAfterPropertiesLoad() throws Exception {
        File propFile = tempDir.resolve("test.properties").toFile();
        try (FileOutputStream fos = new FileOutputStream(propFile)) {
            fos.write("key=value\n".getBytes());
        }
        Assert.assertTrue(propFile.exists(), "Temp file must exist before load");

        PropertyFileManager.loadPropertiesFileIntoSystemProperties(new Properties(), propFile);

        Assert.assertTrue(propFile.delete(),
            "File must be deleteable after loading — try-with-resources must have released the lock");
    }
}
