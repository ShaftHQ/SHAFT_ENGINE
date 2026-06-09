package testPackage.unitTests;

import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Unit tests for file operations
 * Tests file creation, reading, and validation
 */
public class FileHelperUnitTest {

    private String testFilePath;
    private String testContent;

    @BeforeClass
    public void setUp() throws IOException {
        testFilePath = "target/test-files/testFile.txt";
        testContent = "Test content for unit testing";
        
        // Create test file
        File testFile = new File(testFilePath);
        testFile.getParentFile().mkdirs();
        
        try (FileWriter writer = new FileWriter(testFile)) {
            writer.write(testContent);
        }
    }

    @Test(description = "Test file exists validation")
    public void testFileExists() {
        File file = new File(testFilePath);
        Assert.assertTrue(file.exists(), "Test file should exist");
    }

    @Test(description = "Test file is file validation")
    public void testIsFile() {
        File file = new File(testFilePath);
        Assert.assertTrue(file.isFile(), "Should be a file");
    }

    @Test(description = "Test file can read")
    public void testFileCanRead() {
        File file = new File(testFilePath);
        Assert.assertTrue(file.canRead(), "File should be readable");
    }

    @Test(description = "Test file content reading")
    public void testFileContentReading() throws IOException {
        String content = new String(Files.readAllBytes(Paths.get(testFilePath)));
        Assert.assertEquals(content, testContent, "File content should match");
    }

    @Test(description = "Test file size validation")
    public void testFileSize() {
        File file = new File(testFilePath);
        Assert.assertTrue(file.length() > 0, "File size should be greater than 0");
    }

    @Test(description = "Test directory creation")
    public void testDirectoryCreation() {
        String dirPath = "target/test-files/testDir";
        File dir = new File(dirPath);
        dir.mkdirs();
        Assert.assertTrue(dir.exists(), "Directory should exist");
        Assert.assertTrue(dir.isDirectory(), "Should be a directory");
    }

    @Test(description = "Test file path validation")
    public void testFilePathValidation() {
        File file = new File(testFilePath);
        Assert.assertTrue(file.getPath().contains("testFile.txt"), 
            "File path should contain filename");
    }

    @Test(description = "Test file absolute path")
    public void testFileAbsolutePath() {
        File file = new File(testFilePath);
        String absolutePath = file.getAbsolutePath();
        Assert.assertNotNull(absolutePath, "Absolute path should not be null");
        Assert.assertTrue(absolutePath.endsWith("testFile.txt"), 
            "Absolute path should end with filename");
    }

    @Test(description = "Test file parent directory")
    public void testFileParentDirectory() {
        File file = new File(testFilePath);
        File parent = file.getParentFile();
        Assert.assertNotNull(parent, "Parent directory should not be null");
        Assert.assertTrue(parent.exists(), "Parent directory should exist");
    }

    @Test(description = "Test file name extraction")
    public void testFileNameExtraction() {
        File file = new File(testFilePath);
        String fileName = file.getName();
        Assert.assertEquals(fileName, "testFile.txt", "File name should match");
    }

    @AfterClass
    public void tearDown() {
        // Clean up test file
        File file = new File(testFilePath);
        if (file.exists()) {
            file.delete();
        }
    }
}
