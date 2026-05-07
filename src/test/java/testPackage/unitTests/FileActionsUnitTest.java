package testPackage.unitTests;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.apache.commons.io.filefilter.TrueFileFilter;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

public class FileActionsUnitTest {
    private static final FileActions testFileActions = FileActions.getInstance(true);
    private static final Path TEMP_DIR = Path.of("target", "temp", "fileActionsUnitTests");

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        testFileActions.deleteFolder(TEMP_DIR.toString());
        Properties.clearForCurrentThread();
    }

    @Test
    public void renameReadWriteAndExistenceMethodsShouldWork() throws Exception {
        testFileActions.createFolder(TEMP_DIR.toString());
        String filePath = TEMP_DIR.resolve("source.txt").toString();
        testFileActions.writeToFile(filePath, "line-1");
        Assert.assertTrue(testFileActions.doesFileExist(filePath));

        testFileActions.renameFile(filePath, "renamed.txt");
        String renamedPath = TEMP_DIR.resolve("renamed.txt").toString();
        Assert.assertTrue(testFileActions.doesFileExist(renamedPath));
        Assert.assertEquals(testFileActions.readFile(renamedPath), "line-1");

        testFileActions.renameFile(new File(renamedPath).getAbsolutePath(), "renamed.txt");
        Assert.assertTrue(testFileActions.doesFileExist(TEMP_DIR.resolve("").toString() + File.separator, "renamed.txt", 1));

        byte[] bytes = "bytes-content".getBytes(StandardCharsets.UTF_8);
        testFileActions.writeToFile(TEMP_DIR.resolve("").toString() + File.separator, "binary.bin", bytes);
        Assert.assertEquals(testFileActions.readFileAsByteArray(TEMP_DIR.resolve("binary.bin").toString()), bytes);

        String absolute = testFileActions.getAbsolutePath(TEMP_DIR.toString() + "/", "renamed.txt");
        Assert.assertTrue(new File(absolute).isAbsolute());
    }

    @Test
    public void copyListAndArchiveMethodsShouldWork() throws Exception {
        testFileActions.createFolder(TEMP_DIR.toString());
        testFileActions.createFolder(TEMP_DIR.resolve("sub").toString());
        testFileActions.writeToFile(TEMP_DIR.resolve("sub/a.txt").toString(), "A");
        testFileActions.writeToFile(TEMP_DIR.resolve("sub/b.txt").toString(), "B");

        Path copied = TEMP_DIR.resolve("copied");
        testFileActions.copyFolder(TEMP_DIR.resolve("sub").toString(), copied.toString());
        Assert.assertTrue(testFileActions.doesFileExist(copied.resolve("a.txt").toString()));

        String recursiveList = testFileActions.listFilesInDirectory(TEMP_DIR.toString(), (TrueFileFilter) TrueFileFilter.TRUE);
        Assert.assertTrue(recursiveList.contains("a.txt"));

        String zipPath = TEMP_DIR.resolve("archive.zip").toString();
        Assert.assertTrue(testFileActions.zipFiles(copied.toString(), zipPath));
        URL zipUrl = new File(zipPath).toURI().toURL();
        File unpacked = testFileActions.unpackArchive(zipUrl, TEMP_DIR.resolve("unzipped").toString());
        Assert.assertNotNull(unpacked);
        Assert.assertTrue(testFileActions.doesFileExist(TEMP_DIR.resolve("unzipped/copied/a.txt").toString()));
    }

    @Test
    public void downloadFileShouldHandleValidAndNullInput() throws Exception {
        testFileActions.createFolder(TEMP_DIR.toString());
        String sourceFile = TEMP_DIR.resolve("downloadSource.txt").toString();
        testFileActions.writeToFile(sourceFile, "download-text");
        String destination = TEMP_DIR.resolve("downloaded.txt").toString();

        URL result = testFileActions.downloadFile(new File(sourceFile).toURI().toString(), destination, 5000, 5000);
        Assert.assertNotNull(result);
        Assert.assertEquals(Files.readString(Path.of(destination)), "download-text");

        Assert.assertThrows(Throwable.class, () -> testFileActions.downloadFile(null, destination));
    }

    @Test
    public void createAndDeleteFileShouldBeIdempotent() {
        testFileActions.createFile(TEMP_DIR.toString() + "/", "f.txt");
        testFileActions.createFile(TEMP_DIR.toString() + "/", "f.txt");
        Assert.assertTrue(testFileActions.doesFileExist(TEMP_DIR.resolve("f.txt").toString()));
        testFileActions.deleteFile(TEMP_DIR.resolve("f.txt").toString());
        testFileActions.deleteFile(TEMP_DIR.resolve("f.txt").toString());
        Assert.assertFalse(testFileActions.doesFileExist(TEMP_DIR.resolve("f.txt").toString()));
    }
}
