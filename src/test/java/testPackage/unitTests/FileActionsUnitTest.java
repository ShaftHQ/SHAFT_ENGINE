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
    private static final FileActions fileActions = FileActions.getInstance(true);
    private static final Path TEMP_DIR = Path.of("target", "temp", "fileActionsUnitTests");

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        fileActions.deleteFolder(TEMP_DIR.toString());
        Properties.clearForCurrentThread();
    }

    @Test
    public void renameReadWriteAndExistenceMethodsShouldWork() throws Exception {
        fileActions.createFolder(TEMP_DIR.toString());
        String filePath = TEMP_DIR.resolve("source.txt").toString();
        fileActions.writeToFile(filePath, "line-1");
        Assert.assertTrue(fileActions.doesFileExist(filePath));

        fileActions.renameFile(filePath, "renamed.txt");
        String renamedPath = TEMP_DIR.resolve("renamed.txt").toString();
        Assert.assertTrue(fileActions.doesFileExist(renamedPath));
        Assert.assertEquals(fileActions.readFile(renamedPath), "line-1");

        fileActions.renameFile(renamedPath, "renamed.txt");
        Assert.assertTrue(fileActions.doesFileExist(TEMP_DIR.toString() + "/", "renamed.txt", 1));

        byte[] bytes = "bytes-content".getBytes(StandardCharsets.UTF_8);
        fileActions.writeToFile(TEMP_DIR.toString() + "/", "binary.bin", bytes);
        Assert.assertEquals(fileActions.readFileAsByteArray(TEMP_DIR.resolve("binary.bin").toString()), bytes);

        String absolute = fileActions.getAbsolutePath(TEMP_DIR.toString() + "/", "renamed.txt");
        Assert.assertTrue(new File(absolute).isAbsolute());
    }

    @Test
    public void copyListAndArchiveMethodsShouldWork() throws Exception {
        fileActions.createFolder(TEMP_DIR.toString());
        fileActions.createFolder(TEMP_DIR.resolve("sub").toString());
        fileActions.writeToFile(TEMP_DIR.resolve("sub/a.txt").toString(), "A");
        fileActions.writeToFile(TEMP_DIR.resolve("sub/b.txt").toString(), "B");

        Path copied = TEMP_DIR.resolve("copied");
        fileActions.copyFolder(TEMP_DIR.resolve("sub").toString(), copied.toString());
        Assert.assertTrue(fileActions.doesFileExist(copied.resolve("a.txt").toString()));

        String recursiveList = fileActions.listFilesInDirectory(TEMP_DIR.toString(), (TrueFileFilter) TrueFileFilter.TRUE);
        Assert.assertTrue(recursiveList.contains("a.txt"));

        String zipPath = TEMP_DIR.resolve("archive.zip").toString();
        Assert.assertTrue(fileActions.zipFiles(copied.toString(), zipPath));
        URL zipUrl = new File(zipPath).toURI().toURL();
        File unpacked = fileActions.unpackArchive(zipUrl, TEMP_DIR.resolve("unzipped").toString());
        Assert.assertNotNull(unpacked);
        Assert.assertTrue(fileActions.doesFileExist(TEMP_DIR.resolve("unzipped/copied/a.txt").toString()));
    }

    @Test
    public void downloadFileShouldHandleValidAndNullInput() throws Exception {
        fileActions.createFolder(TEMP_DIR.toString());
        String sourceFile = TEMP_DIR.resolve("downloadSource.txt").toString();
        fileActions.writeToFile(sourceFile, "download-text");
        String destination = TEMP_DIR.resolve("downloaded.txt").toString();

        URL result = fileActions.downloadFile(new File(sourceFile).toURI().toString(), destination, 5000, 5000);
        Assert.assertNotNull(result);
        Assert.assertEquals(Files.readString(Path.of(destination)), "download-text");

        Assert.assertThrows(Throwable.class, () -> fileActions.downloadFile(null, destination));
    }

    @Test
    public void createAndDeleteFileShouldBeIdempotent() {
        fileActions.createFile(TEMP_DIR.toString() + "/", "f.txt");
        fileActions.createFile(TEMP_DIR.toString() + "/", "f.txt");
        Assert.assertTrue(fileActions.doesFileExist(TEMP_DIR.resolve("f.txt").toString()));
        fileActions.deleteFile(TEMP_DIR.resolve("f.txt").toString());
        fileActions.deleteFile(TEMP_DIR.resolve("f.txt").toString());
        Assert.assertFalse(fileActions.doesFileExist(TEMP_DIR.resolve("f.txt").toString()));
    }
}
