package testPackage.coverage;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.File;

/**
 * Tests to verify that FileActions.createFile does not destructively delete
 * an existing file, preventing race conditions during parallel execution.
 */
public class CreateFileRaceConditionTests {

    private static final String TEMP_FOLDER = "target/temp/createFileTest/";
    private static final String TEST_FILE = "testfile.txt";

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        File file = new File(TEMP_FOLDER + TEST_FILE);
        if (file.exists()) {
            file.delete();
        }
        File folder = new File(TEMP_FOLDER);
        if (folder.exists()) {
            folder.delete();
        }
    }

    @Test
    public void createFileDoesNotDeleteExistingFile() {
        var fileActions = FileActions.getInstance(true);

        // Create the file and write content
        fileActions.createFile(TEMP_FOLDER, TEST_FILE);
        fileActions.writeToFile(TEMP_FOLDER, TEST_FILE, "existing content");

        // Verify content was written
        String content = fileActions.readFile(TEMP_FOLDER, TEST_FILE);
        SHAFT.Validations.assertThat().object(content).contains("existing content").perform();

        // Call createFile again - it should NOT delete the existing file
        fileActions.createFile(TEMP_FOLDER, TEST_FILE);

        // Verify the file still exists and content is preserved
        SHAFT.Validations.assertThat().file(TEMP_FOLDER, TEST_FILE).exists().perform();
        String contentAfter = fileActions.readFile(TEMP_FOLDER, TEST_FILE);
        SHAFT.Validations.assertThat().object(contentAfter).contains("existing content").perform();
    }

    @Test
    public void createFileCreatesNewFileWhenNotExists() {
        var fileActions = FileActions.getInstance(true);

        // Ensure file does not exist
        File file = new File(TEMP_FOLDER + TEST_FILE);
        SHAFT.Validations.assertThat().object(file.exists()).isFalse().perform();

        // Create the file
        fileActions.createFile(TEMP_FOLDER, TEST_FILE);

        // Verify the file was created
        SHAFT.Validations.assertThat().file(TEMP_FOLDER, TEST_FILE).exists().perform();
    }
}
