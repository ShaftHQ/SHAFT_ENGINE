package testPackage.unitTests;

import com.shaft.validation.Validations;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Unit tests for {@link com.shaft.validation.internal.FileValidationsBuilder}.
 * Tests file existence validations using the SHAFT assertions API.
 */
public class FileValidationsBuilderUnitTest {

    private static final String TEMP_FOLDER = "target/test-temp-files/";
    private static final Path TEMP_DIR = Paths.get(TEMP_FOLDER);

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "file exists: existing file should pass")
    public void fileExistsShouldPassForExistingFile() throws IOException {
        Files.createDirectories(TEMP_DIR);
        Path tempFile = TEMP_DIR.resolve("test-exists.txt");
        Files.writeString(tempFile, "test content");

        try {
            Validations.assertThat()
                    .file(TEMP_FOLDER, "test-exists.txt")
                    .exists()
                    .perform();
        } finally {
            Files.deleteIfExists(tempFile);
        }
    }

    @Test(description = "file exists: non-existing file should fail")
    public void fileExistsShouldFailForNonExistingFile() {
        try {
            Validations.assertThat()
                    .file(TEMP_FOLDER, "definitely-does-not-exist-abc123.txt")
                    .exists()
                    .perform();
            Assert.fail("Expected AssertionError for non-existing file");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage(), "AssertionError should carry a message");
        }
    }

    @Test(description = "file does not exist: non-existing file should pass")
    public void fileDoesNotExistShouldPassForNonExistingFile() {
        Validations.assertThat()
                .file(TEMP_FOLDER, "this-file-should-not-exist-xyz.txt")
                .doesNotExist()
                .perform();
    }

    @Test(description = "file does not exist: existing file should fail")
    public void fileDoesNotExistShouldFailForExistingFile() throws IOException {
        Files.createDirectories(TEMP_DIR);
        Path tempFile = TEMP_DIR.resolve("test-exists-for-negative.txt");
        Files.writeString(tempFile, "content");

        try {
            Validations.assertThat()
                    .file(TEMP_FOLDER, "test-exists-for-negative.txt")
                    .doesNotExist()
                    .perform();
            Assert.fail("Expected AssertionError for existing file in doesNotExist");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        } finally {
            Files.deleteIfExists(tempFile);
        }
    }

    @Test(description = "file content: text file content should match expected")
    public void fileContentShouldMatchExpectedText() throws IOException {
        Files.createDirectories(TEMP_DIR);
        Path tempFile = TEMP_DIR.resolve("test-content.txt");
        Files.writeString(tempFile, "expected text content");

        try {
            Validations.assertThat()
                    .file(TEMP_FOLDER, "test-content.txt")
                    .content()
                    .contains("expected text")
                    .perform();
        } finally {
            Files.deleteIfExists(tempFile);
        }
    }

    @Test(description = "file content: text file content should fail when not matching")
    public void fileContentShouldFailWhenNotMatching() throws IOException {
        Files.createDirectories(TEMP_DIR);
        Path tempFile = TEMP_DIR.resolve("test-content-mismatch.txt");
        Files.writeString(tempFile, "actual text content");

        try {
            Validations.assertThat()
                    .file(TEMP_FOLDER, "test-content-mismatch.txt")
                    .content()
                    .isEqualTo("completely different text")
                    .perform();
            Assert.fail("Expected AssertionError for mismatched file content");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        } finally {
            Files.deleteIfExists(tempFile);
        }
    }
}
