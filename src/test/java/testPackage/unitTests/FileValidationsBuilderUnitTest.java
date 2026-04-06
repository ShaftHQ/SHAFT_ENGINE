package testPackage.unitTests;

import com.shaft.validation.Validations;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

/**
 * Unit tests for {@link com.shaft.validation.internal.FileValidationsBuilder}.
 * Tests file existence validations using the SHAFT assertions API.
 */
public class FileValidationsBuilderUnitTest {

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "file exists: existing file should pass")
    public void fileExistsShouldPassForExistingFile() throws IOException {
        // Create a temporary file for the test
        File tempDir = new File(System.getProperty("user.dir") + "/target/test-temp-files/");
        tempDir.mkdirs();
        File tempFile = new File(tempDir, "test-exists.txt");
        Files.writeString(tempFile.toPath(), "test content");

        try {
            Validations.assertThat()
                    .file("target/test-temp-files/", "test-exists.txt")
                    .exists()
                    .perform();
        } finally {
            tempFile.delete();
        }
    }

    @Test(description = "file exists: non-existing file should fail")
    public void fileExistsShouldFailForNonExistingFile() {
        try {
            Validations.assertThat()
                    .file("target/test-temp-files/", "definitely-does-not-exist-abc123.txt")
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
                .file("target/test-temp-files/", "this-file-should-not-exist-xyz.txt")
                .doesNotExist()
                .perform();
    }

    @Test(description = "file does not exist: existing file should fail")
    public void fileDoesNotExistShouldFailForExistingFile() throws IOException {
        File tempDir = new File(System.getProperty("user.dir") + "/target/test-temp-files/");
        tempDir.mkdirs();
        File tempFile = new File(tempDir, "test-exists-for-negative.txt");
        Files.writeString(tempFile.toPath(), "content");

        try {
            Validations.assertThat()
                    .file("target/test-temp-files/", "test-exists-for-negative.txt")
                    .doesNotExist()
                    .perform();
            Assert.fail("Expected AssertionError for existing file in doesNotExist");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        } finally {
            tempFile.delete();
        }
    }

    @Test(description = "file content: text file content should match expected")
    public void fileContentShouldMatchExpectedText() throws IOException {
        File tempDir = new File(System.getProperty("user.dir") + "/target/test-temp-files/");
        tempDir.mkdirs();
        File tempFile = new File(tempDir, "test-content.txt");
        Files.writeString(tempFile.toPath(), "expected text content");

        try {
            Validations.assertThat()
                    .file("target/test-temp-files/", "test-content.txt")
                    .content()
                    .contains("expected text")
                    .perform();
        } finally {
            tempFile.delete();
        }
    }

    @Test(description = "file content: text file content should fail when not matching")
    public void fileContentShouldFailWhenNotMatching() throws IOException {
        File tempDir = new File(System.getProperty("user.dir") + "/target/test-temp-files/");
        tempDir.mkdirs();
        File tempFile = new File(tempDir, "test-content-mismatch.txt");
        Files.writeString(tempFile.toPath(), "actual text content");

        try {
            Validations.assertThat()
                    .file("target/test-temp-files/", "test-content-mismatch.txt")
                    .content()
                    .isEqualTo("completely different text")
                    .perform();
            Assert.fail("Expected AssertionError for mismatched file content");
        } catch (AssertionError e) {
            Assert.assertNotNull(e.getMessage());
        } finally {
            tempFile.delete();
        }
    }
}
