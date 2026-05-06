package testPackage.unitTests;

import com.shaft.tools.io.PdfFileManager;
import com.shaft.validation.Validations;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

/**
 * Unit tests for {@link PdfFileManager}.
 * Uses the pre-existing sample PDF at
 * {@code src/test/resources/testDataFiles/sample.pdf}.
 */
public class PdfFileManagerUnitTest {

    private static final String SAMPLE_PDF = "src/test/resources/testDataFiles/sample.pdf";

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    // ─── static readFileContent ───────────────────────────────────────────────

    @Test(description = "readFileContent(path) returns non-null, non-empty string")
    public void staticReadFileContentReturnsNonEmptyString() {
        String content = PdfFileManager.readFileContent(SAMPLE_PDF);
        Assert.assertNotNull(content, "readFileContent must not return null");
        Assert.assertFalse(content.isBlank(), "readFileContent must not return a blank string");
    }

    @Test(description = "readFileContent(path) returns content with printable characters")
    public void staticReadFileContentContainsPrintableText() {
        String content = PdfFileManager.readFileContent(SAMPLE_PDF);
        // PDF text content should contain at least one letter or digit
        Assert.assertTrue(content.matches("(?s).*[a-zA-Z0-9].*"),
                "PDF content should contain at least one alphanumeric character");
    }

    @Test(description = "readFileContent(path, keepFile=false) does not delete the source file")
    public void staticReadFileContentWithFalseDeleteKeepsFile() {
        // Reading with deleteAfterReading=false should leave the file in place
        String content = PdfFileManager.readFileContent(SAMPLE_PDF, false);
        Assert.assertNotNull(content, "readFileContent with keepFile=false must not return null");
        // Verify the file still exists after the call
        java.io.File file = new java.io.File(SAMPLE_PDF);
        Assert.assertTrue(file.exists(), "Sample PDF must still exist after read with deleteAfterReading=false");
    }

    // ─── constructor + instance readFileContent ───────────────────────────────

    @Test(description = "PdfFileManager(path) constructor + readFileContent() returns same as static call")
    public void instanceReadFileContentMatchesStaticMethod() {
        String staticContent = PdfFileManager.readFileContent(SAMPLE_PDF);
        PdfFileManager manager = new PdfFileManager(SAMPLE_PDF);
        String instanceContent = manager.readFileContent();
        Assert.assertNotNull(instanceContent, "Instance readFileContent must not return null");
        Validations.assertThat().object(instanceContent).isEqualTo(staticContent).perform();
    }

    // ─── DeleteFileAfterValidationStatus enum ────────────────────────────────

    @Test(description = "DeleteFileAfterValidationStatus enum has TRUE and FALSE values")
    public void deleteFileAfterValidationStatusEnumHasBothValues() {
        PdfFileManager.DeleteFileAfterValidationStatus[] values = PdfFileManager.DeleteFileAfterValidationStatus.values();
        Assert.assertEquals(values.length, 2, "Enum must have exactly 2 values");
        Assert.assertEquals(values[0], PdfFileManager.DeleteFileAfterValidationStatus.TRUE);
        Assert.assertEquals(values[1], PdfFileManager.DeleteFileAfterValidationStatus.FALSE);
    }

    @Test(description = "DeleteFileAfterValidationStatus.TRUE valueOf works")
    public void deleteFileAfterValidationStatusTrueValueOfWorks() {
        PdfFileManager.DeleteFileAfterValidationStatus status =
                PdfFileManager.DeleteFileAfterValidationStatus.valueOf("TRUE");
        Assert.assertEquals(status, PdfFileManager.DeleteFileAfterValidationStatus.TRUE);
    }

    @Test(description = "DeleteFileAfterValidationStatus.FALSE valueOf works")
    public void deleteFileAfterValidationStatusFalseValueOfWorks() {
        PdfFileManager.DeleteFileAfterValidationStatus status =
                PdfFileManager.DeleteFileAfterValidationStatus.valueOf("FALSE");
        Assert.assertEquals(status, PdfFileManager.DeleteFileAfterValidationStatus.FALSE);
    }
}
