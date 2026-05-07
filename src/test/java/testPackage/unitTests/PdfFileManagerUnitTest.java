package testPackage.unitTests;

import com.shaft.tools.io.PdfFileManager;
import com.shaft.validation.Validations;
import com.shaft.validation.internal.ValidationsHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;

/**
 * Unit tests for {@link PdfFileManager}.
 * Uses the pre-existing sample PDF at
 * {@code src/test/resources/testDataFiles/sample.pdf}.
 */
public class PdfFileManagerUnitTest {

    private static final String SAMPLE_PDF = "src/test/resources/testDataFiles/sample.pdf";
    private final List<Path> temporaryFiles = new ArrayList<>();

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        temporaryFiles.forEach(path -> {
            try {
                Files.deleteIfExists(path);
            } catch (IOException ignored) {
                // no-op cleanup
            }
        });
        temporaryFiles.clear();
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

    @Test(description = "readFileContent(path, true) deletes copied file after reading")
    public void staticReadFileContentWithTrueDeleteRemovesFile() throws IOException {
        Path copiedPdf = createTemporaryPdfCopy();
        String content = PdfFileManager.readFileContent(copiedPdf.toString(), true);
        Assert.assertNotNull(content, "readFileContent with delete=true must not return null");
        Assert.assertFalse(content.isBlank(), "readFileContent with delete=true must return non-blank content");
        Assert.assertFalse(Files.exists(copiedPdf), "Copied PDF should be deleted when delete=true");
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

    // ─── 3-argument constructor ───────────────────────────────────────────────

    @Test(description = "PdfFileManager(folder, file, retries) constructor finds existing file")
    public void threeArgConstructorWithExistingFileShouldNotThrow() {
        // The sample PDF lives at: src/test/resources/testDataFiles/sample.pdf
        // Split into folder + filename for the 3-arg constructor
        PdfFileManager manager = new PdfFileManager(
                "src/test/resources/testDataFiles/", "sample.pdf", 1);
        String content = manager.readFileContent();
        Assert.assertNotNull(content, "3-arg constructor: readFileContent must not return null");
        Assert.assertFalse(content.isBlank(), "3-arg constructor: readFileContent must not be blank");
    }

    @Test(description = "readPDFContentFromDownloadedPDF(FALSE) returns PDF text and keeps copied file")
    public void readPdfContentWithoutDeletingFileShouldKeepCopiedFile() throws IOException {
        Path copiedPdf = createTemporaryPdfCopy();
        PdfFileManager manager = new PdfFileManager(copiedPdf.toString());
        String content = manager.readPDFContentFromDownloadedPDF(PdfFileManager.DeleteFileAfterValidationStatus.FALSE);
        Assert.assertNotNull(content, "readPDFContentFromDownloadedPDF(FALSE) must not return null");
        Assert.assertTrue(Files.exists(copiedPdf), "Copied PDF must remain when delete status is FALSE");
    }

    @Test(description = "readPDFContentFromDownloadedPDF(start,end,FALSE) reads requested page range")
    public void readPdfContentWithPageRangeShouldReturnText() {
        PdfFileManager manager = new PdfFileManager(SAMPLE_PDF);
        String content = manager.readPDFContentFromDownloadedPDF(1, 1, PdfFileManager.DeleteFileAfterValidationStatus.FALSE);
        Assert.assertNotNull(content, "readPDFContentFromDownloadedPDF(start,end,FALSE) must not return null");
    }

    @Test(description = "readPDFContentFromDownloadedPDF(TRUE) deletes copied file after reading")
    public void readPdfContentWithDeleteStatusTrueShouldDeleteCopiedFile() throws IOException {
        Path copiedPdf = createTemporaryPdfCopy();
        PdfFileManager manager = new PdfFileManager(copiedPdf.toString());
        String content = manager.readPDFContentFromDownloadedPDF(PdfFileManager.DeleteFileAfterValidationStatus.TRUE);
        Assert.assertNotNull(content, "readPDFContentFromDownloadedPDF(TRUE) must not return null");
        Assert.assertFalse(Files.exists(copiedPdf), "Copied PDF must be deleted when delete status is TRUE");
    }

    @Test(description = "readFileContent throws when file path does not exist")
    public void staticReadFileContentWithMissingFileShouldThrowRuntimeException() {
        try {
            PdfFileManager.readFileContent("src/test/resources/testDataFiles/does-not-exist.pdf");
            Assert.fail("Expected RuntimeException for missing static PDF path");
        } catch (RuntimeException exception) {
            Assert.assertTrue(exception.getMessage().contains("doesn't exist"),
                    "Missing-file static read should fail with a missing-file message");
        }
    }

    @Test(description = "PdfFileManager(path) throws when file path does not exist")
    public void constructorWithMissingFileShouldThrowRuntimeException() {
        try {
            new PdfFileManager("src/test/resources/testDataFiles/does-not-exist.pdf");
            Assert.fail("Expected RuntimeException for missing constructor PDF path");
        } catch (RuntimeException exception) {
            Assert.assertTrue(exception.getMessage().contains("Couldn't find the provided file"),
                    "Missing-file constructor should fail with constructor validation message");
        }
    }

    @Test(description = "PdfFileManager(folder,file,retries) throws when file path does not exist")
    public void threeArgConstructorWithMissingFileShouldThrowRuntimeException() {
        try {
            new PdfFileManager("src/test/resources/testDataFiles/", "does-not-exist.pdf", 1);
            Assert.fail("Expected RuntimeException for missing 3-arg constructor PDF path");
        } catch (RuntimeException exception) {
            Assert.assertTrue(exception.getMessage().contains("Couldn't find the provided file"),
                    "Missing-file 3-arg constructor should fail with constructor validation message");
        }
    }

    @Test(description = "readFileContent throws when target file is not a valid PDF")
    public void staticReadFileContentWithCorruptedPdfShouldThrowRuntimeException() throws IOException {
        Path corruptedPdf = createCorruptedPdfPlaceholder();
        RuntimeException exception =
                Assert.expectThrows(RuntimeException.class, () -> PdfFileManager.readFileContent(corruptedPdf.toString()));
        Assert.assertTrue(exception.getMessage().contains("Failed to read this PDF file"),
                "Corrupted PDF static read should fail with parse/read error message");
    }

    @Test(description = "readFileContent throws when target path points to an existing directory")
    public void staticReadFileContentWithDirectoryPathShouldThrowRuntimeException() {
        Assert.expectThrows(RuntimeException.class,
                () -> PdfFileManager.readFileContent("src/test/resources/testDataFiles"));
    }

    @Test(description = "readPDFContentFromDownloadedPDF throws when target file is not a valid PDF")
    public void readPdfContentFromDownloadedPdfWithCorruptedPdfShouldThrowRuntimeException() throws IOException {
        Path corruptedPdf = createCorruptedPdfPlaceholder();
        PdfFileManager manager = new PdfFileManager(corruptedPdf.toString());
        RuntimeException exception = Assert.expectThrows(RuntimeException.class,
                () -> manager.readPDFContentFromDownloadedPDF(PdfFileManager.DeleteFileAfterValidationStatus.FALSE));
        Assert.assertTrue(exception.getMessage().contains("Couldn't get the document that was parsed"),
                "Corrupted PDF downloaded-flow read should fail with parsing error message");
    }

    @Test(description = "readPDFContentFromDownloadedPDF throws when target path points to a directory")
    public void readPdfContentFromDownloadedPdfWithDirectoryPathShouldThrowRuntimeException() {
        PdfFileManager manager = new PdfFileManager("src/test/resources/testDataFiles");
        Assert.expectThrows(RuntimeException.class,
                () -> manager.readPDFContentFromDownloadedPDF(PdfFileManager.DeleteFileAfterValidationStatus.FALSE));
    }

    private Path createTemporaryPdfCopy() throws IOException {
        Path copiedPdf = Files.createTempFile("shaft-pdf-file-manager-", ".pdf");
        temporaryFiles.add(copiedPdf);
        Files.copy(Path.of(SAMPLE_PDF), copiedPdf, StandardCopyOption.REPLACE_EXISTING);
        return copiedPdf;
    }

    private Path createCorruptedPdfPlaceholder() throws IOException {
        Path corruptedPdf = Files.createTempFile("shaft-corrupted-pdf-", ".pdf");
        temporaryFiles.add(corruptedPdf);
        Files.writeString(corruptedPdf, "this is not a valid pdf");
        return corruptedPdf;
    }
}
