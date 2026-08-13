package com.shaft.tools.io;

import com.shaft.cli.FileActions;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.PdfBatchProcessor;
import com.shaft.tools.io.internal.PdfDocumentProcessor;
import com.shaft.tools.io.pdf.PdfBatchOptions;
import com.shaft.tools.io.pdf.PdfBatchResult;
import com.shaft.tools.io.pdf.PdfDocumentOptions;
import com.shaft.tools.io.pdf.PdfDocumentRequest;
import com.shaft.tools.io.pdf.PdfDocumentResult;
import com.shaft.tools.io.pdf.PdfExportRequest;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.io.IOUtils;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

/**
 * Reads PDF text and processes native, scanned, and mixed multipage PDF documents.
 *
 * <p>The legacy text-reading methods remain source compatible. Document processing uses
 * native positioned text first and the optional {@code shaft-ocr} provider when pixels
 * contain text that the PDF text layer does not cover.</p>
 *
 * @see com.shaft.cli.FileActions
 */
@SuppressWarnings("unused")
public class PdfFileManager {
    private final File file;

    public PdfFileManager(String folderName, String fileName, int numberOfRetries) {
        boolean exists = FileActions.getInstance(true).doesFileExist(folderName, fileName, numberOfRetries);
        file = new File(FileActions.getInstance(true).getAbsolutePath(folderName, fileName));
        if (!exists) {
            FailureReporter.fail("Couldn't find the provided file [" + file
                    + "]. It might need to wait more to download or the path isn't correct");
        }
    }

    public PdfFileManager(String pdfFilePath) {
        String resolved = JavaHelper.appendTestDataToRelativePath(pdfFilePath);
        boolean exists = FileActions.getInstance(true).doesFileExist(resolved);
        file = new File(FileActions.getInstance(true).getAbsolutePath(resolved));
        if (!exists) {
            FailureReporter.fail("Couldn't find the provided file [" + file
                    + "]. It might need to wait more to download or the path isn't correct");
        }
    }

    /** Reads all native PDF text and optionally deletes the source afterward. */
    public static String readFileContent(String relativeFilePath, boolean... deleteFileAfterReading) {
        if (!FileActions.getInstance(true).doesFileExist(relativeFilePath)) {
            FailureReporter.fail("This PDF file [" + relativeFilePath + "] doesn't exist.");
            return "";
        }
        File source = new File(FileActions.getInstance(true).getAbsolutePath(relativeFilePath));
        String content;
        try (PDDocument document = Loader.loadPDF(source, IOUtils.createTempFileOnlyStreamCache())) {
            PDFTextStripper stripper = new PDFTextStripper();
            stripper.setSortByPosition(true);
            content = stripper.getText(document);
        } catch (IOException exception) {
            FailureReporter.fail(PdfFileManager.class, "Failed to read this PDF file [" + relativeFilePath + "].", exception);
            return "";
        }
        if (deleteFileAfterReading != null && deleteFileAfterReading.length > 0 && deleteFileAfterReading[0]) {
            FileActions.getInstance(true).deleteFile(relativeFilePath);
        }
        return content;
    }

    public String readFileContent() {
        return readFileContent(file.getPath());
    }

    /** Reads native text from an inclusive one-based page range. */
    public String readPDFContentFromDownloadedPDF(int startPageNumber, int endPageNumber,
                                                  DeleteFileAfterValidationStatus deleteStatus) {
        if (startPageNumber < 1 || endPageNumber < startPageNumber) {
            throw new IllegalArgumentException("PDF page range must be one-based and ordered.");
        }
        return readRange(startPageNumber, endPageNumber, deleteStatus);
    }

    /** Reads native text from every page. */
    public String readPDFContentFromDownloadedPDF(DeleteFileAfterValidationStatus deleteStatus) {
        return readRange(null, null, deleteStatus);
    }

    /** Processes this PDF using default hybrid document OCR options. */
    public PdfDocumentResult process(PdfExportRequest... exports) {
        return process(PdfDocumentOptions.defaults(), exports);
    }

    /** Processes this PDF and writes only the explicitly requested exports. */
    public PdfDocumentResult process(PdfDocumentOptions options, PdfExportRequest... exports) {
        List<PdfExportRequest> requests = exports == null ? List.of() : Arrays.asList(exports);
        return PdfDocumentProcessor.process(file.toPath(), options, requests);
    }

    /** Processes an ordered PDF batch with bounded default concurrency. */
    public static PdfBatchResult processAll(List<PdfDocumentRequest> requests) {
        return processAll(requests, PdfBatchOptions.defaults());
    }

    /** Processes an ordered PDF batch with explicit concurrency and failure behavior. */
    public static PdfBatchResult processAll(List<PdfDocumentRequest> requests, PdfBatchOptions options) {
        return PdfBatchProcessor.process(List.copyOf(requests), options);
    }

    private String readRange(Integer startPage, Integer endPage, DeleteFileAfterValidationStatus deleteStatus) {
        String content;
        try (PDDocument document = Loader.loadPDF(file, IOUtils.createTempFileOnlyStreamCache())) {
            PDFTextStripper stripper = new PDFTextStripper();
            stripper.setSortByPosition(true);
            if (startPage != null) {
                if (endPage > document.getNumberOfPages()) {
                    throw new IllegalArgumentException("PDF end page " + endPage + " exceeds page count "
                            + document.getNumberOfPages() + ".");
                }
                stripper.setStartPage(startPage);
                stripper.setEndPage(endPage);
            }
            content = stripper.getText(document);
        } catch (IOException exception) {
            FailureReporter.fail(PdfFileManager.class,
                    "Couldn't get the document that was parsed", exception);
            return "";
        }
        if (deleteStatus == DeleteFileAfterValidationStatus.TRUE) {
            FileActions.getInstance(true).deleteFile(file.getPath());
        }
        return content;
    }

    public enum DeleteFileAfterValidationStatus {
        TRUE,
        FALSE
    }
}
