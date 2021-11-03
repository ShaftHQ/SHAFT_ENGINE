package com.shaft.tools.io;

import com.shaft.cli.FileActions;
import org.apache.commons.io.FileUtils;
import org.apache.pdfbox.cos.COSDocument;
import org.apache.pdfbox.io.RandomAccessBufferedFileInputStream;
import org.apache.pdfbox.pdfparser.PDFParser;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;
import org.testng.Assert;

import java.io.File;
import java.io.IOException;

public class PdfFileManager {

	private RandomAccessBufferedFileInputStream stream = null;
	private PDFParser parser = null;
	private COSDocument cosDoc = null;
	private PDFTextStripper strip = null;
	private final File file;

	public PdfFileManager(String folderName, String fileName, int numberOfRetries) {

		boolean doesFileExist = FileActions.doesFileExist(folderName, fileName, numberOfRetries);

		file = new File(FileActions.getAbsolutePath(folderName, fileName));

		if (!doesFileExist) {
			ReportManager.log("Couldn't find the provided file [" + file
					+ "]. It might need to wait more to download or the path isn't correct");
			Assert.fail("Couldn't find the provided file [" + file
					+ "]. It might need to wait more to download or the path isn't correct");
		}
	}

	public enum DeleteFileAfterValidationStatus {
		TRUE, FALSE
	}

	/**
	 * @param startPageNumber                 the starting page for the document to
	 *                                        be validated
	 * @param endPageNumber                   the ending page for the document to be
	 *                                        validated
	 * @param deleteFileAfterValidationStatus the status of deleting the file after
	 *                                        get the document text
	 * @return returns the pdf content in string so that can be validated
	 */
	public String readPDFContentFromDownloadedPDF(int startPageNumber, int endPageNumber,
			DeleteFileAfterValidationStatus deleteFileAfterValidationStatus) {

		stream = readFileInputStream(file);
		parser = parseStreamDocument(stream);

		cosDoc = getParsedDocument(parser);
		String content = getPdfText(cosDoc, startPageNumber, endPageNumber);
		closeStreamAndDeleteFile(file, stream, deleteFileAfterValidationStatus);

		return content;
	}

	/**
	 * Read PDF file content given relative path and optionally delete the file after reading it
	 * @param relativeFilePath relative path to the PDF file
	 * @param deleteFileAfterReading optional boolean to delete the file after reading it or not, default is to leave the file as is
	 * @return a string value representing the entire content of the pdf file
	 */
	public static String readFileContent(String relativeFilePath, boolean... deleteFileAfterReading){
		if (FileActions.doesFileExist(relativeFilePath)) {
			try {
				var randomAccessBufferedFileInputStream = new RandomAccessBufferedFileInputStream(new File(FileActions.getAbsolutePath(relativeFilePath)));
				var pdfParser = new PDFParser(randomAccessBufferedFileInputStream);
				pdfParser.parse();
				var pdfTextStripper = new PDFTextStripper();
				pdfTextStripper.setSortByPosition(true);
				var fileContent = pdfTextStripper.getText(new PDDocument(pdfParser.getDocument()));
				randomAccessBufferedFileInputStream.close();

				if (deleteFileAfterReading!=null
						&& deleteFileAfterReading.length>0
						&& deleteFileAfterReading[0]){
					FileActions.deleteFile(relativeFilePath);
				}
				return fileContent;
			}catch (java.io.IOException e) {
				ReportManagerHelper.log(e);
				Assert.fail("Failed to read this PDF file ["+relativeFilePath+"].");
			}

		}else {
			Assert.fail("This PDF file ["+relativeFilePath+"] doesn't exist.");
		}
		return "";
	}

	public String readPDFContentFromDownloadedPDF(DeleteFileAfterValidationStatus deleteFileAfterValidationStatus) {

		stream = readFileInputStream(file);
		parser = parseStreamDocument(stream);

		cosDoc = getParsedDocument(parser);
		String content = getPdfText(cosDoc);
		closeStreamAndDeleteFile(file, stream, deleteFileAfterValidationStatus);

		return content;
	}

	private RandomAccessBufferedFileInputStream readFileInputStream(File file) {
		try {
			stream = new RandomAccessBufferedFileInputStream(file);
		} catch (IOException e) {
			ReportManagerHelper.log(e);
			ReportManager.log("Couldn't read the date from the provided file [" + file + "].");
			Assert.fail("Couldn't read the data from the provided file [" + file + "].");
		}
		return stream;
	}

	private PDFParser parseStreamDocument(RandomAccessBufferedFileInputStream stream) {
		try {
			parser = new PDFParser(stream);
			parser.parse();
		} catch (IOException e) {
			ReportManagerHelper.log(e);
			ReportManager.log(
					"Couldn't parse the stream that opened the document to be prepared to populate the COSDocument object.");
			Assert.fail(
					"Couldn't parse the stream that opened the document to be prepared to populate the COSDocument object.");
		}
		return parser;
	}

	private COSDocument getParsedDocument(PDFParser parser) {
		try {
			cosDoc = parser.getDocument();
		} catch (IOException e) {
			ReportManagerHelper.log(e);
			ReportManager.log(
					"Couldn't get the document that was parsed. Check that the document parsed before get the document.");
			Assert.fail(
					"Couldn't get the document that was parsed. Check that the document parsed before get the document.");
		}
		return cosDoc;
	}

	private String getPdfText(COSDocument cosDoc, int startPageNumber, int endPageNumber) {
		try {
			strip = new PDFTextStripper();
			// By default, text extraction is done in the same sequence as the text in the
			// PDF page content stream. PDF is a graphic format, not a text format, and
			// unlike HTML, it has no requirements that text one on page be rendered in a
			// certain order. The order is the one that was determined by the software that
			// created the PDF
			// To get text sorted from left to right and top to bottom
			strip.setSortByPosition(true);
		} catch (IOException e) {
			ReportManagerHelper.log(e);
			ReportManager.log("Couldn't load PDFTextStripper properties.");
			Assert.fail("Couldn't load PDFTextStripper properties.");
		}

		strip.setStartPage(startPageNumber);
		strip.setEndPage(endPageNumber);
		PDDocument pdDoc = new PDDocument(cosDoc);

		String content = null;
		try {
			content = strip.getText(pdDoc);
		} catch (IOException e) {
			ReportManagerHelper.log(e);
			ReportManager.log("Couldn't get document text. Document state is invalid or it is encrypted.");
			Assert.fail("Couldn't get document text. Document state is invalid or it is encrypted.");
		}
		return content;
	}

	private String getPdfText(COSDocument cosDoc) {
		try {
			strip = new PDFTextStripper();
			strip.setSortByPosition(true);
		} catch (IOException e) {
			ReportManagerHelper.log(e);
			ReportManager.log("Couldn't load PDFTextStripper properties.");
			Assert.fail("Couldn't load PDFTextStripper properties.");
		}

		PDDocument pdDoc = new PDDocument(cosDoc);

		String content = null;
		try {
			content = strip.getText(pdDoc);
		} catch (IOException e) {
			ReportManagerHelper.log(e);
			ReportManager.log("Couldn't get document text. Document state is invalid or it is encrypted.");
			Assert.fail("Couldn't get document text. Document state is invalid or it is encrypted.");
		}
		return content;
	}

	private void closeStreamAndDeleteFile(File file, RandomAccessBufferedFileInputStream stream,
			DeleteFileAfterValidationStatus deleteFileAfterValidation) {
		try {
			stream.close();
		} catch (IOException e) {
			ReportManagerHelper.log(e);
			ReportManager.log("Couldn't close the stream, check if it already opened.");
			Assert.fail("Couldn't close the stream, check if it already opened.");
		}

		// Delete the file from target folder for next run

		if (deleteFileAfterValidation == DeleteFileAfterValidationStatus.TRUE) {
			try {
				FileUtils.forceDelete(file);
			} catch (IOException e) {
				ReportManagerHelper.log(e);
				ReportManager.log("Couldn't find the file, File directory may be null or file is not found.");
				Assert.fail("Couldn't find the file, File directory may be null or file is not found.");
			}
		}

	}

	// Will implement same for preview pdf file
}
