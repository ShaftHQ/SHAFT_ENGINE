package com.shaft.tools.io;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.pdfbox.cos.COSDocument;
import org.apache.pdfbox.io.RandomAccessBufferedFileInputStream;
import org.apache.pdfbox.pdfparser.PDFParser;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;
import org.testng.Assert;

public class PdfFileManager {

	private RandomAccessBufferedFileInputStream stream = null;
	private PDFParser parser = null;
	private COSDocument cosDoc = null;
	private PDFTextStripper strip = null;
	private File file;

	/**
	 * Validates the downloaded pdf document from the site under test
	 * 
	 * @param url                the path of the downloaded pdf file (usually will
	 *                           be target/downloadedFiles/your-file.pdf), usually
	 *                           will be in "target/downloadedFiles/" for the
	 *                           project
	 * @param milliSecondsToWait the time in milliseconds needed to wait on the
	 *                           document to be downloaded successfully to the
	 *                           target folder path
	 */
	public PdfFileManager(String url, int milliSecondsToWait) {
		// Waiting for file to download
		waitOnDocumentToBeReady(milliSecondsToWait);

		file = new File(url);
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

	private void waitOnDocumentToBeReady(int milliSeconds) {
		try {
			Thread.sleep(milliSeconds);
		} catch (InterruptedException e) {
			ReportManagerHelper.log(e);
			ReportManager.log("Waiting for file to download has been interrupted.");
			Assert.fail("Waiting for file to download has been interrupted.");
		}
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

		switch (deleteFileAfterValidation) {
		case TRUE:
			try {
				FileUtils.forceDelete(file);
			} catch (IOException e) {
				ReportManagerHelper.log(e);
				ReportManager.log("Couldn't find the file, File directory may be null or file is not found.");
				Assert.fail("Couldn't find the file, File directory may be null or file is not found.");
			}
		default:
			break;
		}

	}

	// Will implement same for preview pdf file
}
