package testPackage01;

import org.testng.annotations.Test;

import com.shaft.tools.io.PdfFileManager;
import com.shaft.tools.io.PdfFileManager.DeleteFileAfterValidationStatus;
import com.shaft.validation.Assertions;

public class Test_PDF {

	@Test
	public void testPDF_Downloaded() {
		String content = new PdfFileManager("src/test/resources/TestDataFiles/", "sample.pdf", 20)
				.readPDFContentFromDownloadedPDF(1, 1, DeleteFileAfterValidationStatus.FALSE);
		System.out.println(content);
		Assertions.assertTrue(content.contains("A Simple PDF File"), Assertions.AssertionType.POSITIVE);
	}
}
