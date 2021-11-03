package testPackage01;

import com.shaft.validation.Validations;
import org.testng.annotations.Test;

import com.shaft.tools.io.PdfFileManager;
import com.shaft.tools.io.PdfFileManager.DeleteFileAfterValidationStatus;
import com.shaft.validation.Assertions;

import java.util.Optional;

public class Test_PDF {

	@Test
	public void testPDF_Downloaded() {
		String content = new PdfFileManager("src/test/resources/TestDataFiles/", "sample.pdf", 20)
				.readPDFContentFromDownloadedPDF(1, 1, DeleteFileAfterValidationStatus.FALSE);
		System.out.println(content);
		Assertions.assertTrue(content.contains("A Simple PDF File"), Assertions.AssertionType.POSITIVE);
	}

	@Test
	public void testPDF2() {
		var actualFile = PdfFileManager.readFileContent("src/test/resources/TestDataFiles/sample.pdf");
		var expectedData = "A Simple PDF File";
		Validations.assertThat()
				.object(actualFile)
				.contains(expectedData)
				.withCustomReportMessage("Checking to see that the pdf file contains [" + expectedData + "]")
				.perform();
	}

	@Test
	public void testPDF3(){
		var expectedData = "A Simple PDF File";
		Validations.assertThat()
				.file("src/test/resources/TestDataFiles/", "sample.pdf")
				.content()
				.contains(expectedData)
				.withCustomReportMessage("Checking to see that the pdf file contains [" + expectedData + "]")
				.perform();
	}
}
