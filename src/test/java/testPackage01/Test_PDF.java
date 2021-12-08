package testPackage01;

import com.shaft.tools.io.PdfFileManager;
import com.shaft.validation.Validations;
import org.testng.annotations.Test;

public class Test_PDF {

	@Test
	public void testPDF1() {
		var actualFile = PdfFileManager.readFileContent(System.getProperty("testDataFolderPath")+"sample.pdf");
		var expectedData = "A Simple PDF File";
		Validations.assertThat()
				.object(actualFile)
				.contains(expectedData)
				.withCustomReportMessage("Checking to see that the pdf file contains [" + expectedData + "]")
				.perform();
	}

	@Test
	public void testPDF2(){
		var expectedData = "A Simple PDF File";
		Validations.assertThat()
				.file(System.getProperty("testDataFolderPath"), "sample.pdf")
				.content()
				.contains(expectedData)
				.withCustomReportMessage("Checking to see that the pdf file contains [" + expectedData + "]")
				.perform();
	}
}
