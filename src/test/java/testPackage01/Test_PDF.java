package testPackage01;

import org.testng.annotations.Test;

import com.shaft.cli.FileActions;
import com.shaft.tools.io.PdfFileManager;
import com.shaft.validation.Assertions;

public class Test_PDF {

	@Test
	public void testPDF_Downloaded() {
		String url = FileActions.getAbsolutePath("src/test/resources/TestDataFiles/", "sample.pdf");

		System.out.println(url);

		String content = new PdfFileManager(url, 7000).readPDFContentFromDownloadedPDF(1, 1);
		System.out.println(content);
		Assertions.assertTrue(content.contains("A Simple PDF File"), Assertions.AssertionType.POSITIVE);
	}
}
