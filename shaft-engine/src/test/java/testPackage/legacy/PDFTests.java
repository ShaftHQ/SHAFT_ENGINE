package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.PdfFileManager;
import com.shaft.validation.Validations;

public class PDFTests {

    //    @Test
    public void testPDF1() {
        var actualFile = PdfFileManager.readFileContent(SHAFT.Properties.paths.testData() + "sample.pdf");
        var expectedData = "A Simple PDF File";
        Validations.assertThat()
                .object(actualFile)
                .contains(expectedData)
                .withCustomReportMessage("Checking to see that the pdf file contains [" + expectedData + "]")
                .perform();
    }

    //    @Test
    public void testPDF2(){
        var expectedData = "A Simple PDF File";
        Validations.assertThat()
                .file(SHAFT.Properties.paths.testData(), "sample.pdf")
                .content()
                .contains(expectedData)
                .withCustomReportMessage("Checking to see that the pdf file contains [" + expectedData + "]")
                .perform();
    }
}
