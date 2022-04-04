package testPackage01.SHAFTWizard;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

public class Test_Wizard_CLI {
    SHAFT.CLI driver;

    @Test
    public void test() {
        driver = new SHAFT.CLI();
        var str = driver.file().readFromPdfFile("src/test/resources/testDataFiles/", "sample.pdf");
        SHAFT.Report.attach("Custom", "PDF", str);
    }
}
