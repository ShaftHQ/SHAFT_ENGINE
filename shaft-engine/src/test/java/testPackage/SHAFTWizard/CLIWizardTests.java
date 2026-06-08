package testPackage.SHAFTWizard;

import com.shaft.driver.SHAFT;
import org.testng.annotations.Test;

public class CLIWizardTests {

    @Test
    public void test() {
        var str = SHAFT.CLI.file().readPDF("sample.pdf");
        SHAFT.Report.attach("Custom", "PDF", str);
    }
}
