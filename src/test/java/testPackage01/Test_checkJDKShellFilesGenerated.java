package testPackage01;

import org.testng.annotations.Test;
import com.shaft.tools.io.ReportManagerHelper;
import com.shaft.validation.Validations;

public class Test_checkJDKShellFilesGenerated {

	@Test
	public void testJDKGeneratedSuccessfully() {

		ReportManagerHelper.generateJDKShellFilesToProjectDirectory();
//		if (SystemUtils.IS_OS_WINDOWS) {
			Validations.assertThat().object(ReportManagerHelper.getCommandsToGenerateJDKBatFile())
					.contains(System.getProperty("jdkVersion")).perform();
//		} else {
			System.out.println(ReportManagerHelper.getCommandsToGenerateJDKShellFile());
			Validations.assertThat().object(ReportManagerHelper.getCommandsToGenerateJDKShellFile())
					.contains(System.getProperty("jdkVersion").substring(0,6)).perform();
//		}
	}
	
	

}
