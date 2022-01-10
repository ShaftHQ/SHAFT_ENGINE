package testPackage01;

import org.testng.annotations.Test;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import com.shaft.validation.Validations;

public class Test_checkJDKGenerated {

	@Test
	public void testJDKGeneratedSuccessfully() {
		ReportManagerHelper.generateJDKShellFilesToProjectDirectory();
		ReportManager.logDiscrete("JDK Path = " + System.getProperty("java.home"));
		Validations.assertThat().object(ReportManagerHelper.getCommandsToGenerateJDKValue())
				.contains(System.getProperty("jdkVersion")).perform();
	}

}
