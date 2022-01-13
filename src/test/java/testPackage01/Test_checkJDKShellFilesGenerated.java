package testPackage01;

import org.apache.commons.lang3.SystemUtils;
import org.testng.annotations.Test;
import com.shaft.tools.io.ReportManagerHelper;
import com.shaft.validation.Validations;

public class Test_checkJDKShellFilesGenerated {

	@Test
	public void testJDKGeneratedSuccessfully() {

		ReportManagerHelper.generateJDKShellFilesToProjectDirectory();
		if (SystemUtils.IS_OS_WINDOWS) {
			Validations.assertThat().object(ReportManagerHelper.getCommandsToGenerateJDKBatFile())
					.contains(System.getProperty("java.runtime.version").substring(0,6)).perform();
			Validations.assertThat().object(ReportManagerHelper.getCommandsToGenerateJDKShellFile())
					.contains(System.getProperty("java.runtime.version").substring(0,6)).perform();
		} else {
			Validations.assertThat().object(ReportManagerHelper.getCommandsToGenerateJDKShellFile())
					.contains("JAVA_HOME=$(/usr/libexec/java_home)").perform();
		}
	}
	
	

}
