package testPackage01;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import org.apache.commons.lang3.SystemUtils;
import org.testng.annotations.Test;
import com.shaft.cli.FileActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;

public class Test_checkJDKGenerated {
	private static final String jdkExtractionLocation = System.getProperty("user.home") + File.separator + ".jdks"
			+ File.separator + "openjdk-" + System.getProperty("jdkVersion");
	private static List<String> commandsToGenerateJDK;

	private static void generateJDKShellFilesToProjectDirectory() {
		if (SystemUtils.IS_OS_WINDOWS) {
			// create windows batch file
			commandsToGenerateJDK = Arrays.asList("@echo off", "set JAVA_HOME=" + jdkExtractionLocation,
					"set M2=%M2_HOME%\\bin", "set PATH=%JAVA_HOME%\\bin;%M2%;%PATH%", "echo %JAVA_HOME%", "echo %PATH%",
					"pause", "exit");
			FileActions.writeToFile("", "generateJdk.bat", commandsToGenerateJDK);
		} else {
//            // create unix-based sh file
			// TODO create commands of .sh files
//            commandsToServeAllureReport = Arrays
//                    .asList("");
//            FileActions.writeToFile("", "generateJdk.sh", commandsToServeAllureReport);
//            // make allure executable on unix-based shells
//            (new TerminalActions()).performTerminalCommand("chmod u+x generateJdk.sh");
		}
	}

	@Test
	public void testJDKGeneratedSuccessfully() {
		generateJDKShellFilesToProjectDirectory();
		ReportManager.logDiscrete("JDK Path = " + System.getProperty("java.home"));
		Validations.assertThat().object(commandsToGenerateJDK).contains(System.getProperty("jdkVersion")).perform();
	}

}
