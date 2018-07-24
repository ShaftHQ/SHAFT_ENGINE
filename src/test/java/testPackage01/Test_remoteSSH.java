package testPackage01;

import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaftEngine.ioActionLibrary.ReportManager;
import com.shaftEngine.supportActionLibrary.SSHActions;

public class Test_remoteSSH {

	@Test
	public void test_executeRemoteSSH() {
		String hostname = "72.55.136.10";
		int sshPortNumber = 5022;
		String username = "incorta";
		String keyFileFolderName = System.getProperty("testDataFolderPath");
		String keyFileName = "iWebQAEnv_Key.txt";
		String sshCommand = "ls -ltr";

		SSHActions.performSSHcommand(hostname, sshPortNumber, username, keyFileFolderName, keyFileName, sshCommand);
	}

	@BeforeClass // Set-up method, to be run once before the first test
	public void beforeClass() {
	}

	@AfterClass(alwaysRun = true) // Tear-down method, to be run once after the last test
	public void afterClass() {
		ReportManager.getFullLog();
	}

	@AfterMethod
	public void afterMethod() {
		ReportManager.getTestLog();
	}
}
