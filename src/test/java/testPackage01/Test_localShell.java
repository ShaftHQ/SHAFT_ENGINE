package testPackage01;

import java.util.Arrays;
import java.util.List;

import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.shaft.support.SSHActions;

public class Test_localShell {
	@Test
	public void test_localShellCommand() {

		List<String> commands = Arrays.asList("ls", "ls -ltr");
		(new SSHActions()).executeShellCommand(commands);
	}

	@BeforeClass // Set-up method, to be run once before the first test
	public void beforeClass() {

	}
}
