package testPackage01;

import java.util.Arrays;

import org.testng.annotations.Test;

import com.shaft.cli.TerminalActions;
import com.shaft.validation.Assertions;

public class Test_localShell {
    @Test
    public void test_localShellCommand() {
	String response = (new TerminalActions()).performTerminalCommands(Arrays.asList("ls", "ls -ltr"));
	Assertions.assertEquals("", response, 3, true);
    }
}
