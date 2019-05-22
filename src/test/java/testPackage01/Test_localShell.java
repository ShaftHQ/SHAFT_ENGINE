package testPackage01;

import java.util.Arrays;

import org.testng.annotations.Test;

import com.shaft.cli.TerminalActions;
import com.shaft.validation.Assertions;

public class Test_localShell {
    @Test
    public void test_localShellCommand() {
	String response = (new TerminalActions()).performTerminalCommand("ls");
	Assertions.assertEquals("", response, 3, true);
    }

    @Test
    public void test_localShellCommands() {
	String response = (new TerminalActions()).performTerminalCommands(Arrays.asList("ls", "ls -ltr"));
	Assertions.assertEquals("", response, 3, true);
    }

    @Test
    public void test_localShellIssue() {
	String response = (new TerminalActions()).performTerminalCommand("date +%m/%d/%y");
	Assertions.assertEquals("04/17/19", response.trim(), 1, true);
    }

    @Test
    public void test_localShellIssue2() {
	String calendar = (new TerminalActions()).performTerminalCommand("cal").replaceAll("\n", "").replaceAll(" ", "")
		.replaceAll("_\b", "").trim();
	String lastDayOfCurrentMonth = calendar.substring(calendar.length() - 2, calendar.length());

	String lastDateOfCurrentMonth = (new TerminalActions())
		.performTerminalCommand("date +%m/" + lastDayOfCurrentMonth + "/%y");

	Assertions.assertEquals("04/30/19", lastDateOfCurrentMonth, 1, true);
    }
}
