package testPackage01;

import com.shaft.cli.TerminalActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionComparisonType;
import com.shaft.validation.Assertions.AssertionType;
import io.qameta.allure.Issue;
import org.testng.annotations.Test;

import java.util.Arrays;

public class Test_localShell {
    @Test
    public void test_localShellCommand() {
        String response = (new TerminalActions()).performTerminalCommand("ls");
        Assertions.assertEquals("", response, AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
    }

    @Issue("sampleIssueLink-PassingTest")
    @Test
    public void test_localShellCommands() {
        String response = (new TerminalActions()).performTerminalCommands(Arrays.asList("ls", "ls -ltr"));
        Assertions.assertEquals("", response, AssertionComparisonType.CONTAINS, AssertionType.POSITIVE);
    }

    @Issue("sampleIssueLink-FailingTest")
    @Test
    public void test_localShellIssue() {
        String response = (new TerminalActions()).performTerminalCommand("date +%m/%d/%y");
        Assertions.assertEquals("04/17/19", response.trim());
    }

    @Test
    public void test_localShellIssue2() {
        String calendar = (new TerminalActions()).performTerminalCommand("cal").replaceAll("\n", "").replaceAll(" ", "")
                .replaceAll("_\b", "").trim();
        String lastDayOfCurrentMonth = calendar.substring(calendar.length() - 2);

        String lastDateOfCurrentMonth = (new TerminalActions())
                .performTerminalCommand("date +%m/" + lastDayOfCurrentMonth + "/%y");

        Assertions.assertEquals("04/30/19", lastDateOfCurrentMonth);
    }
}
