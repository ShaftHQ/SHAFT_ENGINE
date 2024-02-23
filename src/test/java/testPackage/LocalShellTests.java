package testPackage;

import com.shaft.cli.TerminalActions;
import com.shaft.validation.Validations;
import io.qameta.allure.Issue;
import org.testng.annotations.Test;

import java.util.Arrays;

public class LocalShellTests {
    @Test
    public void test_localShellCommand() {
        String response = (new TerminalActions()).performTerminalCommand("ls");
        Validations.assertThat().object(response).contains("").perform();
    }

    @Issue("sampleIssueLink-PassingTest")
    @Test
    public void test_localShellCommands() {
        String response = (new TerminalActions()).performTerminalCommands(Arrays.asList("ls", "ls -ltr"));
        Validations.assertThat().object(response).contains("").perform();
    }

    @Issue("sampleIssueLink-FailingTest")
    @Test
    public void test_localShellIssue() {
        String response = (new TerminalActions()).performTerminalCommand("date +%m/%d/%y");
        Validations.assertThat().object(response.trim()).equals("04/17/19");
    }

    @Test
    public void test_localShellIssue2() {
        String calendar = (new TerminalActions()).performTerminalCommand("cal").replaceAll("\n", "").replaceAll(" ", "")
                .replaceAll("_\b", "").trim();
        String lastDayOfCurrentMonth = calendar.substring(calendar.length() - 2);

        String lastDateOfCurrentMonth = (new TerminalActions())
                .performTerminalCommand("date +%m/" + lastDayOfCurrentMonth + "/%y");

        Validations.assertThat().object(lastDateOfCurrentMonth).equals("04/30/19");
    }
}
