package testPackage.legacy;

import com.shaft.cli.TerminalActions;
import com.shaft.validation.Validations;
import org.testng.annotations.Test;

import java.util.Arrays;

public class LocalShellTests {
    @Test
    public void test_localShellCommand() {
        String response = (new TerminalActions()).performTerminalCommand("ls");
        Validations.assertThat().object(response).contains("pom.xml").perform();
    }

    @Test
    public void test_localShellCommands() {
        String response = (new TerminalActions()).performTerminalCommands(Arrays.asList("ls", "ls -ltr"));
        Validations.assertThat().object(response).contains("pom.xml").perform();
    }
}
