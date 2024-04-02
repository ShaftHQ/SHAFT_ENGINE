package testPackage.legacy;

import com.shaft.cli.FileActions;
import com.shaft.validation.Validations;
import org.testng.annotations.Test;

public class FileActionsTests {
    @Test
    public void f() {
        Validations.assertThat().object(FileActions.getInstance().listFilesInDirectory("src/main/java/com/shaft")).contains("SHAFT.java").perform();
    }
}
