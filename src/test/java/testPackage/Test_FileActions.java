package testPackage;

import com.shaft.cli.FileActions;
import com.shaft.validation.Validations;
import org.testng.annotations.Test;

public class Test_FileActions {
    @Test
    public void f() {
        Validations.assertThat().object(FileActions.getInstance().listFilesInDirectory("/home/")).contains("").perform();
    }
}
