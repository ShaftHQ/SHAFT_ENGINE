package testPackage01;

import org.testng.annotations.Test;

import com.shaft.cli.FileActions;
import com.shaft.tools.io.ReportManager;

public class Test_FileActions {
    @Test
    public void f() {
	ReportManager.log(FileActions.listFilesInDirectory("/home/"));
    }
}
