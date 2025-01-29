package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class AllureResultsTests {
    String allureResultsDirectory;

    @BeforeClass
    public void beforeClass() {
        allureResultsDirectory = SHAFT.Properties.allureResults.allureResultsDirectory();
    }

    @Test
    public void test() {
        SHAFT.Properties.allureResults.set().allureResultsDirectory(allureResultsDirectory);
    }
}
