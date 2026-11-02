package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class PatternTests {
    String testDataColumnNamePrefix;
    String allureLinkIssuePattern;

    @BeforeClass
    public void beforeClass() {
        testDataColumnNamePrefix = SHAFT.Properties.pattern.testDataColumnNamePrefix();
        allureLinkIssuePattern = SHAFT.Properties.pattern.allureLinkIssuePattern();

    }

    @Test
    public void test() {
        SHAFT.Properties.pattern.set().testDataColumnNamePrefix(testDataColumnNamePrefix);
        SHAFT.Properties.pattern.set().allureLinkIssuePattern(allureLinkIssuePattern);

    }
}
