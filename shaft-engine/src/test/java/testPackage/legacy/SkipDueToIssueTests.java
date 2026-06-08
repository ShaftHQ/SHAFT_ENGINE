package testPackage.legacy;

import com.shaft.tools.io.ReportManager;
import io.qameta.allure.Issue;
import io.qameta.allure.Issues;
import org.testng.annotations.Test;

public class SkipDueToIssueTests {

    @Issue("INC1")
    @Test
    public void skip() {
        ReportManager.log("This should be skipped.");
    }

    @Test
    public void doNotSkip() {
        ReportManager.log("This should not be skipped.");
    }

    @Issues({@Issue("INC1"), @Issue("INC2")})
    @Test
    public void skip2() {
        ReportManager.log("This should be skipped.");
    }
}
