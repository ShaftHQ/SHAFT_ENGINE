package testPackage.properties;

import com.shaft.driver.SHAFT;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class JiraTests {
    boolean isEnabled;
    String url;
    String projectKey;
    String authorization;
    String authType;
    boolean reportTestCasesExecution;
    String reportPath;
    String executionName;
    String executionDescription;
    boolean reportBugs;
    String assignee;
    String allureLinkTmsPattern;
    String allureLinkCustomPattern;

    @BeforeClass
    public void beforeClass() {
        isEnabled = SHAFT.Properties.jira.isEnabled();
        url = SHAFT.Properties.jira.url();
        projectKey = SHAFT.Properties.jira.projectKey();
        authorization = SHAFT.Properties.jira.authorization();
        authType = SHAFT.Properties.jira.authType();
        reportTestCasesExecution = SHAFT.Properties.jira.reportTestCasesExecution();
        reportPath = SHAFT.Properties.jira.reportPath();
        executionName = SHAFT.Properties.jira.executionName();
        executionDescription = SHAFT.Properties.jira.executionDescription();
        reportBugs = SHAFT.Properties.jira.reportBugs();
        assignee = SHAFT.Properties.jira.assignee();
        allureLinkTmsPattern = SHAFT.Properties.jira.allureLinkTmsPattern();
        allureLinkCustomPattern = SHAFT.Properties.jira.allureLinkCustomPattern();


    }

    @Test
    public void test() {
        SHAFT.Properties.jira.set().jiraInteraction(isEnabled);
        SHAFT.Properties.jira.set().jiraUrl(url);
        SHAFT.Properties.jira.set().projectKey(projectKey);
        SHAFT.Properties.jira.set().authorization(authorization);
        SHAFT.Properties.jira.set().authType(authType);
        SHAFT.Properties.jira.set().reportTestCasesExecution(reportTestCasesExecution);
        SHAFT.Properties.jira.set().reportPath(reportPath);
        SHAFT.Properties.jira.set().executionName(executionName);
        SHAFT.Properties.jira.set().executionDescription(executionDescription);
        SHAFT.Properties.jira.set().reportBugs(reportBugs);
        SHAFT.Properties.jira.set().assignee(assignee);
        SHAFT.Properties.jira.set().allureLinkTmsPattern(allureLinkTmsPattern);
        SHAFT.Properties.jira.set().allureLinkCustomPattern(allureLinkCustomPattern);


    }

}
