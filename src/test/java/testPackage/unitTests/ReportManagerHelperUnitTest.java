package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class ReportManagerHelperUnitTest {

    @BeforeMethod(alwaysRun = true)
    @AfterMethod(alwaysRun = true)
    public void resetReportManagerHelperState() throws Exception {
        getPrivateStaticField("issuesLog", AtomicReference.class).set("");
        getPrivateStaticField("issueCounter", AtomicInteger.class).set(1);
        ReportManagerHelper.setOpenIssuesForFailedTestsCounter(0);
        ReportManagerHelper.setOpenIssuesForPassedTestsCounter(0);
        ReportManagerHelper.setFailedTestsWithoutOpenIssuesCounter(0);
        ReportManagerHelper.setListOfOpenIssuesForFailedTests(Collections.synchronizedList(new ArrayList<>()));
        ReportManagerHelper.setListOfOpenIssuesForPassedTests(Collections.synchronizedList(new ArrayList<>()));
        ReportManagerHelper.setListOfNewIssuesForFailedTests(Collections.synchronizedList(new ArrayList<>()));
    }

    @Test
    public void prepareIssuesLogShouldReturnEmptyWhenNoIssuesExist() {
        String issuesSummary = ReportManagerHelper.prepareIssuesLog();

        SHAFT.Validations.assertThat().object(issuesSummary).isEqualTo("").perform();
        SHAFT.Validations.assertThat().object(ReportManagerHelper.getIssueCounter()).isEqualTo(0).perform();
    }

    @Test
    public void prepareIssuesLogShouldAggregateNewAndOpenIssueEntries() {
        ReportManagerHelper.setFailedTestsWithoutOpenIssuesCounter(1);
        ReportManagerHelper.setOpenIssuesForPassedTestsCounter(1);
        ReportManagerHelper.setOpenIssuesForFailedTestsCounter(1);

        List<List<String>> newIssues = Collections.synchronizedList(new ArrayList<>());
        newIssues.add(Arrays.asList("ClassA", "testOne"));
        ReportManagerHelper.setListOfNewIssuesForFailedTests(newIssues);

        List<List<String>> openPassedIssues = Collections.synchronizedList(new ArrayList<>());
        openPassedIssues.add(Arrays.asList("ClassB", "testTwo", "#2", "passed with historical issue"));
        ReportManagerHelper.setListOfOpenIssuesForPassedTests(openPassedIssues);

        List<List<String>> openFailedIssues = Collections.synchronizedList(new ArrayList<>());
        openFailedIssues.add(Arrays.asList("ClassC", "testThree", "#3", "failed with known issue"));
        ReportManagerHelper.setListOfOpenIssuesForFailedTests(openFailedIssues);

        String issuesSummary = ReportManagerHelper.prepareIssuesLog();

        SHAFT.Validations.assertThat().object(issuesSummary).contains("Total Issues = 3").perform();
        SHAFT.Validations.assertThat().object(issuesSummary).contains("New issues for Failed Tests = 1").perform();
        SHAFT.Validations.assertThat().object(issuesSummary).contains("Open issues for Passed Tests = 1").perform();
        SHAFT.Validations.assertThat().object(issuesSummary).contains("Open issues for Failed Tests = 1").perform();
        SHAFT.Validations.assertThat().object(ReportManagerHelper.getIssueCounter()).isEqualTo(3).perform();
    }

    @Test
    public void deduplicateConsecutiveLogLinesShouldKeepOnlyAdjacentUniqueLines() throws Exception {
        Method deduplicateMethod = ReportManagerHelper.class.getDeclaredMethod("deduplicateConsecutiveLogLines", byte[].class);
        deduplicateMethod.setAccessible(true);

        String rawLog = "line-1\nline-1\nline-2\nline-2\nline-1";
        byte[] deduplicated = (byte[]) deduplicateMethod.invoke(null, rawLog.getBytes(StandardCharsets.UTF_8));

        SHAFT.Validations.assertThat().object(new String(deduplicated, StandardCharsets.UTF_8))
                .isEqualTo("line-1" + System.lineSeparator() + "line-2" + System.lineSeparator() + "line-1")
                .perform();
    }

    @Test
    public void inferMimeTypeFromAttachmentShouldReturnExpectedMimeTypes() throws Exception {
        Method inferMimeTypeMethod = ReportManagerHelper.class.getDeclaredMethod("inferMimeTypeFromAttachment", String.class, String.class);
        inferMimeTypeMethod.setAccessible(true);

        String imagePng = (String) inferMimeTypeMethod.invoke(null, "Screenshot", "result.png");
        String gif = (String) inferMimeTypeMethod.invoke(null, "Animated GIF", "result");
        String mp4 = (String) inferMimeTypeMethod.invoke(null, "Recording", "video.mp4");
        String fallback = (String) inferMimeTypeMethod.invoke(null, "Unknown", "data.bin");

        SHAFT.Validations.assertThat().object(imagePng).isEqualTo("image/png").perform();
        SHAFT.Validations.assertThat().object(gif).isEqualTo("image/gif").perform();
        SHAFT.Validations.assertThat().object(mp4).isEqualTo("video/mp4").perform();
        SHAFT.Validations.assertThat().object(fallback).isEqualTo("text/plain").perform();
    }

    @Test
    public void getExecutionDurationShouldFormatDifferentTimeRanges() {
        SHAFT.Validations.assertThat().object(ReportManagerHelper.getExecutionDuration(0, 500)).isEqualTo("500 millis").perform();
        SHAFT.Validations.assertThat().object(ReportManagerHelper.getExecutionDuration(0, 1500)).isEqualTo("01 sec, 500 millis").perform();
        SHAFT.Validations.assertThat().object(ReportManagerHelper.getExecutionDuration(0, 121000)).isEqualTo("02 min, 01 sec").perform();
        SHAFT.Validations.assertThat().object(ReportManagerHelper.getExecutionDuration(0, 3660000)).isEqualTo("01 hr, 01 min").perform();
    }

    @Test
    public void formatStackTraceToLogEntryShouldIncludeCauseTrace() {
        Throwable throwable = new RuntimeException("outer", new IllegalArgumentException("inner"));

        String formattedTrace = ReportManagerHelper.formatStackTraceToLogEntry(throwable);

        SHAFT.Validations.assertThat().object(formattedTrace).contains("java.lang.RuntimeException: outer").perform();
        SHAFT.Validations.assertThat().object(formattedTrace).contains("Caused by: java.lang.IllegalArgumentException: inner").perform();
    }

    private static <T> T getPrivateStaticField(String fieldName, Class<T> type) throws Exception {
        Field field = ReportManagerHelper.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        return type.cast(field.get(null));
    }
}
