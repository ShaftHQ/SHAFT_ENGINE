package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.io.internal.IssueReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.mockito.Mockito;
import org.testng.ITestResult;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

@Test(singleThreaded = true)
public class ReportManagerHelperUnitTests {

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() throws Exception {
        resetReportManagerHelperState();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws Exception {
        resetReportManagerHelperState();
        Properties.clearForCurrentThread();
    }

    private void resetReportManagerHelperState() throws Exception {
        getPrivateStaticField("issuesLog", AtomicReference.class).set("");
        getPrivateStaticField("issueCounter", AtomicInteger.class).set(1);
        getPrivateStaticField("testCasesCounter", AtomicInteger.class).set(0);
        setPrivateStaticField("featureName", "");
        setPrivateStaticField("debugMode", false);
        invokePrivateStaticMethod("resetRetryDiagnosticLogging");
        ReportManagerHelper.setOpenIssuesForFailedTestsCounter(0);
        ReportManagerHelper.setOpenIssuesForPassedTestsCounter(0);
        ReportManagerHelper.setFailedTestsWithoutOpenIssuesCounter(0);
        ReportManagerHelper.setListOfOpenIssuesForFailedTests(Collections.synchronizedList(new ArrayList<>()));
        ReportManagerHelper.setListOfOpenIssuesForPassedTests(Collections.synchronizedList(new ArrayList<>()));
        ReportManagerHelper.setListOfNewIssuesForFailedTests(Collections.synchronizedList(new ArrayList<>()));
        ReportManagerHelper.setDiscreteLogging(false);
        ReportManagerHelper.setTotalNumberOfTests(0);
    }

    @Test
    public void prepareIssuesLogShouldReturnEmptyWhenNoIssuesExist() {
        String issuesSummary = ReportManagerHelper.prepareIssuesLog();

        SHAFT.Validations.assertThat().object(issuesSummary).isEqualTo("").perform();
        SHAFT.Validations.assertThat().object(ReportManagerHelper.getIssueCounter()).isEqualTo(0).perform();
    }

    @Test
    public void issueReporterShouldNotLogRetriedFailureAttempts() {
        ITestResult retriedFailure = Mockito.mock(ITestResult.class);
        Mockito.when(retriedFailure.wasRetried()).thenReturn(true);
        Mockito.when(retriedFailure.getStatus()).thenReturn(ITestResult.FAILURE);

        IssueReporter.updateIssuesLog(retriedFailure);

        SHAFT.Validations.assertThat().object(ReportManagerHelper.prepareIssuesLog()).isEqualTo("").perform();
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
    public void prepareIssuesLogShouldHandleEntriesWithoutIssueDescription() {
        ReportManagerHelper.setOpenIssuesForPassedTestsCounter(1);
        ReportManagerHelper.setOpenIssuesForFailedTestsCounter(1);

        List<List<String>> openPassedIssues = Collections.synchronizedList(new ArrayList<>());
        openPassedIssues.add(Arrays.asList("ClassB", "testTwo", "#2", ""));
        ReportManagerHelper.setListOfOpenIssuesForPassedTests(openPassedIssues);

        List<List<String>> openFailedIssues = Collections.synchronizedList(new ArrayList<>());
        openFailedIssues.add(Arrays.asList("ClassC", "testThree", "#3", ""));
        ReportManagerHelper.setListOfOpenIssuesForFailedTests(openFailedIssues);

        String issuesSummary = ReportManagerHelper.prepareIssuesLog();

        SHAFT.Validations.assertThat().object(issuesSummary).contains("Total Issues = 2").perform();
        SHAFT.Validations.assertThat().object(ReportManagerHelper.getIssueCounter()).isEqualTo(2).perform();
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
    public void deduplicateConsecutiveLogLinesShouldHandleEdgeCases() throws Exception {
        Method deduplicateMethod = ReportManagerHelper.class.getDeclaredMethod("deduplicateConsecutiveLogLines", byte[].class);
        deduplicateMethod.setAccessible(true);

        byte[] empty = (byte[]) deduplicateMethod.invoke(null, new byte[0]);
        byte[] singleLine = (byte[]) deduplicateMethod.invoke(null, "single".getBytes(StandardCharsets.UTF_8));
        byte[] onlyNewlines = (byte[]) deduplicateMethod.invoke(null, "\n\n".getBytes(StandardCharsets.UTF_8));

        SHAFT.Validations.assertThat().object(new String(empty, StandardCharsets.UTF_8)).isEqualTo("").perform();
        SHAFT.Validations.assertThat().object(new String(singleLine, StandardCharsets.UTF_8)).isEqualTo("single").perform();
        SHAFT.Validations.assertThat().object(new String(onlyNewlines, StandardCharsets.UTF_8)).isEqualTo("").perform();
    }

    @Test
    public void inferMimeTypeFromAttachmentShouldReturnExpectedMimeTypes() throws Exception {
        Method inferMimeTypeMethod = ReportManagerHelper.class.getDeclaredMethod("inferMimeTypeFromAttachment", String.class, String.class);
        inferMimeTypeMethod.setAccessible(true);

        String imagePng = (String) inferMimeTypeMethod.invoke(null, "Screenshot", "result.png");
        String gif = (String) inferMimeTypeMethod.invoke(null, "Animated GIF", "result");
        String mp4 = (String) inferMimeTypeMethod.invoke(null, "Recording", "video.mp4");
        String json = (String) inferMimeTypeMethod.invoke(null, "Response JSON", "payload.json");
        String xml = (String) inferMimeTypeMethod.invoke(null, "Response XML", "payload.xml");
        String csv = (String) inferMimeTypeMethod.invoke(null, "Report CSV", "report.csv");
        String html = (String) inferMimeTypeMethod.invoke(null, "Page Snapshot", "index.html");
        String fallback = (String) inferMimeTypeMethod.invoke(null, "Unknown", "data.bin");

        SHAFT.Validations.assertThat().object(imagePng).isEqualTo("image/png").perform();
        SHAFT.Validations.assertThat().object(gif).isEqualTo("image/gif").perform();
        SHAFT.Validations.assertThat().object(mp4).isEqualTo("video/mp4").perform();
        SHAFT.Validations.assertThat().object(json).isEqualTo("application/json").perform();
        SHAFT.Validations.assertThat().object(xml).isEqualTo("text/xml").perform();
        SHAFT.Validations.assertThat().object(csv).isEqualTo("text/csv").perform();
        SHAFT.Validations.assertThat().object(html).isEqualTo("text/html").perform();
        SHAFT.Validations.assertThat().object(fallback).isEqualTo("text/plain").perform();
    }

    @Test
    public void prepareIssuesLogShouldAggregateOpenIssuesWithNullDescriptions() {
        ReportManagerHelper.setOpenIssuesForPassedTestsCounter(1);
        ReportManagerHelper.setOpenIssuesForFailedTestsCounter(1);

        List<List<String>> openPassedIssues = Collections.synchronizedList(new ArrayList<>());
        openPassedIssues.add(Arrays.asList("ClassB", "testTwo", "#2", null));
        ReportManagerHelper.setListOfOpenIssuesForPassedTests(openPassedIssues);

        List<List<String>> openFailedIssues = Collections.synchronizedList(new ArrayList<>());
        openFailedIssues.add(Arrays.asList("ClassC", "testThree", "#3", null));
        ReportManagerHelper.setListOfOpenIssuesForFailedTests(openFailedIssues);

        String issuesSummary = ReportManagerHelper.prepareIssuesLog();
        String issueDetails = getPrivateStaticFieldUnchecked("issuesLog", AtomicReference.class).get().toString();

        SHAFT.Validations.assertThat().object(issuesSummary).contains("Total Issues = 2").perform();
        SHAFT.Validations.assertThat().object(issueDetails).contains("ClassB.testTwo").perform();
        SHAFT.Validations.assertThat().object(issueDetails).contains("ClassC.testThree").perform();
        SHAFT.Validations.assertThat().object(issueDetails).doesNotContain("'null'").perform();
    }

    @Test
    public void discreteLoggingToggleShouldPreserveAndRestoreImportantLogState() throws Exception {
        ReportManagerHelper.setDiscreteLogging(true);
        SHAFT.Validations.assertThat().object(ReportManagerHelper.getDiscreteLogging()).isTrue().perform();

        ReportManagerHelper.logImportantEntry("important log should preserve discrete logging state", Level.INFO);
        SHAFT.Validations.assertThat().object(ReportManagerHelper.getDiscreteLogging()).isTrue().perform();

        ReportManagerHelper.writeStepToReport("logger initialization branch");
        ReportManagerHelper.logImportantEntry("important logger initialization branch", Level.INFO);

        ReportManagerHelper.setDebugMode(true);
        ReportManagerHelper.setDiscreteLogging(false);
        SHAFT.Validations.assertThat().object(ReportManagerHelper.getDiscreteLogging()).isFalse().perform();

        ReportManagerHelper.setDiscreteLogging(true);
        SHAFT.Validations.assertThat().object(ReportManagerHelper.getDiscreteLogging()).isTrue().perform();
    }

    @Test
    public void internalDiscreteLogShouldAttachStringBuilderAndStreamAttachments() {
        ReportManagerHelper.setDiscreteLogging(true);
        List<List<Object>> attachments = new ArrayList<>();
        attachments.add(Arrays.asList("Attachment", "StringBuilder", new StringBuilder("builder-value")));
        attachments.add(Arrays.asList("Attachment", "Stream", new ByteArrayInputStream("stream-value".getBytes(StandardCharsets.UTF_8))));

        InternalshaftCaller.logPassedStepWithAttachments(attachments);

        SHAFT.Validations.assertThat().object(ReportManagerHelper.getDiscreteLogging()).isTrue().perform();
    }

    @Test
    public void attachmentMethodsShouldIgnoreNullEmptyAndEngineLogEntries() throws Exception {
        Method logAttachmentActionMethod = ReportManagerHelper.class.getDeclaredMethod(
                "logAttachmentAction", String.class, String.class, java.io.ByteArrayOutputStream.class);
        logAttachmentActionMethod.setAccessible(true);

        ReportManagerHelper.attach("Attachment", "Blank String", "   ");
        ReportManagerHelper.attach("Attachment", "Null Stream", (java.io.InputStream) null);
        ReportManagerHelper.attach((List<List<Object>>) null);
        ReportManagerHelper.attach(Collections.emptyList());
        ReportManagerHelper.attach(Collections.singletonList(null));
        ReportManagerHelper.attach(Collections.singletonList(Collections.emptyList()));
        logAttachmentActionMethod.invoke(null, "SHAFT Engine Logs", "engine", new java.io.ByteArrayOutputStream());

        SHAFT.Validations.assertThat().object(ReportManagerHelper.getIssueCounter()).isEqualTo(0).perform();
    }

    @Test
    public void debugFileLoggingShouldSkipDirectoryPathAndMissingLogAttachment() throws Exception {
        ReportManagerHelper.createLogEntry("prime logger before directory path check", Level.INFO);
        Path directoryPath = Files.createTempDirectory("rpmgr-debug-directory-");
        ThreadLocalPropertiesManager.setProperty("appender.file.fileName", directoryPath.toString());
        SHAFT.Properties.reporting.set().disableLogging(true);

        ReportManagerHelper.enableDebugFileLogging();

        boolean debugFileLoggingEnabled = isRetryDiagnosticLoggingEnabledForCurrentThread();
        SHAFT.Validations.assertThat().object(debugFileLoggingEnabled).isFalse().perform();
        SHAFT.Validations.assertThat().object(Files.isDirectory(directoryPath)).isTrue().perform();

        Path missingLog = directoryPath.resolve("missing.log");
        ThreadLocalPropertiesManager.setProperty("appender.file.fileName", missingLog.toString());
        ReportManagerHelper.enableDebugFileLogging();
        Files.deleteIfExists(missingLog);
        ReportManagerHelper.attachEngineLog("missing-log");

        debugFileLoggingEnabled = isRetryDiagnosticLoggingEnabledForCurrentThread();
        SHAFT.Validations.assertThat().object(debugFileLoggingEnabled).isFalse().perform();
        SHAFT.Validations.assertThat().object(Files.exists(missingLog)).isFalse().perform();
        Files.deleteIfExists(directoryPath);
    }

    @Test
    public void debugFileLoggingShouldCreateNestedTemporaryLogFile() throws Exception {
        Path debugDirectory = Files.createTempDirectory("rpmgr-debug-nested-");
        Path nestedLogFile = debugDirectory.resolve("nested").resolve("debug.log");
        ThreadLocalPropertiesManager.setProperty("appender.file.fileName", nestedLogFile.toString());

        ReportManagerHelper.enableDebugFileLogging();
        ReportManagerHelper.createLogEntry("nested debug entry", Level.DEBUG);

        SHAFT.Validations.assertThat().object(Files.isRegularFile(nestedLogFile)).isTrue().perform();
        waitForFileToContain(nestedLogFile, "nested debug entry");

        ReportManagerHelper.attachEngineLog("nested-log");
        SHAFT.Validations.assertThat().object(Files.exists(nestedLogFile)).isTrue().perform();
        Files.deleteIfExists(nestedLogFile);
        Files.deleteIfExists(nestedLogFile.getParent());
        Files.deleteIfExists(debugDirectory);
    }

    @Test
    public void bundledLog4jConfigurationShouldUseAsyncFileAppender() throws Exception {
        String bundledLog4jConfiguration;
        try (InputStream log4jConfiguration = Objects.requireNonNull(
                ReportManagerHelperUnitTests.class.getClassLoader()
                        .getResourceAsStream("properties/default/log4j2.properties"))) {
            bundledLog4jConfiguration = new String(log4jConfiguration.readAllBytes(), StandardCharsets.UTF_8);
        }

        SHAFT.Validations.assertThat().object(bundledLog4jConfiguration)
                .contains("appender.asyncFile.type=Async").perform();
        SHAFT.Validations.assertThat().object(bundledLog4jConfiguration)
                .contains("appender.asyncFile.appenderRef.file.ref=LOGFILE").perform();
        SHAFT.Validations.assertThat().object(bundledLog4jConfiguration)
                .contains("rootLogger=info, ASYNC_STDOUT, ASYNC_LOGFILE, ASYNC_REPORT_PORTAL").perform();
    }

    @Test
    public void nullAndEmptyHelpersShouldReturnSafeDefaults() throws Exception {
        Method addSpacingMethod = ReportManagerHelper.class.getDeclaredMethod("addSpacing", String.class);
        addSpacingMethod.setAccessible(true);
        Method getStepStatusMethod = ReportManagerHelper.class.getDeclaredMethod("getStepStatus", String.class);
        getStepStatusMethod.setAccessible(true);
        Method writeNestedStepsMethod = ReportManagerHelper.class.getDeclaredMethod("writeNestedStepsToReport", String.class);
        writeNestedStepsMethod.setAccessible(true);

        String formattedNullTrace = ReportManagerHelper.formatStackTraceToLogEntry(null);
        Object passedStatusForNullLog = getStepStatusMethod.invoke(null, new Object[]{null});
        String spacedEmpty = (String) addSpacingMethod.invoke(null, "");
        ReportManagerHelper.createLogEntry(null, Level.INFO);
        ReportManagerHelper.writeStepToReport(null);
        writeNestedStepsMethod.invoke(null, "nested empty-safe log");
        ReportManagerHelper.logNestedSteps("verification failed", Collections.singletonList("   "), Collections.emptyList());
        ReportManagerHelper.logNestedSteps("assertion failed", null, Collections.singletonList(Arrays.asList("Attachment", "Fallback", "fallback")));
        ReportManagerHelper.log("passed log without attachments", Collections.emptyList());

        SHAFT.Validations.assertThat().object(formattedNullTrace).isEqualTo("").perform();
        SHAFT.Validations.assertThat().object(passedStatusForNullLog.toString()).isEqualTo("PASSED").perform();
        SHAFT.Validations.assertThat().object(spacedEmpty).isNotNull().perform();
    }

    private static class InternalshaftCaller {
        private static void logPassedStepWithAttachments(List<List<Object>> attachments) {
            ReportManagerHelper.log("passed internal step", attachments);
        }
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

    @Test
    public void getCallingMethodsShouldReturnCurrentTestContext() {
        String callingMethod = ReportManagerHelper.getCallingMethodFullName();
        String callingClass = ReportManagerHelper.getCallingClassFullName();

        SHAFT.Validations.assertThat().object(callingMethod).contains("ReportManagerHelperUnitTests.getCallingMethodsShouldReturnCurrentTestContext").perform();
        SHAFT.Validations.assertThat().object(callingClass).contains("testPackage.unitTests.ReportManagerHelperUnitTests").perform();
    }

    @Test
    public void testNGContextHelpersShouldReturnCurrentMethodData() {
        String currentClassName = ReportManagerHelper.getTestClassName();
        String currentMethodName = ReportManagerHelper.getTestMethodName();
        Boolean testPassed = ReportManagerHelper.isCurrentTestPassed();

        SHAFT.Validations.assertThat().object(currentClassName).contains("testPackage.unitTests.ReportManagerHelperUnitTests").perform();
        SHAFT.Validations.assertThat().object(currentMethodName).isEqualTo("testNGContextHelpersShouldReturnCurrentMethodData").perform();
        SHAFT.Validations.assertThat().object(testPassed).isNotNull().perform();
    }

    @Test
    public void loggingAndAttachmentMethodsShouldExecuteWithoutThrowing() throws Exception {
        int initialTestCaseCounter = getPrivateStaticField("testCasesCounter", AtomicInteger.class).get();
        ReportManagerHelper.setDebugMode(true);
        ReportManagerHelper.setTotalNumberOfTests(4);
        ReportManagerHelper.setFeatureName("Feature A");

        ReportManagerHelper.logTestInformation("ClassX", "methodX", "description");
        ReportManagerHelper.logScenarioInformation("Scenario", "Scenario Name", "Given step");
        ReportManagerHelper.logConfigurationMethodInformation("ClassX", "beforeMethod", "beforeMethod");
        ReportManagerHelper.logExecutionSummary("4", "3", "1", "0");
        SHAFT.Properties.reporting.set().disableLogging(true);
        ReportManagerHelper.logFinishedTestInformation("ClassX", "methodX", "description", "FAILED");
        ReportManagerHelper.logFinishedTestInformation("ClassX", "methodX", "", "SKIPPED");
        SHAFT.Properties.reporting.set().disableLogging(false);
        ReportManagerHelper.logFinishedTestInformation("ClassX", "methodX", "", "PASSED");
        ReportManagerHelper.logFinishedTestInformation("ClassX", "methodX", "", "BROKEN");
        ReportManagerHelper.logEngineVersion();
        ReportManagerHelper.logEngineClosure();

        ReportManagerHelper.setTestCaseName("Scenario Name");
        ReportManagerHelper.setTestCaseDescription("Given و Then");
        ReportManagerHelper.setTestCaseDescription("Given Then");
        ReportManagerHelper.writeStepToReport("passed step");
        ReportManagerHelper.writeStepToReport("failed step", Level.INFO);
        ReportManagerHelper.logImportantEntry("important log", Level.INFO);

        ReportManagerHelper.attach("Attachment", "From String", "value");
        ReportManagerHelper.attach("Attachment", "From InputStream", new ByteArrayInputStream("value".getBytes(StandardCharsets.UTF_8)));
        ReportManagerHelper.attachTestLog("methodX", "test-log");
        ReportManagerHelper.attachTestLog("methodX", "");
        ReportManagerHelper.attachAsStep("Attachment", "As Step", new ByteArrayInputStream("step".getBytes(StandardCharsets.UTF_8)));

        List<List<Object>> attachments = new ArrayList<>();
        attachments.add(Arrays.asList("Attachment", "String", "value"));
        attachments.add(Arrays.asList("Attachment", "Bytes", "bytes".getBytes(StandardCharsets.UTF_8)));
        attachments.add(Arrays.asList("Attachment", "Stream", new ByteArrayInputStream("stream".getBytes(StandardCharsets.UTF_8))));
        ReportManagerHelper.attach(attachments);

        List<String> customLogMessages = new ArrayList<>();
        customLogMessages.add("custom log");
        ReportManagerHelper.log("passed log", attachments);
        ReportManagerHelper.log("failed log", attachments);
        ReportManagerHelper.logNestedSteps("verification passed", customLogMessages, attachments);
        ReportManagerHelper.logNestedSteps("verification failed", customLogMessages, attachments);
        ReportManagerHelper.logNestedSteps("assertion passed", customLogMessages, null);
        ReportManagerHelper.logNestedSteps("assertion failed", customLogMessages, null);
        ReportManagerHelper.logNestedSteps("assertion passed", attachments);
        ReportManagerHelper.log(new RuntimeException("runtime exception"));
        ReportManagerHelper.log(new RuntimeException());
        SHAFT.Properties.reporting.set().disableLogging(true);
        ReportManagerHelper.logDiscrete(new RuntimeException("discrete exception"));
        ReportManagerHelper.logDiscrete(new RuntimeException("discrete exception warning"), Level.WARN);
        SHAFT.Properties.reporting.set().disableLogging(false);
        ReportManagerHelper.logDiscrete("discrete text", Level.INFO);

        int testCaseCounter = getPrivateStaticField("testCasesCounter", AtomicInteger.class).get();
        SHAFT.Validations.assertThat().number(testCaseCounter - initialTestCaseCounter).isGreaterThanOrEquals(2).perform();
    }

    @Test
    public void debugFileLoggingShouldCreateAndAttachEngineLog() throws Exception {
        String uniqueLogFilePath = "target/logs/report-manager-helper-" + System.nanoTime() + ".log";
        ThreadLocalPropertiesManager.setProperty("appender.file.fileName", uniqueLogFilePath);
        Method getLogFilePathMethod = ReportManagerHelper.class.getDeclaredMethod("getLogFilePath");
        getLogFilePathMethod.setAccessible(true);
        String logFilePath = (String) getLogFilePathMethod.invoke(null);

        ReportManagerHelper.enableDebugFileLogging();
        ReportManagerHelper.writeStepToReport("debug log entry");
        waitForFileToContain(Path.of(logFilePath), "debug log entry");
        SHAFT.Validations.assertThat().object(new File(logFilePath).isFile()).isTrue().perform();
        ReportManagerHelper.attachEngineLog("execution-end");

        boolean debugFileLoggingEnabled = isRetryDiagnosticLoggingEnabledForCurrentThread();
        SHAFT.Validations.assertThat().object(debugFileLoggingEnabled).isFalse().perform();
        SHAFT.Validations.assertThat().object(new File(logFilePath).exists()).isTrue().perform();
        Files.deleteIfExists(Path.of(logFilePath));
    }

    @Test
    public void resettingRetryDiagnosticsShouldNotClearAnotherThreadSession() throws Exception {
        Path workerLog = Path.of("target", "logs", "report-manager-helper-worker-" + System.nanoTime() + ".log");
        CountDownLatch workerEnabled = new CountDownLatch(1);
        CountDownLatch mainResetComplete = new CountDownLatch(1);
        AtomicReference<Boolean> workerSessionActiveAfterMainReset = new AtomicReference<>(false);
        AtomicReference<Throwable> workerFailure = new AtomicReference<>();

        Thread worker = new Thread(() -> {
            try {
                ThreadLocalPropertiesManager.setProperty("appender.file.fileName", workerLog.toString());
                ReportManagerHelper.enableDebugFileLogging();
                workerEnabled.countDown();
                mainResetComplete.await();
                workerSessionActiveAfterMainReset.set(isRetryDiagnosticLoggingEnabledForCurrentThread());
                invokePrivateStaticMethod("resetRetryDiagnosticLogging");
            } catch (Throwable throwable) {
                workerFailure.set(throwable);
            } finally {
                workerEnabled.countDown();
                Properties.clearForCurrentThread();
            }
        }, "retry-diagnostics-worker");

        worker.setDaemon(true);
        worker.start();
        boolean workerReachedEnabledState = workerEnabled.await(5, TimeUnit.SECONDS);
        try {
            if (workerReachedEnabledState) {
                invokePrivateStaticMethod("resetRetryDiagnosticLogging");
            }
        } finally {
            mainResetComplete.countDown();
        }
        worker.join(5000);

        try {
            SHAFT.Validations.assertThat().object(workerReachedEnabledState).isTrue().perform();
            SHAFT.Validations.assertThat().object(worker.isAlive()).isFalse().perform();
            SHAFT.Validations.assertThat().object(workerFailure.get()).isNull().perform();
            SHAFT.Validations.assertThat().object(workerSessionActiveAfterMainReset.get()).isTrue().perform();
        } finally {
            worker.interrupt();
            Files.deleteIfExists(workerLog);
        }
    }

    @Test
    public void privateHelpersShouldReturnExpectedValues() throws Exception {
        Method createSeparatorMethod = ReportManagerHelper.class.getDeclaredMethod("createSeparator", char.class);
        createSeparatorMethod.setAccessible(true);
        Method addSpacingMethod = ReportManagerHelper.class.getDeclaredMethod("addSpacing", String.class);
        addSpacingMethod.setAccessible(true);
        Method getStepStatusMethod = ReportManagerHelper.class.getDeclaredMethod("getStepStatus", String.class);
        getStepStatusMethod.setAccessible(true);
        int separatorWidth = getPrivateStaticField("SEPARATOR_WIDTH", Integer.class);

        String separator = (String) createSeparatorMethod.invoke(null, '=');
        String doubleLineSeparator = (String) createSeparatorMethod.invoke(null, '═');
        String spaced = (String) addSpacingMethod.invoke(null, "one\ntwo");
        Object failedStatus = getStepStatusMethod.invoke(null, "step failed");

        SHAFT.Validations.assertThat().object(separator.length()).isEqualTo(separatorWidth).perform();
        SHAFT.Validations.assertThat().object(doubleLineSeparator.length()).isEqualTo(separatorWidth).perform();
        SHAFT.Validations.assertThat().object(spaced).contains("one").perform();
        SHAFT.Validations.assertThat().object(failedStatus.toString()).isEqualTo("FAILED").perform();
    }

    @Test
    public void noOpPublicMethodsShouldExecute() {
        ReportManagerHelper.cleanExecutionSummaryReportDirectory();
        ReportManagerHelper.openExecutionSummaryReportAfterExecution();
        boolean internalStep = ReportManagerHelper.isInternalStep();
        SHAFT.Validations.assertThat().object(internalStep).isFalse().perform();
    }

    @Test
    public void attachIssuesLogShouldAttachWhenIssuesExist() {
        ReportManagerHelper.setFailedTestsWithoutOpenIssuesCounter(1);
        List<List<String>> newIssues = Collections.synchronizedList(new ArrayList<>());
        newIssues.add(Arrays.asList("ClassA", "testOne"));
        ReportManagerHelper.setListOfNewIssuesForFailedTests(newIssues);

        ReportManagerHelper.attachIssuesLog("execution-end");

        SHAFT.Validations.assertThat().object(ReportManagerHelper.getIssueCounter()).isEqualTo(1).perform();
        SHAFT.Validations.assertThat().object(getPrivateStaticFieldUnchecked("issuesLog", AtomicReference.class).get())
                .contains("ClassA.testOne").perform();
    }

    @Test
    public void utilityConstructorShouldThrowIllegalStateException() throws Exception {
        Constructor<ReportManagerHelper> constructor = ReportManagerHelper.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        InvocationTargetException invocationTargetException = null;
        try {
            constructor.newInstance();
        } catch (InvocationTargetException e) {
            invocationTargetException = e;
        }
        SHAFT.Validations.assertThat().object(invocationTargetException).isNotNull().perform();
        SHAFT.Validations.assertThat().object(invocationTargetException.getCause().getClass().getName()).isEqualTo("java.lang.IllegalStateException").perform();
        SHAFT.Validations.assertThat().object(invocationTargetException.getCause().getMessage()).isEqualTo("Utility class").perform();
    }

    private static Object invokePrivateStaticMethod(String methodName, Object... args) throws Exception {
        Class<?>[] parameterTypes = Arrays.stream(args)
                .map(arg -> arg == null ? Object.class : arg.getClass())
                .toArray(Class<?>[]::new);
        Method method;
        if ("writeToDebugLogFileIfEnabled".equals(methodName)) {
            method = ReportManagerHelper.class.getDeclaredMethod(methodName, String.class, Level.class);
        } else {
            method = ReportManagerHelper.class.getDeclaredMethod(methodName, parameterTypes);
        }
        method.setAccessible(true);
        return method.invoke(null, args);
    }

    private static void waitForFileToContain(Path path, String expectedText) throws Exception {
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
        while (System.nanoTime() < deadline) {
            if (Files.exists(path) && Files.readString(path, StandardCharsets.UTF_8).contains(expectedText)) {
                return;
            }
            Thread.sleep(50);
        }
        SHAFT.Validations.assertThat().object(Files.exists(path)).isTrue().perform();
        SHAFT.Validations.assertThat().object(Files.readString(path, StandardCharsets.UTF_8))
                .contains(expectedText).perform();
    }

    private static <T> T getPrivateStaticField(String fieldName, Class<T> type) throws Exception {
        Field field = ReportManagerHelper.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        return type.cast(field.get(null));
    }

    private static void setPrivateStaticField(String fieldName, Object value) throws Exception {
        Field field = ReportManagerHelper.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(null, value);
    }

    private static boolean isRetryDiagnosticLoggingEnabledForCurrentThread() throws Exception {
        Method method = ReportManagerHelper.class
                .getDeclaredMethod("isRetryDiagnosticLoggingEnabledForCurrentThread");
        method.setAccessible(true);
        return (boolean) method.invoke(null);
    }

    private static <T> T getPrivateStaticFieldUnchecked(String fieldName, Class<T> type) {
        try {
            return getPrivateStaticField(fieldName, type);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
