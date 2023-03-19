package io.github.shafthq.shaft.listeners;

import io.github.shafthq.shaft.gui.image.ImageProcessingActions;
import io.github.shafthq.shaft.properties.PropertiesHelper;
import io.github.shafthq.shaft.tools.io.ExecutionSummaryReport;
import io.github.shafthq.shaft.tools.io.IssueReporter;
import io.github.shafthq.shaft.tools.io.ProjectStructureManager;
import io.github.shafthq.shaft.tools.io.ReportManagerHelper;
import io.github.shafthq.shaft.tools.security.GoogleTink;
import io.qameta.allure.Allure;
import lombok.Getter;
import org.testng.*;
import org.testng.annotations.ITestAnnotation;
import org.testng.xml.XmlSuite;
import org.testng.xml.XmlTest;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

public class TestNGListener implements IAlterSuiteListener, IAnnotationTransformer,
        IExecutionListener, ISuiteListener, IInvokedMethodListener, ITestListener {

    private static List<ITestNGMethod> passedTests = new ArrayList<ITestNGMethod>();
    private static List<ITestNGMethod> failedTests = new ArrayList<ITestNGMethod>();
    private static List<ITestNGMethod> skippedTests = new ArrayList<ITestNGMethod>();

    private static String executionStartTime = "";
    private static long executionStartTime2;
    private static String executionEndTime = "";
    private static long executionEndTime2;

    @Getter
    private static XmlTest xmlTest;

    /**
     * gets invoked before TestNG proceeds with invoking any other listener.
     */
    @Override
    public void onExecutionStart() {
        ReportManagerHelper.setDiscreteLogging(true);
        System.setProperty("disableLogging", "true");
        //TODO: Enable Properties Helper and refactor the old PropertyFileManager to read any unmapped user properties in a specific directory
        Allure.getLifecycle();
        Reporter.setEscapeHtml(false);
        PropertiesHelper.initialize();
        ProjectStructureManager.initialize();
        TestNGListenerHelper.configureJVMProxy();
        GoogleTink.initialize();
        GoogleTink.decrypt();
        System.setProperty("disableLogging", "false");

        ReportManagerHelper.logEngineVersion();
        ImageProcessingActions.loadOpenCV();

        ReportManagerHelper.cleanExecutionSummaryReportDirectory();
        ReportManagerHelper.initializeAllureReportingEnvironment();
        ReportManagerHelper.initializeExtentReportingEnvironment();

        ReportManagerHelper.setDiscreteLogging(Boolean.parseBoolean(System.getProperty("alwaysLogDiscreetly")));
        ReportManagerHelper.setDebugMode(Boolean.valueOf(System.getProperty("debugMode")));
    }

    /**
     * Implementations of this interface will gain access to the {@link XmlSuite} object and thus let
     * users be able to alter a suite or a test based on their own needs.
     *
     * @param suites - The list of {@link XmlSuite}s that are part of the current execution.
     */
    @Override
    public void alter(List<XmlSuite> suites) {
        TestNGListenerHelper.configureTestNGProperties(suites);
        TestNGListenerHelper.updateDefaultSuiteAndTestNames(suites);
        TestNGListenerHelper.attachConfigurationHelperClass(suites);
        //All alterations should be finalized before duplicating the
        //test suites for cross browser execution
        TestNGListenerHelper.configureCrossBrowserExecution(suites);
    }

    /**
     * This method is invoked before the SuiteRunner starts.
     *
     * @param suite The suite
     */
    @Override
    public void onStart(ISuite suite) {
        TestNGListenerHelper.setTotalNumberOfTests(suite);
        executionStartTime = new SimpleDateFormat("HH:mm:ss").format(System.currentTimeMillis());
        executionStartTime2 = System.currentTimeMillis();
    }

    /**
     * This method will be invoked by TestNG to give you a chance to modify a TestNG annotation read
     * from your test classes. You can change the values you need by calling any of the setters on the
     * ITest interface.
     *
     * <p>Note that only one of the three parameters testClass, testConstructor and testMethod will be
     * non-null.
     *
     * @param annotation      The annotation that was read from your test class.
     * @param testClass       If the annotation was found on a class, this parameter represents this class
     *                        (null otherwise).
     * @param testConstructor If the annotation was found on a constructor, this parameter represents
     *                        this constructor (null otherwise).
     * @param testMethod      If the annotation was found on a method, this parameter represents this
     *                        method (null otherwise).
     */
    @Override
    public void transform(ITestAnnotation annotation, Class testClass, Constructor testConstructor, Method testMethod) {
        annotation.setRetryAnalyzer(RetryAnalyzer.class);
    }

    /**
     * A listener that gets invoked before a method is invoked by TestNG. This listener will
     * be invoked for configuration and test methods irrespective of whether they pass/fail or get
     * skipped. This listener invocation can be disabled for SKIPPED tests through one of the below
     * mechanisms:
     *
     * <ul>
     *   <li>Command line parameter <code>alwaysRunListeners</code>
     *   <li>Build tool
     *   <li>Via {@code TestNG.alwaysRunListeners(false)}
     * </ul>
     */
    @Override
    public void beforeInvocation(IInvokedMethod method, ITestResult iTestResult, ITestContext context) {
        xmlTest = method.getTestMethod().getXmlTest();
        JiraHelper.prepareTestResultAttributes(method, iTestResult);
        TestNGListenerHelper.logTestInformation(iTestResult);
        TestNGListenerHelper.failFast(iTestResult);
        TestNGListenerHelper.skipTestsWithLinkedIssues(iTestResult);
    }

    /**
     * A listener that gets invoked after a method is invoked by TestNG. This listener will
     * be invoked for configuration and test methods irrespective of whether they pass/fail or get
     * skipped. This listener invocation can be disabled for SKIPPED tests through one of the below
     * mechanisms:
     *
     * <ul>
     *   <li>Command line parameter <code>alwaysRunListeners</code>
     *   <li>Build tool
     *   <li>Via {@code TestNG.alwaysRunListeners(false)}
     * </ul>
     */
    @Override
    public void afterInvocation(IInvokedMethod iInvokedMethod, ITestResult iTestResult, ITestContext iTestContext) {
        IssueReporter.updateTestStatusInCaseOfVerificationFailure(iTestResult);
        IssueReporter.updateIssuesLog(iTestResult);
        TestNGListenerHelper.updateConfigurationMethodLogs(iTestResult);
        ReportManagerHelper.setDiscreteLogging(Boolean.parseBoolean(System.getProperty("alwaysLogDiscreetly")));
    }

    /**
     * gets invoked at the very last (after attachTestArtifacts generation phase), before TestNG exits the JVM.
     */
    @Override
    public void onExecutionFinish() {
        ReportManagerHelper.setDiscreteLogging(true);
        JiraHelper.reportExecutionStatusToJira();
        GoogleTink.encrypt();
        ReportManagerHelper.generateAllureReportArchive();
        ReportManagerHelper.openAllureReportAfterExecution();
        executionEndTime = new SimpleDateFormat("HH:mm:ss").format(System.currentTimeMillis());
        executionEndTime2 = System.currentTimeMillis();
        ExecutionSummaryReport.generateExecutionSummaryReport(passedTests.size(), failedTests.size(), skippedTests.size(), executionStartTime, executionEndTime, executionStartTime2, executionEndTime2);
        ReportManagerHelper.logEngineClosure();
    }

    @Override
    public void onTestSuccess(ITestResult result) {
        passedTests.add(result.getMethod());
    }

    @Override
    public void onTestFailure(ITestResult result) {
        failedTests.add(result.getMethod());
        ExecutionSummaryReport.casesDetailsIncrement(result.getMethod().getQualifiedName().replace("." + result.getMethod().getMethodName(), ""),
                result.getMethod().getMethodName(), result.getMethod().getDescription(),
                ExecutionSummaryReport.ExecutionSummaryReportStatusIcon.FAILED.getValue() + ExecutionSummaryReport.ExecutionSummaryReportStatus.FAILED.name());
    }

    @Override
    public void onTestSkipped(ITestResult result) {
        skippedTests.add(result.getMethod());
        ExecutionSummaryReport.casesDetailsIncrement(result.getMethod().getQualifiedName().replace("." + result.getMethod().getMethodName(), ""),
                result.getMethod().getMethodName(), result.getMethod().getDescription(),
                ExecutionSummaryReport.ExecutionSummaryReportStatusIcon.SKIPPED.getValue() + ExecutionSummaryReport.ExecutionSummaryReportStatus.SKIPPED.name());
    }

}