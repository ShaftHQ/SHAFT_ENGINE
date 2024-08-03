package com.shaft.listeners;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.listeners.internal.*;
import com.shaft.properties.internal.PropertiesHelper;
import com.shaft.tools.internal.security.GoogleTink;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.*;
import io.qameta.allure.Allure;
import lombok.Getter;
import org.testng.*;
import org.testng.annotations.ITestAnnotation;
import org.testng.xml.XmlSuite;
import org.testng.xml.XmlTest;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Stream;

public class TestNGListener implements IAlterSuiteListener, IAnnotationTransformer,
        IExecutionListener, ISuiteListener, IInvokedMethodListener, ITestListener {

    private static final List<ITestNGMethod> passedTests = new ArrayList<>();
    private static final List<ITestNGMethod> failedTests = new ArrayList<>();
    private static final List<ITestNGMethod> skippedTests = new ArrayList<>();
    @Getter
    private static ITestResult iTestResult;
    private static long executionStartTime;

    @Getter
    private static XmlTest xmlTest;

    private static Thread allureEnvironmentSetup;

    public static ProjectStructureManager.RunType identifyRunType() {
        Supplier<Stream<?>> stacktraceSupplier = () -> Arrays.stream((new Throwable()).getStackTrace()).map(StackTraceElement::getClassName);
        var isUsingJunitDiscovery = stacktraceSupplier.get().anyMatch(org.junit.platform.launcher.core.EngineDiscoveryOrchestrator.class.getCanonicalName()::equals);
        var isUsingTestNG = stacktraceSupplier.get().anyMatch(TestNG.class.getCanonicalName()::equals);
        var isUsingCucumber = stacktraceSupplier.get().anyMatch(io.cucumber.core.runner.Runner.class.getCanonicalName()::equals);
        if (isUsingJunitDiscovery || isUsingTestNG) {
            System.out.println("TestNG run detected...");
            return ProjectStructureManager.RunType.TESTNG;
        } else if (isUsingCucumber) {
            System.out.println("Cucumber run detected...");
            return ProjectStructureManager.RunType.CUCUMBER;
        } else {
            System.out.println("JUnit5 run detected...");
            return ProjectStructureManager.RunType.JUNIT;
        }
    }

    public static void engineSetup(ProjectStructureManager.RunType runType) {
        Allure.getLifecycle();
        Reporter.setEscapeHtml(false);
        ReportManagerHelper.setDiscreteLogging(true);
        PropertiesHelper.initialize();
        ReportManager.logDiscrete("Initializing Engine Setup...");
        SHAFT.Properties.reporting.set().disableLogging(true);
        switch (runType) {
            case TESTNG ->
                    Thread.ofVirtual().start(() -> ProjectStructureManager.initialize(ProjectStructureManager.RunType.TESTNG));
            case CUCUMBER ->
                    Thread.ofVirtual().start(() -> ProjectStructureManager.initialize(ProjectStructureManager.RunType.CUCUMBER));
            case JUNIT ->
                    Thread.ofVirtual().start(() -> ProjectStructureManager.initialize(ProjectStructureManager.RunType.JUNIT));
        }
        TestNGListenerHelper.configureJVMProxy();
        Thread.ofVirtual().start(() -> {
            GoogleTink.initialize();
            GoogleTink.decrypt();
        });
        SHAFT.Properties.reporting.set().disableLogging(false);
        ReportManagerHelper.logEngineVersion();
        Thread.ofVirtual().start(UpdateChecker::check);
        Thread.ofVirtual().start(ImageProcessingActions::loadOpenCV);
        allureEnvironmentSetup = Thread.ofVirtual().start(AllureManager::initializeAllureReportingEnvironment);
        Thread.ofVirtual().start(ReportManagerHelper::cleanExecutionSummaryReportDirectory);
        ReportManagerHelper.setDiscreteLogging(SHAFT.Properties.reporting.alwaysLogDiscreetly());
        ReportManagerHelper.setDebugMode(SHAFT.Properties.reporting.debugMode());
    }

    /**
     * gets invoked before TestNG proceeds with invoking any other listener.
     */
    @Override
    public void onExecutionStart() {
        engineSetup(ProjectStructureManager.RunType.TESTNG);
    }

    /**
     * Implementations of this interface will gain access to the {@link XmlSuite} object and thus let
     * users be able to alter a suite or a test based on their own needs.
     *
     * @param suites - The list of {@link XmlSuite}s that are part of the current execution.
     */
    @Override
    public void alter(List<XmlSuite> suites) {
        switch (TestNGListener.identifyRunType()) {
            case TESTNG -> TestNGListenerHelper.configureTestNGProperties(suites);
            case CUCUMBER -> CucumberHelper.configureCucumberProperties(suites);
        }
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
        executionStartTime = System.currentTimeMillis();
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
    public void beforeInvocation(IInvokedMethod method, ITestResult iTestResult, ITestContext iTestContext) {
        var elapsedTime = System.currentTimeMillis() - executionStartTime;
        if (SHAFT.Properties.reporting.debugMode()) {
            ReportManager.logDiscrete("elapsedTime: " + elapsedTime + "ms");
        }
        if (elapsedTime >= SHAFT.Properties.testNG.testSuiteTimeout() * 60000) {
            throw new SkipException("Skipping method as the test suite has exceeded the defined timeout of " + SHAFT.Properties.testNG.testSuiteTimeout() + " minutes.");
        }
        xmlTest = method.getTestMethod().getXmlTest();
        JiraHelper.prepareTestResultAttributes(method, iTestResult);
        TestNGListenerHelper.setTestName(iTestContext);
        TestNGListenerHelper.logTestInformation(iTestResult);
        TestNGListenerHelper.failFast(iTestResult);
        TestNGListenerHelper.skipTestsWithLinkedIssues(iTestResult);
        TestNGListener.iTestResult = iTestResult;
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
        TestNGListenerHelper.updateTestMethods(iTestResult);
//            TestNGListenerHelper.updateConfigurationMethodLogs(iTestResult);
        TestNGListenerHelper.logFinishedTestInformation(iTestResult);
        ReportManagerHelper.setDiscreteLogging(SHAFT.Properties.reporting.alwaysLogDiscreetly());
    }

    /**
     * gets invoked at the very last (after attachTestArtifacts generation phase), before TestNG exits the JVM.
     */
    @Override
    public void onExecutionFinish() {
        ReportManagerHelper.setDiscreteLogging(true);
        Thread.ofVirtual().start(() -> ExecutionSummaryReport.generateExecutionSummaryReport(passedTests.size(), failedTests.size(), skippedTests.size(), executionStartTime, System.currentTimeMillis()));
        Thread.ofVirtual().start(JiraHelper::reportExecutionStatusToJira);
        Thread.ofVirtual().start(GoogleTink::encrypt);
        ReportManagerHelper.logEngineClosure();
        try {
            allureEnvironmentSetup.join();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        AllureManager.openAllureReportAfterExecution();
        AllureManager.generateAllureReportArchive();
    }

    @Override
    public void onTestSuccess(ITestResult result) {
        passedTests.add(result.getMethod());
        ExecutionSummaryReport.casesDetailsIncrement(TestNGListenerHelper.getTmsLinkAnnotationValue(result), result.getMethod().getQualifiedName().replace("." + result.getMethod().getMethodName(), ""),
                result.getMethod().getMethodName(), result.getMethod().getDescription(), "",
                ExecutionSummaryReport.StatusIcon.PASSED.getValue() + ExecutionSummaryReport.Status.PASSED.name(), TestNGListenerHelper.getIssueAnnotationValue(result));
    }

    @Override
    public void onTestFailure(ITestResult result) {
        failedTests.add(result.getMethod());
        ExecutionSummaryReport.casesDetailsIncrement(TestNGListenerHelper.getTmsLinkAnnotationValue(result), result.getMethod().getQualifiedName().replace("." + result.getMethod().getMethodName(), ""),
                result.getMethod().getMethodName(), result.getMethod().getDescription(), result.getThrowable().getMessage(),
                ExecutionSummaryReport.StatusIcon.FAILED.getValue() + ExecutionSummaryReport.Status.FAILED.name(), TestNGListenerHelper.getIssueAnnotationValue(result));
    }

    @Override
    public void onTestSkipped(ITestResult result) {
        skippedTests.add(result.getMethod());
        ExecutionSummaryReport.casesDetailsIncrement(TestNGListenerHelper.getTmsLinkAnnotationValue(result), result.getMethod().getQualifiedName().replace("." + result.getMethod().getMethodName(), ""),
                result.getMethod().getMethodName(), result.getMethod().getDescription(), result.getThrowable().getMessage(),
                ExecutionSummaryReport.StatusIcon.SKIPPED.getValue() + ExecutionSummaryReport.Status.SKIPPED.name(), TestNGListenerHelper.getIssueAnnotationValue(result));
    }
}