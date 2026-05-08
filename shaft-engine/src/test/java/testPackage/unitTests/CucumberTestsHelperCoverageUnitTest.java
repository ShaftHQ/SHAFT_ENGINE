package testPackage.unitTests;

import com.shaft.listeners.TestNGListener;
import com.shaft.listeners.internal.CucumberHelper;
import com.shaft.listeners.internal.JiraHelper;
import com.shaft.properties.internal.ThreadLocalPropertiesManager;
import com.shaft.tools.internal.security.GoogleTink;
import com.shaft.tools.io.internal.*;
import io.cucumber.plugin.event.Status;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.ITestResult;
import org.testng.Reporter;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import org.testng.xml.XmlSuite;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

@Test(singleThreaded = true)
public class CucumberTestsHelperCoverageUnitTest {

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        ThreadLocalPropertiesManager.setGlobalProperty("cucumber.options", "");
    }

    @Test
    public void utilityConstructorShouldThrowIllegalStateException() throws Exception {
        Constructor<CucumberHelper> constructor = CucumberHelper.class.getDeclaredConstructor();
        constructor.setAccessible(true);

        InvocationTargetException exception = Assert.expectThrows(InvocationTargetException.class, constructor::newInstance);
        Assert.assertTrue(exception.getCause() instanceof IllegalStateException);
    }

    @Test
    public void configureCucumberPropertiesShouldPopulateAllExpectedParameters() {
        XmlSuite suite = new XmlSuite();
        suite.setParameters(new HashMap<>(Map.of("existing", "value")));

        CucumberHelper.configureCucumberProperties(List.of(suite));

        Map<String, String> parameters = suite.getParameters();
        Assert.assertEquals(parameters.get("existing"), "value");
        Assert.assertEquals(parameters.get("cucumber.ansi-colors.disabled"), String.valueOf(com.shaft.driver.SHAFT.Properties.cucumber.cucumberAnsiColorsDisabled()));
        Assert.assertEquals(parameters.get("cucumber.execution.dry-run"), String.valueOf(com.shaft.driver.SHAFT.Properties.cucumber.cucumberExecutionDryRun()));
        Assert.assertEquals(parameters.get("cucumber.execution.limit"), com.shaft.driver.SHAFT.Properties.cucumber.cucumberExecutionLimit());
        Assert.assertEquals(parameters.get("cucumber.execution.order"), com.shaft.driver.SHAFT.Properties.cucumber.cucumberExecutionOrder());
        Assert.assertEquals(parameters.get("cucumber.execution.wip"), String.valueOf(com.shaft.driver.SHAFT.Properties.cucumber.cucumberExecutionWip()));
        Assert.assertEquals(parameters.get("cucumber.features"), com.shaft.driver.SHAFT.Properties.cucumber.cucumberFeatures());
        Assert.assertEquals(parameters.get("cucumber.filter.name"), com.shaft.driver.SHAFT.Properties.cucumber.cucumberFilterName());
        Assert.assertEquals(parameters.get("cucumber.filter.tags"), com.shaft.driver.SHAFT.Properties.cucumber.cucumberFilterTags());
        Assert.assertEquals(parameters.get("cucumber.glue"), com.shaft.driver.SHAFT.Properties.cucumber.cucumberGlue());
        Assert.assertEquals(parameters.get("cucumber.plugin"), com.shaft.driver.SHAFT.Properties.cucumber.cucumberPlugin());
        Assert.assertEquals(parameters.get("cucumber.object-factory"), com.shaft.driver.SHAFT.Properties.cucumber.cucumberObjectFactory());
        Assert.assertEquals(parameters.get("cucumber.snippet-type"), com.shaft.driver.SHAFT.Properties.cucumber.cucumberSnippetType());
    }

    @Test
    public void engineSetupShouldResetStateAndSetLegacyCucumberOptions() throws Exception {
        setExecutionTimestamp("executionStartTime", 0L);
        CucumberHelper.recordScenarioResult(Status.SKIPPED, "feature:1");

        try (MockedStatic<TestNGListener> mockedTestNgListener = Mockito.mockStatic(TestNGListener.class)) {
            CucumberHelper.engineSetup();
            mockedTestNgListener.verify(() -> TestNGListener.engineSetup(ProjectStructureManager.RunType.CUCUMBER));
        }

        long executionStartTime = getExecutionTimestamp("executionStartTime");
        Assert.assertTrue(executionStartTime > 0);

        String cucumberOptions = ThreadLocalPropertiesManager.getProperty("cucumber.options");
        Assert.assertNotNull(cucumberOptions);
        Assert.assertTrue(cucumberOptions.contains(" --dry-run "));
        Assert.assertTrue(cucumberOptions.contains(" --features "));

        AtomicInteger skippedScenarios = getSkippedScenarios();
        Assert.assertEquals(skippedScenarios.get(), 0);
    }

    @Test
    public void recordScenarioResultShouldTrackPassedFailedAndSkippedScenarios() throws Exception {
        try (MockedStatic<TestNGListener> mockedTestNgListener = Mockito.mockStatic(TestNGListener.class)) {
            CucumberHelper.engineSetup();
            mockedTestNgListener.verify(() -> TestNGListener.engineSetup(ProjectStructureManager.RunType.CUCUMBER));
        }

        CucumberHelper.recordScenarioResult(Status.PASSED, "feature:10");
        CucumberHelper.recordScenarioResult(Status.FAILED, "feature:20");
        CucumberHelper.recordScenarioResult(Status.AMBIGUOUS, "feature:30");
        CucumberHelper.recordScenarioResult(Status.UNDEFINED, "feature:40");
        CucumberHelper.recordScenarioResult(Status.SKIPPED, "feature:50");

        Assert.assertEquals(getScenarioIds("passedScenarioIds"), java.util.Set.of("feature:10"));
        Assert.assertTrue(getScenarioIds("failedScenarioIds").containsAll(java.util.Set.of("feature:20", "feature:30", "feature:40")));
        Assert.assertEquals(getSkippedScenarios().get(), 1);
    }

    @Test
    public void shaftTearDownShouldSkipNativePathWhenCurrentTestResultExists() {
        try (MockedStatic<Reporter> mockedReporter = Mockito.mockStatic(Reporter.class);
             MockedStatic<ReportHelper> mockedReportHelper = Mockito.mockStatic(ReportHelper.class)) {
            mockedReporter.when(Reporter::getCurrentTestResult).thenReturn(Mockito.mock(ITestResult.class));

            CucumberHelper.shaftTearDown();

            mockedReportHelper.verifyNoInteractions();
        }
    }

    @Test
    public void shaftTearDownShouldExecuteNativeTelemetryPathWhenNoCurrentTestResultExists() throws Exception {
        try (MockedStatic<TestNGListener> mockedTestNgListener = Mockito.mockStatic(TestNGListener.class)) {
            CucumberHelper.engineSetup();
            mockedTestNgListener.verify(() -> TestNGListener.engineSetup(ProjectStructureManager.RunType.CUCUMBER));
        }

        CucumberHelper.recordScenarioResult(Status.PASSED, "feature:100");
        CucumberHelper.recordScenarioResult(Status.FAILED, "feature:100");
        CucumberHelper.recordScenarioResult(Status.FAILED, "feature:200");
        CucumberHelper.recordScenarioResult(Status.SKIPPED, "feature:300");

        try (MockedStatic<Reporter> mockedReporter = Mockito.mockStatic(Reporter.class);
             MockedStatic<ReportHelper> mockedReportHelper = Mockito.mockStatic(ReportHelper.class);
             MockedStatic<CheckpointCounter> mockedCheckpointCounter = Mockito.mockStatic(CheckpointCounter.class);
             MockedStatic<ReportManagerHelper> mockedReportManagerHelper = Mockito.mockStatic(ReportManagerHelper.class);
             MockedStatic<JiraHelper> mockedJiraHelper = Mockito.mockStatic(JiraHelper.class);
             MockedStatic<GoogleTink> mockedGoogleTink = Mockito.mockStatic(GoogleTink.class);
             MockedStatic<AllureManager> mockedAllureManager = Mockito.mockStatic(AllureManager.class)) {

            mockedReporter.when(Reporter::getCurrentTestResult).thenReturn(null);

            CucumberHelper.shaftTearDown();

            mockedReportHelper.verify(ReportHelper::attachEngineLog);
            mockedReportHelper.verify(ReportHelper::attachCucumberReport);
            mockedCheckpointCounter.verify(CheckpointCounter::attach);
            mockedReportHelper.verify(ReportHelper::attachIssuesLog);
            mockedReportManagerHelper.verify(() -> ReportManagerHelper.setDiscreteLogging(true));
            mockedJiraHelper.verify(JiraHelper::reportExecutionStatusToJira);
            mockedGoogleTink.verify(GoogleTink::encrypt);
            mockedAllureManager.verify(AllureManager::generateAllureReportArchive);
            mockedAllureManager.verify(AllureManager::openAllureReportAfterExecution);
            mockedReportManagerHelper.verify(ReportManagerHelper::logEngineClosure);
        }

        long executionEndTime = getExecutionTimestamp("executionEndTime");
        Assert.assertTrue(executionEndTime > 0);
    }

    private static long getExecutionTimestamp(String fieldName) throws Exception {
        Field field = CucumberHelper.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        return field.getLong(null);
    }

    private static void setExecutionTimestamp(String fieldName, long value) throws Exception {
        Field field = CucumberHelper.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.setLong(null, value);
    }

    @SuppressWarnings("unchecked")
    private static java.util.Set<String> getScenarioIds(String fieldName) throws Exception {
        Field field = CucumberHelper.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        return (java.util.Set<String>) field.get(null);
    }

    private static AtomicInteger getSkippedScenarios() throws Exception {
        Field field = CucumberHelper.class.getDeclaredField("skippedScenarios");
        field.setAccessible(true);
        return (AtomicInteger) field.get(null);
    }
}
