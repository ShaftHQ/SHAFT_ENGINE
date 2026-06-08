package com.shaft.listeners;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.listeners.internal.TestNGListenerHelper;
import io.qameta.allure.AllureLifecycle;
import io.qameta.allure.FileSystemResultsWriter;
import io.qameta.allure.model.FixtureResult;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StatusDetails;
import io.qameta.allure.model.StepResult;
import io.qameta.allure.model.TestResult;
import io.qameta.allure.model.TestResultContainer;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.ITestResult;
import org.testng.Reporter;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertTrue;

@Test(singleThreaded = true)
public class AllureListenerCoverageUnitTest {
    private AllureListener listener;
    private AllureLifecycle lifecycle;
    private Path allureResultsDirectory;

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod() throws Exception {
        allureResultsDirectory = Files.createTempDirectory("shaft-allure-listener-results");
        lifecycle = new AllureLifecycle(new FileSystemResultsWriter(allureResultsDirectory));
        listener = new AllureListener(lifecycle);
        setITestResult(null);
        setKillSwitch(false);
        TestNGListenerHelper.setPendingConfigFailure(null);
        Reporter.clear();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws Exception {
        setITestResult(null);
        setKillSwitch(false);
        TestNGListenerHelper.setPendingConfigFailure(null);
        TestNGListenerHelper.cleanup();
        Reporter.clear();
        lifecycle = null;
        deleteDirectory(allureResultsDirectory);
    }

    @Test
    public void lifecyclePassThroughCallbacksShouldLeaveAllureModelsUnchanged() {
        StepResult step = new StepResult().setName("step").setStatus(Status.PASSED);
        FixtureResult fixture = new FixtureResult().setName("fixture").setStatus(Status.PASSED);
        TestResult test = new TestResult().setUuid("test-uuid").setName("test").setStatus(Status.PASSED);
        TestResultContainer container = new TestResultContainer()
                .setUuid("container-uuid")
                .setName("container")
                .setChildren(List.of("test-uuid"));

        listener.beforeStepStart(step);
        listener.afterStepStart(step);
        listener.beforeStepUpdate(step);
        listener.afterStepUpdate(step);
        listener.beforeStepStop(step);
        listener.afterStepStop(step);

        listener.beforeContainerStart(container);
        listener.afterContainerStart(container);
        listener.beforeContainerUpdate(container);
        listener.afterContainerUpdate(container);
        listener.beforeContainerStop(container);
        listener.afterContainerStop(container);
        listener.beforeContainerWrite(container);
        listener.afterContainerWrite(container);

        listener.beforeFixtureStart(fixture);
        listener.afterFixtureStart(fixture);
        listener.beforeFixtureUpdate(fixture);
        listener.afterFixtureUpdate(fixture);
        listener.afterFixtureStop(fixture);

        listener.beforeTestSchedule(test);
        listener.afterTestSchedule(test);
        listener.beforeTestUpdate(test);
        listener.afterTestUpdate(test);
        listener.beforeTestStart(test);
        listener.afterTestStart(test);
        listener.beforeTestStop(test);
        listener.afterTestStop(test);
        listener.beforeTestWrite(test);
        listener.afterTestWrite(test);

        assertEquals(step.getName(), "step");
        assertEquals(step.getStatus(), Status.PASSED);
        assertEquals(fixture.getName(), "fixture");
        assertEquals(fixture.getStatus(), Status.PASSED);
        assertEquals(test.getName(), "test");
        assertEquals(test.getStatus(), Status.PASSED);
        assertEquals(container.getName(), "container");
        assertEquals(container.getChildren(), List.of("test-uuid"));
    }

    @Test
    public void beforeFixtureStopShouldDelegateToConfigurationAttachmentWrapper() {
        FixtureResult fixture = new FixtureResult().setName("configuration");

        try (MockedStatic<TestNGListenerHelper> helper = Mockito.mockStatic(TestNGListenerHelper.class)) {
            listener.beforeFixtureStop(fixture);

            helper.verify(TestNGListenerHelper::attachConfigurationMethods);
            helper.verifyNoMoreInteractions();
        }
    }

    @Test
    public void afterStepStopShouldNoOpWhenTestNgResultStateIsNull() throws Exception {
        setITestResult(null);

        try (MockedStatic<TestNGListenerHelper> helper = Mockito.mockStatic(TestNGListenerHelper.class)) {
            listener.afterStepStop(new StepResult().setName("step without testng result"));

            helper.verifyNoInteractions();
        }
    }

    @Test
    public void afterStepStopShouldUpdateConfigurationMethodsWhenTestNgResultStateExists() throws Exception {
        ITestResult testResult = Mockito.mock(ITestResult.class);
        setITestResult(testResult);

        try (MockedStatic<TestNGListenerHelper> helper = Mockito.mockStatic(TestNGListenerHelper.class)) {
            listener.afterStepStop(new StepResult().setName("step with testng result"));

            helper.verify(() -> TestNGListenerHelper.updateConfigurationMethods(testResult));
            helper.verifyNoMoreInteractions();
        }
    }

    @Test
    public void beforeTestStopShouldLeaveNonSkippedResultUnchangedAndNotConsumePendingFailure() {
        RuntimeException pendingFailure = new RuntimeException("ignored config failure");
        TestNGListenerHelper.setPendingConfigFailure(pendingFailure);
        TestResult result = new TestResult().setStatus(Status.PASSED);

        try (MockedStatic<DriverFactoryHelper> driverFactoryHelper = Mockito.mockStatic(DriverFactoryHelper.class)) {
            driverFactoryHelper.when(DriverFactoryHelper::isKillSwitch).thenReturn(false);
            listener.beforeTestStop(result);
        }

        assertEquals(result.getStatus(), Status.PASSED);
        assertNull(result.getStatusDetails());
        assertEquals(TestNGListenerHelper.getAndClearPendingConfigFailure(), pendingFailure);
    }

    @Test
    public void beforeTestStopShouldKeepIntentionalSkipAsSkippedAndClearPendingFailure() {
        TestNGListenerHelper.setPendingConfigFailure(new SkipException("linked issue skip"));
        TestResult result = skippedResult("intentional skip");

        try (MockedStatic<DriverFactoryHelper> driverFactoryHelper = Mockito.mockStatic(DriverFactoryHelper.class)) {
            driverFactoryHelper.when(DriverFactoryHelper::isKillSwitch).thenReturn(false);
            listener.beforeTestStop(result);
        }

        assertEquals(result.getStatus(), Status.SKIPPED);
        assertEquals(result.getStatusDetails().getMessage(), "intentional skip");
        assertNull(TestNGListenerHelper.getAndClearPendingConfigFailure());
    }

    @Test
    public void beforeTestStopShouldKeepOrdinarySkipAsSkippedWhenNoFatalStateExists() {
        TestResult result = skippedResult("ordinary skip");

        try (MockedStatic<DriverFactoryHelper> driverFactoryHelper = Mockito.mockStatic(DriverFactoryHelper.class)) {
            driverFactoryHelper.when(DriverFactoryHelper::isKillSwitch).thenReturn(false);
            listener.beforeTestStop(result);
        }

        assertEquals(result.getStatus(), Status.SKIPPED);
        assertEquals(result.getStatusDetails().getMessage(), "ordinary skip");
        assertNull(TestNGListenerHelper.getAndClearPendingConfigFailure());
    }

    @Test
    public void beforeTestStopShouldPromoteSkippedConfigFailureToBrokenAndAttachStacktrace() throws Exception {
        RuntimeException configFailure = new RuntimeException("configuration exploded");
        TestNGListenerHelper.setPendingConfigFailure(configFailure);
        TestResult result = skippedResult("config skip")
                .setUuid(UUID.randomUUID().toString())
                .setName("config failure host");
        lifecycle.scheduleTestCase(result);
        lifecycle.startTestCase(result.getUuid());

        try (MockedStatic<DriverFactoryHelper> driverFactoryHelper = Mockito.mockStatic(DriverFactoryHelper.class)) {
            driverFactoryHelper.when(DriverFactoryHelper::isKillSwitch).thenReturn(false);
            listener.beforeTestStop(result);
        } finally {
            lifecycle.stopTestCase(result.getUuid());
            lifecycle.writeTestCase(result.getUuid());
        }

        assertEquals(result.getStatus(), Status.BROKEN);
        assertNotNull(result.getStatusDetails());
        assertEquals(result.getStatusDetails().getMessage(), "configuration exploded");
        assertTrue(result.getStatusDetails().getTrace().contains("configuration exploded"));
        assertEquals(result.getAttachments().size(), 1);
        assertEquals(result.getAttachments().getFirst().getName(), "Exception Stacktrace");
        assertNull(TestNGListenerHelper.getAndClearPendingConfigFailure());
        assertAttachmentFileContains("configuration exploded");
    }

    @Test
    public void beforeTestStopShouldPromoteSkippedKillSwitchToBrokenWithStatusDetails() {
        TestResult result = skippedResult("kill switch skip");

        try (MockedStatic<DriverFactoryHelper> driverFactoryHelper = Mockito.mockStatic(DriverFactoryHelper.class)) {
            driverFactoryHelper.when(DriverFactoryHelper::isKillSwitch).thenReturn(true);
            listener.beforeTestStop(result);
        }

        assertEquals(result.getStatus(), Status.BROKEN);
        assertNotNull(result.getStatusDetails());
        assertTrue(result.getStatusDetails().getMessage().contains("kill switch"));
        assertEquals(result.getStatusDetails().getTrace(), result.getStatusDetails().getMessage());
        assertNull(TestNGListenerHelper.getAndClearPendingConfigFailure());
    }

    private static TestResult skippedResult(String message) {
        return new TestResult()
                .setStatus(Status.SKIPPED)
                .setStatusDetails(new StatusDetails().setMessage(message));
    }

    private void assertAttachmentFileContains(String expectedText) throws IOException {
        List<Path> attachmentFiles = listFiles(allureResultsDirectory).stream()
                .filter(path -> path.getFileName().toString().endsWith(".txt"))
                .toList();
        assertFalse(attachmentFiles.isEmpty(), "Expected a text attachment in the isolated allure-results directory.");
        boolean foundExpectedText = false;
        for (Path attachmentFile : attachmentFiles) {
            String content = Files.readString(attachmentFile, StandardCharsets.UTF_8);
            if (content.contains(expectedText)) {
                foundExpectedText = true;
                break;
            }
        }
        assertTrue(foundExpectedText, "Expected the stacktrace attachment to contain: " + expectedText);
    }

    private static List<Path> listFiles(Path directory) throws IOException {
        try (Stream<Path> files = Files.list(directory)) {
            return files.toList();
        }
    }

    private static void setITestResult(ITestResult testResult) throws Exception {
        Field iTestResultField = TestNGListener.class.getDeclaredField("iTestResult");
        iTestResultField.setAccessible(true);
        iTestResultField.set(null, testResult);
    }

    private static void setKillSwitch(boolean value) throws Exception {
        Field killSwitchField = DriverFactoryHelper.class.getDeclaredField("killSwitch");
        killSwitchField.setAccessible(true);
        killSwitchField.setBoolean(null, value);
    }

    private static void deleteDirectory(Path directory) throws IOException {
        if (directory == null || !Files.exists(directory)) {
            return;
        }
        try (Stream<Path> paths = Files.walk(directory)) {
            paths.sorted(Comparator.reverseOrder())
                    .forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (IOException e) {
                            throw new IllegalStateException("Failed to delete " + path, e);
                        }
                    });
        }
    }
}
