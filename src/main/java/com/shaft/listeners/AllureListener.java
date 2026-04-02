package com.shaft.listeners;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.listeners.internal.TestNGListenerHelper;
import com.shaft.tools.io.internal.ReportManagerHelper;
import io.qameta.allure.Allure;
import io.qameta.allure.listener.ContainerLifecycleListener;
import io.qameta.allure.listener.FixtureLifecycleListener;
import io.qameta.allure.listener.StepLifecycleListener;
import io.qameta.allure.listener.TestLifecycleListener;
import io.qameta.allure.model.FixtureResult;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StatusDetails;
import io.qameta.allure.model.StepResult;
import io.qameta.allure.model.TestResult;
import io.qameta.allure.model.TestResultContainer;
import org.testng.SkipException;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

/**
 * Allure lifecycle listener that integrates SHAFT's TestNG support with the Allure reporting engine.
 *
 * <p>This listener is registered automatically and hooks into all four Allure lifecycle interfaces:
 * {@link StepLifecycleListener}, {@link FixtureLifecycleListener}, {@link TestLifecycleListener},
 * and {@link ContainerLifecycleListener}. The SHAFT-specific behaviour is in:
 * <ul>
 *   <li>{@link #afterStepStop(StepResult)} — updates TestNG configuration method metadata after
 *       each step completes</li>
 *   <li>{@link #beforeFixtureStop(FixtureResult)} — attaches configuration-method artefacts to
 *       the Allure report before a fixture (setup/teardown) finishes</li>
 *   <li>{@link #beforeTestStop(TestResult)} — promotes a SKIPPED test result to BROKEN when the
 *       skip was caused by a configuration-method failure or kill-switch activation, and attaches
 *       the exception stacktrace so it is readable in the Allure HTML report</li>
 * </ul>
 *
 * <p><b>Example — automatic registration via SPI:</b>
 * <pre>{@code
 * // Listed in META-INF/services/io.qameta.allure.listener.LifecycleListener
 * // No manual registration required.
 * com.shaft.listeners.AllureListener
 * }</pre>
 */
public class AllureListener implements StepLifecycleListener, FixtureLifecycleListener, TestLifecycleListener,
        ContainerLifecycleListener {

    //Before each step starts inside the methods
    @Override
    public void beforeStepStart(StepResult result) {
        StepLifecycleListener.super.beforeStepStart(result);
    }

    //After each step starts inside the methods
    @Override
    public void afterStepStart(StepResult result) {
        StepLifecycleListener.super.afterStepStart(result);
    }

    //Before each step update inside the methods
    @Override
    public void beforeStepUpdate(StepResult result) {
        StepLifecycleListener.super.beforeStepUpdate(result);
    }

    //After each step update inside the methods
    @Override
    public void afterStepUpdate(StepResult result) {
        StepLifecycleListener.super.afterStepUpdate(result);
    }

    //Before each step Stop inside the methods
    @Override
    public void beforeStepStop(StepResult result) {
        StepLifecycleListener.super.beforeStepStop(result);
    }

    /**
     * Invoked after each Allure step stops. If a TestNG {@link org.testng.ITestResult} is available
     * for the current thread, delegates to {@link TestNGListenerHelper#updateConfigurationMethods}
     * to keep configuration-method metadata in sync with the Allure step model.
     *
     * @param result the {@link StepResult} representing the step that has just stopped
     */
    @Override
    public void afterStepStop(StepResult result) {
        var iTestResult = TestNGListener.getITestResult();
        if (iTestResult != null) {
            TestNGListenerHelper.updateConfigurationMethods(iTestResult);
        }
    }

    //Before The Class starts
    @Override
    public void beforeContainerStart(TestResultContainer container) {
        ContainerLifecycleListener.super.beforeContainerStart(container);
    }

    //After The Class starts
    @Override
    public void afterContainerStart(TestResultContainer container) {
        ContainerLifecycleListener.super.afterContainerStart(container);
    }

    //Before The Class updates
    @Override
    public void beforeContainerUpdate(TestResultContainer container) {
        ContainerLifecycleListener.super.beforeContainerUpdate(container);
    }

    //After The Class updates
    @Override
    public void afterContainerUpdate(TestResultContainer container) {
        ContainerLifecycleListener.super.afterContainerUpdate(container);
    }

    //Before The Class stops
    @Override
    public void beforeContainerStop(TestResultContainer container) {
        ContainerLifecycleListener.super.beforeContainerStop(container);
    }

    //After The Class stops
    @Override
    public void afterContainerStop(TestResultContainer container) {
        ContainerLifecycleListener.super.afterContainerStop(container);
    }

    //Before The Class writes
    @Override
    public void beforeContainerWrite(TestResultContainer container) {
        ContainerLifecycleListener.super.beforeContainerWrite(container);
    }

    //After The Class writes
    @Override
    public void afterContainerWrite(TestResultContainer container) {
        ContainerLifecycleListener.super.afterContainerWrite(container);
    }

    //Before The Configuration 'SetUp' "and probably 'TearDown' too" starts
    @Override
    public void beforeFixtureStart(FixtureResult result) {
        FixtureLifecycleListener.super.beforeFixtureStart(result);
    }

    //After The Configuration 'SetUp' "and probably 'TearDown' too" starts
    @Override
    public void afterFixtureStart(FixtureResult result) {
        FixtureLifecycleListener.super.afterFixtureStart(result);
    }

    //Before The Configuration 'SetUp' "and probably 'TearDown' too" updates
    @Override
    public void beforeFixtureUpdate(FixtureResult result) {
        FixtureLifecycleListener.super.beforeFixtureUpdate(result);
    }

    //After The Configuration 'SetUp' "and probably 'TearDown' too" updates
    @Override
    public void afterFixtureUpdate(FixtureResult result) {
        FixtureLifecycleListener.super.afterFixtureUpdate(result);
    }

    /**
     * Invoked before each Allure fixture (TestNG {@code @BeforeXxx}/{@code @AfterXxx} method) stops.
     * Calls {@link TestNGListenerHelper#attachConfigurationMethods()} to attach any pending
     * configuration-method artefacts to the Allure report before the fixture result is finalised.
     *
     * @param result the {@link FixtureResult} representing the fixture that is about to stop
     */
    @Override
    public void beforeFixtureStop(FixtureResult result) {
        TestNGListenerHelper.attachConfigurationMethods();
    }

    //After The Configuration 'SetUp' "and probably 'TearDown' too" stops
    @Override
    public void afterFixtureStop(FixtureResult result) {
        FixtureLifecycleListener.super.afterFixtureStop(result);
    }

    //Before The Configuration 'SetUp' starts
    @Override
    public void beforeTestSchedule(TestResult result) {
        TestLifecycleListener.super.beforeTestSchedule(result);
    }

    //After The Configuration 'SetUp' starts
    @Override
    public void afterTestSchedule(TestResult result) {
        TestLifecycleListener.super.afterTestSchedule(result);
    }

    //Before The @test updates
    @Override
    public void beforeTestUpdate(TestResult result) {
        TestLifecycleListener.super.beforeTestUpdate(result);
    }

    //After The @test updates
    @Override
    public void afterTestUpdate(TestResult result) {
        TestLifecycleListener.super.afterTestUpdate(result);
    }

    //Before The @test starts
    @Override
    public void beforeTestStart(TestResult result) {
        TestLifecycleListener.super.beforeTestStart(result);
    }

    //After The @test starts
    @Override
    public void afterTestStart(TestResult result) {
        TestLifecycleListener.super.afterTestStart(result);
    }

    /**
     * Invoked before each Allure test case stops. When the result status is {@link Status#SKIPPED}
     * and the skip was caused by a non-intentional reason (configuration method failure or kill-switch
     * activation rather than a deliberate {@link org.testng.SkipException}), this method:
     * <ol>
     *   <li>Promotes the status to {@link Status#BROKEN} so the test appears as failed in the report.</li>
     *   <li>Attaches the full exception stacktrace as a readable {@code text/plain} attachment.</li>
     * </ol>
     *
     * <p>Intentional skips — those raised by SHAFT's linked-issue skipper or the test-suite
     * timeout guard — are left as {@link Status#SKIPPED}.
     *
     * @param result the {@link TestResult} that is about to be finalised
     */
    @Override
    public void beforeTestStop(TestResult result) {
        if (Status.SKIPPED.equals(result.getStatus())) {
            Throwable configFailure = TestNGListenerHelper.getAndClearPendingConfigFailure();
            boolean isKillSwitch = DriverFactoryHelper.isKillSwitch();
            boolean isRealConfigFailure = configFailure != null && !(configFailure instanceof SkipException);

            // Determine whether this skip was caused by a real failure rather than an intentional skip
            boolean isFatalSkip = isKillSwitch || isRealConfigFailure;

            if (isFatalSkip) {
                String message;
                String trace;
                if (isRealConfigFailure) {
                    message = configFailure.getMessage();
                    trace = ReportManagerHelper.formatStackTraceToLogEntry(configFailure);
                } else {
                    message = "Test execution halted: a previous driver initialisation failure activated the kill switch.";
                    trace = message;
                }
                result.setStatus(Status.BROKEN);
                StatusDetails details = result.getStatusDetails() != null ? result.getStatusDetails() : new StatusDetails();
                details.setMessage(message);
                details.setTrace(trace);
                result.setStatusDetails(details);
                // Attach the stacktrace as a readable text file so it appears in the Allure report
                Allure.addAttachment(
                        "Exception Stacktrace",
                        "text/plain",
                        new ByteArrayInputStream(trace.getBytes(StandardCharsets.UTF_8)),
                        ".txt");
            }
        }
        TestLifecycleListener.super.beforeTestStop(result);
    }

    //After The @test stops
    @Override
    public void afterTestStop(TestResult result) {
        TestLifecycleListener.super.afterTestStop(result);
    }

    //Before The @test writes
    @Override
    public void beforeTestWrite(TestResult result) {
        TestLifecycleListener.super.beforeTestWrite(result);
    }

    //After The @test writes
    @Override
    public void afterTestWrite(TestResult result) {
        TestLifecycleListener.super.afterTestWrite(result);
    }
}
