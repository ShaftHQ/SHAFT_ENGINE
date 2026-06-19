package com.shaft.listeners;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.listeners.internal.ExecutionFailureContext;
import com.shaft.listeners.internal.ExecutionLifecycleHelper;
import com.shaft.listeners.internal.RetryAnalyzer;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.ValidationsHelper;
import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.AfterTestExecutionCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.InvocationInterceptor;
import org.junit.jupiter.api.extension.LifecycleMethodExecutionExceptionHandler;
import org.junit.jupiter.api.extension.ReflectiveInvocationContext;
import org.opentest4j.TestAbortedException;

import java.lang.reflect.Method;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Jupiter extension for SHAFT execution behavior that cannot be provided by a launcher listener.
 */
public class JunitExtension implements BeforeAllCallback, AfterAllCallback, BeforeEachCallback,
        AfterTestExecutionCallback, InvocationInterceptor, LifecycleMethodExecutionExceptionHandler {
    private static final AtomicLong suiteStartTime = new AtomicLong(0);

    @Override
    public void beforeAll(ExtensionContext context) {
        suiteStartTime.compareAndSet(0, System.currentTimeMillis());
        Properties.clearForCurrentThread();
    }

    @Override
    public void afterAll(ExtensionContext context) {
        Properties.clearForCurrentThread();
    }

    @Override
    public void beforeEach(ExtensionContext context) {
        enforceSuiteTimeout();
        failFast(context);
        skipLinkedIssues(context);
    }

    @Override
    public void interceptTestMethod(Invocation<Void> invocation, ReflectiveInvocationContext<Method> invocationContext,
                                    ExtensionContext extensionContext) throws Throwable {
        int maxRetryCount = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        Throwable lastFailure = null;
        for (int attempt = 0; attempt <= maxRetryCount; attempt++) {
            boolean retryAttempt = attempt > 0;
            try {
                if (retryAttempt) {
                    ReportManagerHelper.enableDebugFileLogging();
                    RetryAnalyzer.enableSupportingEvidenceCaptureForRetryAttempt();
                    RetryAnalyzer.activateSupportingEvidenceCaptureForRetryAttempt();
                    ReportManager.logDiscrete("Retry #" + attempt + "/" + maxRetryCount
                            + " for test: " + invocationContext.getExecutable().getName()
                            + ", on thread: " + Thread.currentThread().getName());
                }
                invocation.proceed();
                return;
            } catch (Throwable throwable) {
                lastFailure = throwable;
                if (attempt >= maxRetryCount) {
                    throw throwable;
                }
                JunitListener.recordRetriedFailure(toTestExecutionInfo(extensionContext, invocationContext.getExecutable(), throwable));
            } finally {
                if (retryAttempt) {
                    RetryAnalyzer.restoreSupportingEvidenceCaptureForRetryAttempt();
                }
            }
        }
        if (lastFailure != null) {
            throw lastFailure;
        }
    }

    @Override
    public void afterTestExecution(ExtensionContext context) {
        AssertionError verificationError = ValidationsHelper.getVerificationErrorToForceFail();
        if (verificationError != null) {
            ValidationsHelper.resetVerificationStateAfterFailing();
            if (context.getExecutionException().isEmpty()) {
                throw verificationError;
            }
        }
    }

    @Override
    public void handleBeforeAllMethodExecutionException(ExtensionContext context, Throwable throwable) throws Throwable {
        ExecutionFailureContext.setPendingConfigFailure(throwable);
        throw throwable;
    }

    @Override
    public void handleBeforeEachMethodExecutionException(ExtensionContext context, Throwable throwable) throws Throwable {
        ExecutionFailureContext.setPendingConfigFailure(throwable);
        throw throwable;
    }

    @Override
    public void handleAfterEachMethodExecutionException(ExtensionContext context, Throwable throwable) throws Throwable {
        ExecutionFailureContext.setPendingConfigFailure(throwable);
        throw throwable;
    }

    @Override
    public void handleAfterAllMethodExecutionException(ExtensionContext context, Throwable throwable) throws Throwable {
        ExecutionFailureContext.setPendingConfigFailure(throwable);
        throw throwable;
    }

    private static void enforceSuiteTimeout() {
        long startTime = suiteStartTime.get();
        long timeoutMillis = SHAFT.Properties.testNG.testSuiteTimeout() * 60_000L;
        if (startTime > 0 && timeoutMillis > 0 && System.currentTimeMillis() - startTime >= timeoutMillis) {
            throw new TestAbortedException("Skipping method as the test suite has exceeded the defined timeout of "
                    + SHAFT.Properties.testNG.testSuiteTimeout() + " minutes.");
        }
    }

    private static void failFast(ExtensionContext context) {
        if (DriverFactoryHelper.isKillSwitch()) {
            TestAbortedException exception = new TestAbortedException("Skipping Test: " + context.getDisplayName());
            ReportManagerHelper.logDiscrete(exception);
            throw exception;
        }
    }

    private static void skipLinkedIssues(ExtensionContext context) {
        Method method = context.getTestMethod().orElse(null);
        String skipMessage = ExecutionLifecycleHelper.getLinkedIssueSkipMessage(method);
        if (!skipMessage.isBlank()) {
            TestAbortedException exception = new TestAbortedException(skipMessage);
            ReportManagerHelper.logDiscrete(exception);
            throw exception;
        }
    }

    private static TestExecutionInfo toTestExecutionInfo(ExtensionContext context, Method method, Throwable throwable) {
        String className = context.getRequiredTestClass().getName();
        String methodName = method == null ? context.getDisplayName() : method.getName();
        return new TestExecutionInfo(context.getUniqueId(), className, methodName, context.getDisplayName(),
                context.getDisplayName(), method, throwable, true);
    }
}
