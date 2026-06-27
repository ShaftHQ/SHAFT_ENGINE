package com.shaft.listeners;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.listeners.internal.ExecutionFailureContext;
import com.shaft.listeners.internal.ExecutionLifecycleHelper;
import com.shaft.listeners.internal.RetryAnalyzer;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FlakeProfiler;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.ValidationsHelper;
import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.AfterTestExecutionCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.LifecycleMethodExecutionExceptionHandler;
import org.junit.jupiter.api.extension.TestExecutionExceptionHandler;
import org.junit.platform.engine.DiscoverySelector;
import org.junit.platform.engine.discovery.DiscoverySelectors;
import org.junit.platform.launcher.LauncherDiscoveryRequest;
import org.junit.platform.launcher.core.LauncherConfig;
import org.junit.platform.launcher.core.LauncherDiscoveryRequestBuilder;
import org.junit.platform.launcher.core.LauncherFactory;
import org.junit.platform.launcher.listeners.SummaryGeneratingListener;
import org.junit.platform.launcher.listeners.TestExecutionSummary;
import org.opentest4j.TestAbortedException;

import java.lang.reflect.Method;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Jupiter extension for SHAFT execution behavior that cannot be provided by a launcher listener.
 */
public class JunitExtension implements BeforeAllCallback, AfterAllCallback, BeforeEachCallback,
        AfterEachCallback, AfterTestExecutionCallback, TestExecutionExceptionHandler,
        LifecycleMethodExecutionExceptionHandler {
    private static final AtomicLong suiteStartTime = new AtomicLong(0);
    private static final ExtensionContext.Namespace RETRY_NAMESPACE =
            ExtensionContext.Namespace.create(JunitExtension.class, "retry");
    private static final String PENDING_RETRY_KEY = "pendingRetry";
    private static final ConcurrentMap<String, ActiveRetryAttempt> activeRetryAttempts = new ConcurrentHashMap<>();

    @Override
    public void beforeAll(ExtensionContext context) {
        suiteStartTime.compareAndSet(0, System.currentTimeMillis());
        Properties.clearForCurrentThread();
        DriverFactoryHelper.preflightRemoteGridIfConfigured();
    }

    @Override
    public void afterAll(ExtensionContext context) {
        Properties.clearForCurrentThread();
    }

    @Override
    public void beforeEach(ExtensionContext context) {
        ActiveRetryAttempt retryAttempt = activeRetryAttempt(context);
        if (retryAttempt != null) {
            Method method = context.getRequiredTestMethod();
            logRetryAttempt(method, retryAttempt.attempt(), retryAttempt.maxRetryCount());
            RetryAnalyzer.enableSupportingEvidenceCaptureForRetryAttempt();
            RetryAnalyzer.activateSupportingEvidenceCaptureForRetryAttempt();
        }
        enforceSuiteTimeout();
        failFast(context);
        skipLinkedIssues(context);
    }

    @Override
    public void handleTestExecutionException(ExtensionContext context, Throwable throwable) throws Throwable {
        if (isActiveRetry(context) || !scheduleRetry(context, throwable)) {
            throw throwable;
        }
    }

    @Override
    public void afterTestExecution(ExtensionContext context) throws Exception {
        AssertionError verificationError = ValidationsHelper.getVerificationErrorToForceFail();
        if (verificationError != null) {
            ValidationsHelper.resetVerificationStateAfterFailing();
            if (context.getExecutionException().isEmpty()) {
                if (isActiveRetry(context) || !scheduleRetry(context, verificationError)) {
                    throw verificationError;
                }
            }
        }
    }

    @Override
    public void afterEach(ExtensionContext context) throws Exception {
        if (isActiveRetry(context)) {
            RetryAnalyzer.restoreSupportingEvidenceCaptureForRetryAttempt();
            return;
        }
        PendingRetry pendingRetry = context.getStore(RETRY_NAMESPACE).remove(PENDING_RETRY_KEY, PendingRetry.class);
        if (pendingRetry != null) {
            executeScheduledRetry(context, pendingRetry);
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

    private static boolean scheduleRetry(ExtensionContext context, Throwable throwable) {
        int maxRetryCount = RetryAnalyzer.retryMaximumNumberOfAttempts();
        if (maxRetryCount <= 0) {
            return false;
        }
        ExtensionContext.Store store = context.getStore(RETRY_NAMESPACE);
        if (store.get(PENDING_RETRY_KEY, PendingRetry.class) == null) {
            store.put(PENDING_RETRY_KEY,
                    new PendingRetry(context.getRequiredTestMethod(), throwable, maxRetryCount));
        }
        return true;
    }

    private static void executeScheduledRetry(ExtensionContext context, PendingRetry pendingRetry) throws Exception {
        Throwable lastFailure = pendingRetry.failure();
        for (int attempt = 1; attempt <= pendingRetry.maxRetryCount(); attempt++) {
            JunitListener.recordRetriedFailure(toTestExecutionInfo(context, pendingRetry.method(), lastFailure));
            TestExecutionSummary summary = executeRetryAttempt(context, pendingRetry.method(), attempt,
                    pendingRetry.maxRetryCount());
            if (isSuccessfulRetry(summary)) {
                return;
            }
            lastFailure = failureFrom(summary)
                    .orElseGet(() -> new AssertionError("Retry attempt did not execute test: "
                            + context.getDisplayName()));
        }
        throwAsException(lastFailure);
    }

    private static TestExecutionSummary executeRetryAttempt(ExtensionContext context, Method method, int attempt,
                                                           int maxRetryCount) throws Exception {
        String uniqueId = context.getUniqueId();
        activeRetryAttempts.put(uniqueId, new ActiveRetryAttempt(attempt, maxRetryCount));
        try {
            TestExecutionSummary summary = executeRetryRequest(
                    retryRequest(DiscoverySelectors.selectUniqueId(uniqueId)));
            if (summary.getTestsFoundCount() == 0 && method != null) {
                summary = executeRetryRequest(
                        retryRequest(DiscoverySelectors.selectMethod(context.getRequiredTestClass(), method)));
            }
            return summary;
        } finally {
            activeRetryAttempts.remove(uniqueId);
        }
    }

    private static LauncherDiscoveryRequest retryRequest(DiscoverySelector selector) {
        return LauncherDiscoveryRequestBuilder.request()
                .selectors(selector)
                .configurationParameter("junit.jupiter.extensions.autodetection.enabled", "true")
                .configurationParameter("junit.jupiter.execution.parallel.enabled", "false")
                .build();
    }

    private static TestExecutionSummary executeRetryRequest(LauncherDiscoveryRequest request) throws Exception {
        ExecutorService executor = Executors.newSingleThreadExecutor(runnable -> {
            Thread thread = new Thread(runnable, "shaft-junit-retry");
            thread.setDaemon(false);
            return thread;
        });
        try {
            return executor.submit(() -> {
                SummaryGeneratingListener summaryListener = new SummaryGeneratingListener();
                LauncherFactory.create(LauncherConfig.builder()
                        .enableLauncherSessionListenerAutoRegistration(false)
                        .enableTestExecutionListenerAutoRegistration(false)
                        .build()).execute(request, summaryListener);
                return summaryListener.getSummary();
            }).get();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw e;
        } catch (ExecutionException e) {
            throwAsException(e.getCause() == null ? e : e.getCause());
            throw e;
        } finally {
            executor.shutdownNow();
        }
    }

    private static boolean isSuccessfulRetry(TestExecutionSummary summary) {
        return summary.getTestsSucceededCount() > 0
                && summary.getTestsFailedCount() == 0
                && summary.getFailures().isEmpty();
    }

    private static Optional<Throwable> failureFrom(TestExecutionSummary summary) {
        return summary.getFailures().stream()
                .findFirst()
                .map(TestExecutionSummary.Failure::getException);
    }

    private static boolean isActiveRetry(ExtensionContext context) {
        return activeRetryAttempt(context) != null;
    }

    private static ActiveRetryAttempt activeRetryAttempt(ExtensionContext context) {
        String uniqueId = context.getUniqueId();
        return uniqueId == null ? null : activeRetryAttempts.get(uniqueId);
    }

    private static void throwAsException(Throwable throwable) throws Exception {
        if (throwable instanceof Error error) {
            throw error;
        }
        if (throwable instanceof Exception exception) {
            throw exception;
        }
        throw new RuntimeException(throwable);
    }

    private static void logRetryAttempt(Method method, int attempt, int maxRetryCount) {
        ReportManagerHelper.enableDebugFileLogging();
        ReportManager.logDiscrete("Retry #" + attempt + "/" + maxRetryCount
                + " for test: " + method.getName()
                + ", on thread: " + Thread.currentThread().getName());
        FlakeProfiler.recordRetryAttempt(method.getName(), attempt, maxRetryCount,
                SHAFT.Properties.flags.forceCaptureSupportingEvidenceOnRetry());
    }

    private static TestExecutionInfo toTestExecutionInfo(ExtensionContext context, Method method, Throwable throwable) {
        String className = context.getRequiredTestClass().getName();
        String methodName = method == null ? context.getDisplayName() : method.getName();
        return new TestExecutionInfo(context.getUniqueId(), className, methodName, context.getDisplayName(),
                context.getDisplayName(), method, throwable, true);
    }

    private record PendingRetry(Method method, Throwable failure, int maxRetryCount) {
    }

    private record ActiveRetryAttempt(int attempt, int maxRetryCount) {
    }
}
