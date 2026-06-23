package com.shaft.listeners;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.ExecutionFailureContext;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.internal.ValidationsHelper;
import io.qameta.allure.Issue;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExecutableInvoker;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.platform.engine.discovery.DiscoverySelectors;
import org.junit.platform.launcher.LauncherDiscoveryRequest;
import org.junit.platform.launcher.core.LauncherDiscoveryRequestBuilder;
import org.junit.platform.launcher.core.LauncherConfig;
import org.junit.platform.launcher.core.LauncherFactory;
import org.junit.platform.launcher.listeners.SummaryGeneratingListener;
import org.junit.platform.launcher.listeners.TestExecutionSummary;
import org.opentest4j.TestAbortedException;

import java.lang.reflect.Method;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

class JunitExtensionLifecycleTest {
    private final JunitExtension extension = new JunitExtension();
    private final int originalRetryCount = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
    private final boolean originalSkipLinkedIssues = SHAFT.Properties.flags.skipTestsWithLinkedIssues();

    @AfterEach
    void cleanup() {
        SHAFT.Properties.flags.set()
                .retryMaximumNumberOfAttempts(originalRetryCount)
                .skipTestsWithLinkedIssues(originalSkipLinkedIssues);
        ValidationsHelper.resetVerificationStateAfterFailing();
        ExecutionFailureContext.getAndClearPendingConfigFailure();
        JunitListener.clearRecordedFailures();
        Properties.clearForCurrentThread();
    }

    @Test
    void afterTestExecutionShouldFailJunitTestWhenSoftVerificationFailed() {
        ExtensionContext context = mock(ExtensionContext.class);
        when(context.getExecutionException()).thenReturn(Optional.empty());

        SHAFT.Validations.verifyThat().object("actual").isEqualTo("expected").perform();

        assertThrows(AssertionError.class, () -> extension.afterTestExecution(context));
    }

    @Test
    void beforeEachShouldAbortTestsLinkedToOpenIssuesWhenFlagIsEnabled() throws Exception {
        SHAFT.Properties.flags.set().skipTestsWithLinkedIssues(true);
        Method method = JunitExtensionLifecycleTest.class.getDeclaredMethod("linkedIssueFixture");
        ExtensionContext context = mock(ExtensionContext.class);
        when(context.getTestMethod()).thenReturn(Optional.of(method));

        TestAbortedException thrown = assertThrows(TestAbortedException.class, () -> extension.beforeEach(context));

        assertEquals("Skipping Test as it's expected to fail due to open issue: [SHAFT-123]", thrown.getMessage());
    }

    @Test
    void lifecycleFailureHandlerShouldExposeConfigFailureToAllureListener() throws Throwable {
        ExtensionContext context = mock(ExtensionContext.class);
        RuntimeException failure = new RuntimeException("setup failed");

        RuntimeException thrown = assertThrows(RuntimeException.class,
                () -> extension.handleBeforeEachMethodExecutionException(context, failure));

        assertSame(failure, thrown);
        assertSame(failure, ExecutionFailureContext.getAndClearPendingConfigFailure());
    }

    @Test
    void handleTestExecutionExceptionShouldRethrowOriginalFailureWhenRetriesAreDisabled() throws Throwable {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0);
        AssertionError failure = new AssertionError("first attempt");
        ExecutableInvoker invoker = mock(ExecutableInvoker.class);
        ExtensionContext context = mockRetryContext("handleTestExecutionExceptionShouldRethrowOriginalFailureWhenRetriesAreDisabled", invoker);

        AssertionError thrown = assertThrows(AssertionError.class,
                () -> extension.handleTestExecutionException(context, failure));

        assertSame(failure, thrown);
        verifyNoInteractions(invoker);
    }

    @Test
    void handleTestExecutionExceptionShouldRetryFailedTestMethodOnce() throws Throwable {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        AtomicInteger attempts = new AtomicInteger(1);
        ExecutableInvoker invoker = mock(ExecutableInvoker.class);
        Method method = JunitExtensionLifecycleTest.class.getDeclaredMethod("retryFixture");
        ExtensionContext context = mockRetryContext("handleTestExecutionExceptionShouldRetryFailedTestMethodOnce", invoker, method);

        doAnswer(invocation -> {
            attempts.incrementAndGet();
            return null;
        }).when(invoker).invoke(method, this);

        extension.handleTestExecutionException(context, new AssertionError("first attempt"));

        assertEquals(2, attempts.get());
    }

    @Test
    void handleTestExecutionExceptionShouldThrowRetryFailureWhenRetriesAreExhausted() throws Throwable {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        AssertionError retryFailure = new AssertionError("retry attempt");
        ExecutableInvoker invoker = mock(ExecutableInvoker.class);
        Method method = JunitExtensionLifecycleTest.class.getDeclaredMethod("retryFixture");
        ExtensionContext context = mockRetryContext("handleTestExecutionExceptionShouldThrowRetryFailureWhenRetriesAreExhausted", invoker, method);
        doThrow(retryFailure).when(invoker).invoke(method, this);

        AssertionError thrown = assertThrows(AssertionError.class,
                () -> extension.handleTestExecutionException(context, new AssertionError("first attempt")));

        assertSame(retryFailure, thrown);
    }

    @Test
    void junitLauncherShouldRetryWithoutInvocationInterceptorChainFailure() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        LauncherDiscoveryRequest request = LauncherDiscoveryRequestBuilder.request()
                .selectors(DiscoverySelectors.selectClass(FlakyJunitRetryFixture.class))
                .configurationParameter("junit.jupiter.extensions.autodetection.enabled", "true")
                .build();
        SummaryGeneratingListener summaryListener = new SummaryGeneratingListener();
        FlakyJunitRetryFixture.attempts.set(0);

        LauncherFactory.create(LauncherConfig.builder()
                .enableLauncherSessionListenerAutoRegistration(false)
                .build()).execute(request, summaryListener);

        TestExecutionSummary summary = summaryListener.getSummary();
        assertEquals(2, FlakyJunitRetryFixture.attempts.get());
        assertEquals(0, summary.getFailures().size(), () -> summary.getFailures().toString());
        assertTrue(summary.getTestsSucceededCount() > 0);
    }

    @Issue("SHAFT-123")
    private void linkedIssueFixture() {
        // Used by reflection.
    }

    private void retryFixture() {
        // Used by reflection.
    }

    static class FlakyJunitRetryFixture {
        private static final AtomicInteger attempts = new AtomicInteger();

        @Test
        void failsOnceThenPasses() {
            int currentAttempt = attempts.incrementAndGet();
            if (currentAttempt == 1) {
                throw new AssertionError("first attempt");
            }
            assertEquals(2, currentAttempt);
        }
    }

    private static ExtensionContext mockExtensionContext(String displayName) {
        ExtensionContext context = mock(ExtensionContext.class);
        when(context.getRequiredTestClass()).thenAnswer(invocation -> JunitExtensionLifecycleTest.class);
        when(context.getUniqueId()).thenReturn("[engine:junit-jupiter]/[class:com.shaft.listeners.JunitExtensionLifecycleTest]/[method:" + displayName + "()]");
        when(context.getDisplayName()).thenReturn(displayName);
        return context;
    }

    private ExtensionContext mockRetryContext(String displayName, ExecutableInvoker invoker) throws Exception {
        return mockRetryContext(displayName, invoker, JunitExtensionLifecycleTest.class.getDeclaredMethod("retryFixture"));
    }

    private ExtensionContext mockRetryContext(String displayName, ExecutableInvoker invoker, Method method) {
        ExtensionContext context = mockExtensionContext(displayName);
        when(context.getRequiredTestMethod()).thenReturn(method);
        when(context.getTestInstance()).thenReturn(Optional.of(this));
        when(context.getExecutableInvoker()).thenReturn(invoker);
        return context;
    }
}
