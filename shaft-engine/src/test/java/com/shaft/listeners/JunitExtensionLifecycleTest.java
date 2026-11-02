package com.shaft.listeners;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.ExecutionFailureContext;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.internal.ValidationsHelper;
import io.qameta.allure.Issue;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
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
import static org.mockito.Mockito.mock;
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
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0);
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
        ExtensionContext context = mockExtensionContext("handleTestExecutionExceptionShouldRethrowOriginalFailureWhenRetriesAreDisabled");

        AssertionError thrown = assertThrows(AssertionError.class,
                () -> extension.handleTestExecutionException(context, failure));

        assertSame(failure, thrown);
    }

    @Test
    void junitLauncherShouldRetryWithFreshBeforeEachAndAfterEachLifecycle() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        LauncherRetryFixture.reset();

        TestExecutionSummary summary = executeFixture(LauncherRetryFixture.class);

        assertEquals(2, LauncherRetryFixture.attempts.get());
        assertEquals(2, LauncherRetryFixture.beforeEachCalls.get());
        assertEquals(2, LauncherRetryFixture.afterEachCalls.get());
        assertEquals(0, summary.getFailures().size(), () -> summary.getFailures().toString());
        assertTrue(summary.getTestsSucceededCount() > 0);
    }

    @Test
    void junitLauncherShouldKeepOuterSessionStateIsolatedFromRetryLifecycle() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        OuterSessionStateRetryFixture.reset();

        TestExecutionSummary summary = executeFixture(OuterSessionStateRetryFixture.class);

        assertEquals(2, OuterSessionStateRetryFixture.attempts.get());
        assertEquals(0, summary.getFailures().size(), () -> summary.getFailures().toString());
        assertEquals(2, summary.getTestsSucceededCount());
    }

    @Test
    void junitLauncherShouldReportRetryFailureWhenRetriesAreExhausted() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        ExhaustedLauncherRetryFixture.reset();

        TestExecutionSummary summary = executeFixture(ExhaustedLauncherRetryFixture.class);

        assertEquals(2, ExhaustedLauncherRetryFixture.attempts.get());
        assertEquals(2, ExhaustedLauncherRetryFixture.beforeEachCalls.get());
        assertEquals(2, ExhaustedLauncherRetryFixture.afterEachCalls.get());
        assertEquals(1, summary.getFailures().size());
        assertEquals("retry attempt", summary.getFailures().get(0).getException().getMessage());
    }

    @Test
    void junitLauncherShouldRetryWithoutInvocationInterceptorChainFailure() {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        LauncherRetryFixture.reset();

        TestExecutionSummary summary = executeFixture(LauncherRetryFixture.class);

        assertEquals(2, LauncherRetryFixture.attempts.get());
        assertEquals(0, summary.getFailures().size(), () -> summary.getFailures().toString());
        assertTrue(summary.getTestsSucceededCount() > 0);
    }

    private static TestExecutionSummary executeFixture(Class<?> fixtureClass) {
        LauncherDiscoveryRequest request = LauncherDiscoveryRequestBuilder.request()
                .selectors(DiscoverySelectors.selectClass(fixtureClass))
                .configurationParameter("junit.jupiter.extensions.autodetection.enabled", "true")
                .configurationParameter("junit.jupiter.execution.parallel.enabled", "false")
                .build();
        SummaryGeneratingListener summaryListener = new SummaryGeneratingListener();

        LauncherFactory.create(LauncherConfig.builder()
                .enableLauncherSessionListenerAutoRegistration(false)
                .enableTestExecutionListenerAutoRegistration(false)
                .build()).execute(request, summaryListener);

        return summaryListener.getSummary();
    }

    @Issue("SHAFT-123")
    private void linkedIssueFixture() {
        // Used by reflection.
    }

    private static ExtensionContext mockExtensionContext(String displayName) {
        ExtensionContext context = mock(ExtensionContext.class);
        when(context.getRequiredTestClass()).thenAnswer(invocation -> JunitExtensionLifecycleTest.class);
        when(context.getUniqueId()).thenReturn("[engine:junit-jupiter]/[class:com.shaft.listeners.JunitExtensionLifecycleTest]/[method:" + displayName + "()]");
        when(context.getDisplayName()).thenReturn(displayName);
        return context;
    }
}

class LauncherRetryFixture {
    static final AtomicInteger attempts = new AtomicInteger();
    static final AtomicInteger beforeEachCalls = new AtomicInteger();
    static final AtomicInteger afterEachCalls = new AtomicInteger();

    static void reset() {
        attempts.set(0);
        beforeEachCalls.set(0);
        afterEachCalls.set(0);
    }

    @BeforeEach
    void setup() {
        beforeEachCalls.incrementAndGet();
    }

    @AfterEach
    void teardown() {
        afterEachCalls.incrementAndGet();
    }

    @Test
    void failsOnceThenPasses() {
        int currentAttempt = attempts.incrementAndGet();
        if (currentAttempt == 1) {
            throw new AssertionError("first attempt");
        }
        assertEquals(2, currentAttempt);
        assertEquals(2, beforeEachCalls.get(), "retry should run with a fresh setup call");
        assertEquals(1, afterEachCalls.get(), "failed attempt teardown should run before retry");
    }
}

class ExhaustedLauncherRetryFixture {
    static final AtomicInteger attempts = new AtomicInteger();
    static final AtomicInteger beforeEachCalls = new AtomicInteger();
    static final AtomicInteger afterEachCalls = new AtomicInteger();

    static void reset() {
        attempts.set(0);
        beforeEachCalls.set(0);
        afterEachCalls.set(0);
    }

    @BeforeEach
    void setup() {
        beforeEachCalls.incrementAndGet();
    }

    @AfterEach
    void teardown() {
        afterEachCalls.incrementAndGet();
    }

    @Test
    void alwaysFails() {
        int currentAttempt = attempts.incrementAndGet();
        if (currentAttempt == 2) {
            assertEquals(2, beforeEachCalls.get(), "retry should run with a fresh setup call");
            assertEquals(1, afterEachCalls.get(), "failed attempt teardown should run before retry");
            throw new AssertionError("retry attempt");
        }
        throw new AssertionError("first attempt");
    }
}

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class OuterSessionStateRetryFixture {
    static final AtomicInteger attempts = new AtomicInteger();

    static void reset() {
        attempts.set(0);
    }

    @BeforeAll
    static void setupClass() {
        SHAFT.Properties.web.set().baseURL("https://class-session.example");
    }

    @Test
    @Order(1)
    void failsOnceThenPasses() {
        int currentAttempt = attempts.incrementAndGet();
        assertEquals("https://class-session.example", SHAFT.Properties.web.baseURL());
        if (currentAttempt == 1) {
            throw new AssertionError("first attempt");
        }
        assertEquals(2, currentAttempt);
    }

    @Test
    @Order(2)
    void classSessionStateSurvivesRetryLifecycle() {
        assertEquals("https://class-session.example", SHAFT.Properties.web.baseURL());
    }
}
