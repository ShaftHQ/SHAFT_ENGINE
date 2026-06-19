package junitTestPackage;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.JunitExtension;
import com.shaft.listeners.internal.ExecutionFailureContext;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.internal.ValidationsHelper;
import io.qameta.allure.Issue;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.InvocationInterceptor;
import org.junit.jupiter.api.extension.ReflectiveInvocationContext;
import org.opentest4j.TestAbortedException;

import java.lang.reflect.Method;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
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
    void interceptTestMethodShouldRetryFailedInvocation() throws Throwable {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1);
        AtomicInteger attempts = new AtomicInteger();
        InvocationInterceptor.Invocation<Void> invocation = () -> {
            if (attempts.incrementAndGet() == 1) {
                throw new AssertionError("first attempt");
            }
            return null;
        };
        ReflectiveInvocationContext<Method> invocationContext = mockReflectiveInvocationContext(
                JunitExtensionLifecycleTest.class.getDeclaredMethod("retryFixture"));
        ExtensionContext extensionContext = mockExtensionContext("interceptTestMethodShouldRetryFailedInvocation");

        extension.interceptTestMethod(invocation, invocationContext, extensionContext);

        assertEquals(2, attempts.get());
    }

    @Issue("SHAFT-123")
    private void linkedIssueFixture() {
        // Used by reflection.
    }

    private void retryFixture() {
        // Used by reflection.
    }

    @SuppressWarnings("unchecked")
    private static ReflectiveInvocationContext<Method> mockReflectiveInvocationContext(Method method) {
        ReflectiveInvocationContext<Method> context = mock(ReflectiveInvocationContext.class);
        when(context.getExecutable()).thenReturn(method);
        return context;
    }

    private static ExtensionContext mockExtensionContext(String displayName) {
        ExtensionContext context = mock(ExtensionContext.class);
        when(context.getRequiredTestClass()).thenAnswer(invocation -> JunitExtensionLifecycleTest.class);
        when(context.getUniqueId()).thenReturn("[engine:junit-jupiter]/[class:junitTestPackage.JunitExtensionLifecycleTest]/[method:" + displayName + "()]");
        when(context.getDisplayName()).thenReturn(displayName);
        return context;
    }
}
