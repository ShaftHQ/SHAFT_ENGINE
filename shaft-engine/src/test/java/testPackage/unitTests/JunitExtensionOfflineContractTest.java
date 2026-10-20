package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.JunitExtension;
import com.shaft.listeners.internal.ExecutionFailureContext;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.internal.ValidationsHelper;
import io.qameta.allure.Issue;
import io.qameta.allure.Issues;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.opentest4j.TestAbortedException;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Drives {@link JunitExtension} through the TestNG unit shard. The existing Jupiter tests are not
 * part of that shard, so Codecov recorded the extension as uncovered.
 */
public class JunitExtensionOfflineContractTest {
    static final String RETRY_TARGET_PROPERTY = "shaft.offlineContract.retryTarget";

    private final JunitExtension extension = new JunitExtension();
    private int originalRetryCount;
    private boolean originalSkipLinkedIssues;

    @BeforeMethod(alwaysRun = true)
    public void rememberFlags() {
        System.setProperty(RETRY_TARGET_PROPERTY, "true");
        originalRetryCount = SHAFT.Properties.flags.retryMaximumNumberOfAttempts();
        originalSkipLinkedIssues = SHAFT.Properties.flags.skipTestsWithLinkedIssues();
    }

    @AfterMethod(alwaysRun = true)
    public void restoreFlags() {
        System.clearProperty(RETRY_TARGET_PROPERTY);
        SHAFT.Properties.flags.set()
                .retryMaximumNumberOfAttempts(originalRetryCount)
                .skipTestsWithLinkedIssues(originalSkipLinkedIssues);
        ValidationsHelper.resetVerificationStateAfterFailing();
        ExecutionFailureContext.getAndClearPendingConfigFailure();
        Properties.clearForCurrentThread();
    }

    @Test
    public void quietLifecycleStaysLocal() throws Exception {
        ExtensionContext context = contextFor(method("plainFixture"), JunitExtensionOfflineContractTest.class);
        extension.beforeAll(context);
        extension.beforeEach(context);
        extension.afterTestExecution(context);
        extension.afterEach(context);
        extension.afterAll(context);
    }

    @Test
    public void linkedIssuesAbortBeforeEach() throws Exception {
        SHAFT.Properties.flags.set().skipTestsWithLinkedIssues(true);
        ExtensionContext single = contextFor(method("linkedIssueFixture"), JunitExtensionOfflineContractTest.class);
        TestAbortedException one = Assert.expectThrows(TestAbortedException.class, () -> extension.beforeEach(single));
        Assert.assertEquals(one.getMessage(),
                "Skipping Test as it's expected to fail due to open issue: [SHAFT-123]");

        ExtensionContext several = contextFor(method("linkedIssuesFixture"), JunitExtensionOfflineContractTest.class);
        TestAbortedException many = Assert.expectThrows(TestAbortedException.class, () -> extension.beforeEach(several));
        Assert.assertTrue(many.getMessage().contains("SHAFT-1"), many.getMessage());
        Assert.assertTrue(many.getMessage().contains("SHAFT-2"), many.getMessage());
    }

    @Test
    public void configurationFailuresStayPending() throws Throwable {
        ExtensionContext context = contextFor(method("plainFixture"), JunitExtensionOfflineContractTest.class);
        RuntimeException failure = new RuntimeException("setup failed");
        Assert.assertSame(failure, Assert.expectThrows(RuntimeException.class,
                () -> extension.handleBeforeAllMethodExecutionException(context, failure)));
        Assert.assertSame(failure, ExecutionFailureContext.getAndClearPendingConfigFailure());
        Assert.assertSame(failure, Assert.expectThrows(RuntimeException.class,
                () -> extension.handleBeforeEachMethodExecutionException(context, failure)));
        Assert.assertSame(failure, ExecutionFailureContext.getAndClearPendingConfigFailure());
        Assert.assertSame(failure, Assert.expectThrows(RuntimeException.class,
                () -> extension.handleAfterEachMethodExecutionException(context, failure)));
        Assert.assertSame(failure, ExecutionFailureContext.getAndClearPendingConfigFailure());
        Assert.assertSame(failure, Assert.expectThrows(RuntimeException.class,
                () -> extension.handleAfterAllMethodExecutionException(context, failure)));
        Assert.assertSame(failure, ExecutionFailureContext.getAndClearPendingConfigFailure());
    }

    @Test
    public void softVerificationAndDisabledRetryStayOnTheCallingTest() throws Throwable {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0);
        ExtensionContext context = contextFor(method("plainFixture"), JunitExtensionOfflineContractTest.class);
        SHAFT.Validations.verifyThat().object("actual").isEqualTo("expected").perform();
        AssertionError verification = Assert.expectThrows(AssertionError.class,
                () -> extension.afterTestExecution(context));
        Assert.assertTrue(verification.getMessage().contains("actual"), verification.getMessage());

        AssertionError original = new AssertionError("first attempt");
        Assert.assertSame(original, Assert.expectThrows(AssertionError.class,
                () -> extension.handleTestExecutionException(context, original)));
    }

    @Test
    public void scheduledRetryRerunsAPassingMethodLocally() throws Throwable {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1).skipTestsWithLinkedIssues(false);
        PassingRetryTarget.calls = 0;
        Method method = PassingRetryTarget.class.getDeclaredMethod("passes");
        ExtensionContext context = contextFor(method, PassingRetryTarget.class);
        extension.handleTestExecutionException(context, new AssertionError("first attempt"));
        extension.afterEach(context);
        Assert.assertEquals(PassingRetryTarget.calls, 1);
    }

    @Test
    public void exhaustedRetrySurfacesTheLastFailure() throws Throwable {
        SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(1).skipTestsWithLinkedIssues(false);
        Method method = FailingRetryTarget.class.getDeclaredMethod("fails");
        ExtensionContext context = contextFor(method, FailingRetryTarget.class);
        extension.handleTestExecutionException(context, new AssertionError("first attempt"));
        AssertionError thrown = Assert.expectThrows(AssertionError.class, () -> extension.afterEach(context));
        Assert.assertTrue(thrown.getMessage().contains("retry attempt"), thrown.getMessage());
    }

    @Issue("SHAFT-123")
    private void linkedIssueFixture() {
        // Used by reflection.
    }

    @Issues({@Issue("SHAFT-1"), @Issue("SHAFT-2")})
    private void linkedIssuesFixture() {
        // Used by reflection.
    }

    private void plainFixture() {
        // Used by reflection.
    }

    private Method method(String name) throws NoSuchMethodException {
        return JunitExtensionOfflineContractTest.class.getDeclaredMethod(name);
    }

    private static ExtensionContext contextFor(Method method, Class<?> type) {
        ExtensionContext context = mock(ExtensionContext.class);
        Map<Object, Object> saved = new HashMap<>();
        ExtensionContext.Store store = mock(ExtensionContext.Store.class);
        when(store.get(any(), any(Class.class))).thenAnswer(invocation -> saved.get(invocation.getArgument(0)));
        when(store.remove(any(), any(Class.class))).thenAnswer(invocation -> saved.remove(invocation.getArgument(0)));
        doAnswer(invocation -> {
            saved.put(invocation.getArgument(0), invocation.getArgument(1));
            return null;
        }).when(store).put(any(), any());
        when(context.getStore(any())).thenReturn(store);
        when(context.getTestMethod()).thenReturn(Optional.of(method));
        when(context.getRequiredTestMethod()).thenReturn(method);
        when(context.getRequiredTestClass()).thenAnswer(invocation -> type);
        when(context.getUniqueId()).thenReturn("[engine:junit-jupiter]/[class:" + type.getName()
                + "]/[method:missing()]");
        when(context.getDisplayName()).thenReturn(method.getName());
        when(context.getExecutionException()).thenReturn(Optional.empty());
        return context;
    }

    /**
     * Retry targets are driven by this TestNG contract only. Without the guard, JUnit Platform
     * discovery in the {@code junit} profile runs them as standalone tests and the deliberately
     * failing target fails Ubuntu_JUnit_E2E.
     */
    @org.junit.jupiter.api.condition.EnabledIfSystemProperty(named = RETRY_TARGET_PROPERTY, matches = "true")
    public static class PassingRetryTarget {
        static int calls;

        @org.junit.jupiter.api.Test
        public void passes() {
            calls++;
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0);
        }
    }

    /**
     * Retry targets are driven by this TestNG contract only. Without the guard, JUnit Platform
     * discovery in the {@code junit} profile runs them as standalone tests and the deliberately
     * failing target fails Ubuntu_JUnit_E2E.
     */
    @org.junit.jupiter.api.condition.EnabledIfSystemProperty(named = RETRY_TARGET_PROPERTY, matches = "true")
    public static class FailingRetryTarget {
        @org.junit.jupiter.api.Test
        public void fails() {
            SHAFT.Properties.flags.set().retryMaximumNumberOfAttempts(0);
            throw new AssertionError("retry attempt");
        }
    }
}
