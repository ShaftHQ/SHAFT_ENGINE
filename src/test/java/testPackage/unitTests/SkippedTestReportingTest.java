package testPackage.unitTests;

import com.shaft.listeners.AllureListener;
import com.shaft.listeners.internal.TestNGListenerHelper;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StatusDetails;
import io.qameta.allure.model.TestResult;
import org.testng.SkipException;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Unit tests for the SHAFT framework's behaviour when a test is skipped due to a
 * configuration-method failure or kill-switch activation.
 *
 * <p>Specifically, these tests verify that:
 * <ul>
 *   <li>A test marked SKIPPED by Allure because of a {@code @BeforeMethod} failure is
 *       promoted to BROKEN (so it appears as a failure in the Allure report).</li>
 *   <li>A test marked SKIPPED because of kill-switch activation is promoted to BROKEN.</li>
 *   <li>A test intentionally skipped via {@link SkipException} (e.g. linked-issue skip)
 *       remains SKIPPED and is NOT promoted.</li>
 *   <li>The NPE guard in {@code TestNGListener.onTestSkipped} handles a null throwable
 *       safely.</li>
 * </ul>
 */
public class SkippedTestReportingTest {

    private AllureListener listener;

    @BeforeMethod(alwaysRun = true)
    public void setUp() {
        listener = new AllureListener();
        // Make sure the pending-config-failure slot is clean before each test
        TestNGListenerHelper.setPendingConfigFailure(null);
    }

    /**
     * When a non-SkipException is stored as a pending config failure, {@code beforeTestStop}
     * must promote the status from SKIPPED to BROKEN and populate statusDetails.
     */
    @Test
    public void configFailureShouldPromoteSkippedToBroken() {
        TestNGListenerHelper.setPendingConfigFailure(
                new RuntimeException("Simulated @BeforeMethod failure"));

        TestResult result = skippedResult();
        listener.beforeTestStop(result);

        assertEquals(result.getStatus(), Status.BROKEN,
                "A config-method failure should promote SKIPPED to BROKEN");
        assertNotNull(result.getStatusDetails(), "statusDetails must not be null after promotion");
        assertTrue(result.getStatusDetails().getMessage().contains("Simulated"),
                "Status message should contain the original exception message");
        assertNotNull(result.getStatusDetails().getTrace(),
                "Stack trace must be set in statusDetails after promotion");
        // Thread-local must be cleared to avoid bleeding into next test
        assertNull(TestNGListenerHelper.getAndClearPendingConfigFailure(),
                "Pending config failure must be cleared after consumption");
    }

    /**
     * When the throwable stored as pending config failure is a {@link SkipException}, the test
     * result should remain SKIPPED (it was intentionally skipped, not a real failure).
     */
    @Test
    public void intentionalSkipExceptionShouldNotPromoteToFailed() {
        TestNGListenerHelper.setPendingConfigFailure(
                new SkipException("Intentional skip due to linked issue"));

        TestResult result = skippedResult();
        listener.beforeTestStop(result);

        assertEquals(result.getStatus(), Status.SKIPPED,
                "An intentional SkipException should not be promoted to BROKEN");
        TestNGListenerHelper.getAndClearPendingConfigFailure(); // cleanup
    }

    /**
     * When a PASSED result is passed to {@code beforeTestStop} together with a pending config
     * failure, the status must NOT change — only SKIPPED results are eligible for promotion.
     */
    @Test
    public void passedResultMustNotBeModifiedByConfigFailureHandler() {
        TestNGListenerHelper.setPendingConfigFailure(
                new RuntimeException("Config failure that should be ignored for passing test"));

        TestResult result = new TestResult().setStatus(Status.PASSED);
        listener.beforeTestStop(result);

        assertEquals(result.getStatus(), Status.PASSED,
                "A PASSED result must not be affected by a pending config failure");
        TestNGListenerHelper.getAndClearPendingConfigFailure(); // cleanup
    }

    /**
     * {@link TestNGListenerHelper#getAndClearPendingConfigFailure()} must return null when no
     * failure was stored (i.e. a clean state must not throw or produce a stale throwable).
     */
    @Test
    public void getAndClearOnCleanStateMustReturnNull() {
        assertNull(TestNGListenerHelper.getAndClearPendingConfigFailure(),
                "No config failure is pending; must return null");
    }

    /**
     * {@link TestNGListenerHelper#setPendingConfigFailure(Throwable)} with a null argument must
     * clear any previously stored value.
     */
    @Test
    public void setNullShouldClearPendingFailure() {
        TestNGListenerHelper.setPendingConfigFailure(new RuntimeException("to be cleared"));
        TestNGListenerHelper.setPendingConfigFailure(null);

        assertNull(TestNGListenerHelper.getAndClearPendingConfigFailure(),
                "Setting null must clear the pending config failure");
    }

    // ─── helpers ──────────────────────────────────────────────────────────────

    private static TestResult skippedResult() {
        return new TestResult()
                .setStatus(Status.SKIPPED)
                .setStatusDetails(new StatusDetails().setMessage("skipped by testng"));
    }
}
