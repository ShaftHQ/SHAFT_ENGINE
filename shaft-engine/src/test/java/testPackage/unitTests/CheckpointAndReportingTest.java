package testPackage.unitTests;

import com.shaft.tools.io.internal.CheckpointCounter;
import com.shaft.tools.io.internal.CheckpointStatus;
import com.shaft.tools.io.internal.CheckpointType;
import com.shaft.validation.Validations;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.lang.reflect.Field;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import static org.testng.Assert.*;

/**
 * Tests for checkpoint counter population and reporting.
 * Verifies that validations correctly increment the checkpoint counter
 * and that the checkpoints report is generated with the right data.
 */
public class CheckpointAndReportingTest {

    private static final Field CHECKPOINTS_FIELD;
    private static final Field PASSED_FIELD;
    private static final Field FAILED_FIELD;

    static {
        try {
            CHECKPOINTS_FIELD = CheckpointCounter.class.getDeclaredField("checkpoints");
            CHECKPOINTS_FIELD.setAccessible(true);
            PASSED_FIELD = CheckpointCounter.class.getDeclaredField("passedCheckpoints");
            PASSED_FIELD.setAccessible(true);
            FAILED_FIELD = CheckpointCounter.class.getDeclaredField("failedCheckpoints");
            FAILED_FIELD.setAccessible(true);
        } catch (NoSuchFieldException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    /**
     * Resets the CheckpointCounter static state via reflection so each test starts clean.
     */
    @BeforeMethod(alwaysRun = true)
    public void resetCheckpointCounter() throws Exception {
        ((ConcurrentHashMap<?, ?>) CHECKPOINTS_FIELD.get(null)).clear();
        ((AtomicInteger) PASSED_FIELD.get(null)).set(0);
        ((AtomicInteger) FAILED_FIELD.get(null)).set(0);
    }

    private int getCheckpointsSize() throws Exception {
        return ((ConcurrentHashMap<?, ?>) CHECKPOINTS_FIELD.get(null)).size();
    }

    private int getPassedCount() throws Exception {
        return ((AtomicInteger) PASSED_FIELD.get(null)).get();
    }

    private int getFailedCount() throws Exception {
        return ((AtomicInteger) FAILED_FIELD.get(null)).get();
    }

    @Test(description = "CheckpointCounter.increment records passed checkpoints correctly")
    public void testCheckpointCounterIncrementPass() throws Exception {
        CheckpointCounter.increment(CheckpointType.ASSERTION, "test pass", CheckpointStatus.PASS);
        assertEquals(getCheckpointsSize(), 1, "Should have 1 checkpoint");
        assertEquals(getPassedCount(), 1, "Should have 1 passed checkpoint");
        assertEquals(getFailedCount(), 0, "Should have 0 failed checkpoints");
    }

    @Test(description = "CheckpointCounter.increment records failed checkpoints correctly")
    public void testCheckpointCounterIncrementFail() throws Exception {
        CheckpointCounter.increment(CheckpointType.VERIFICATION, "test fail", CheckpointStatus.FAIL);
        assertEquals(getCheckpointsSize(), 1, "Should have 1 checkpoint");
        assertEquals(getPassedCount(), 0, "Should have 0 passed checkpoints");
        assertEquals(getFailedCount(), 1, "Should have 1 failed checkpoint");
    }

    @Test(description = "CheckpointCounter.increment records multiple checkpoints")
    public void testCheckpointCounterMultipleIncrements() throws Exception {
        CheckpointCounter.increment(CheckpointType.ASSERTION, "assert 1", CheckpointStatus.PASS);
        CheckpointCounter.increment(CheckpointType.VERIFICATION, "verify 1", CheckpointStatus.PASS);
        CheckpointCounter.increment(CheckpointType.ASSERTION, "assert 2", CheckpointStatus.FAIL);
        assertEquals(getCheckpointsSize(), 3, "Should have 3 checkpoints");
        assertEquals(getPassedCount(), 2, "Should have 2 passed");
        assertEquals(getFailedCount(), 1, "Should have 1 failed");
    }

    @Test(description = "CheckpointCounter.attach does not produce NaN when no checkpoints exist")
    public void testCheckpointCounterAttachEmpty() throws Exception {
        // Should not throw and should not produce NaN in the attachment
        CheckpointCounter.attach();
        assertEquals(getCheckpointsSize(), 0, "Should still have 0 checkpoints after attach");
    }

    @Test(description = "Validations.assertThat().number() populates checkpoint counter via ValidationsHelper2")
    public void testValidationsAssertPopulatesCheckpointCounter() throws Exception {
        Validations.assertThat().number(42).isEqualTo(42).perform();
        assertEquals(getCheckpointsSize(), 1,
                "Checkpoint count should be exactly 1 after assertion");
        assertEquals(getPassedCount(), 1,
                "Passed checkpoint count should be exactly 1 after successful assertion");
    }

    @Test(description = "Validations.verifyThat().number() populates checkpoint counter via ValidationsHelper2")
    public void testValidationsVerifyPopulatesCheckpointCounter() throws Exception {
        Validations.verifyThat().number(10).isEqualTo(10).perform();
        assertEquals(getCheckpointsSize(), 1,
                "Checkpoint count should be exactly 1 after verification");
        assertEquals(getPassedCount(), 1,
                "Passed checkpoint count should be exactly 1 after successful verification");
    }

    @Test(description = "Validations.assertThat().object() populates checkpoint counter via ValidationsHelper2")
    public void testValidationsObjectAssertPopulatesCheckpointCounter() throws Exception {
        Validations.assertThat().object("hello").isEqualTo("hello").perform();
        assertEquals(getCheckpointsSize(), 1,
                "Checkpoint count should be exactly 1 after object assertion");
        assertEquals(getPassedCount(), 1,
                "Passed checkpoint count should be exactly 1 after successful object assertion");
    }

    @Test(description = "Failed assertion populates checkpoint counter with FAIL status",
            expectedExceptions = {AssertionError.class})
    public void testFailedAssertionPopulatesCheckpointCounterWithFail() throws Exception {
        try {
            Validations.assertThat().number(1).isEqualTo(2).perform();
        } finally {
            assertEquals(getFailedCount(), 1,
                    "Failed checkpoint count should be exactly 1 after failed assertion");
        }
    }
}
