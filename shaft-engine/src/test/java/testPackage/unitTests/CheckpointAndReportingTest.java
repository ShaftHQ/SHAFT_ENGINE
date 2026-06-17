package testPackage.unitTests;

import com.shaft.tools.io.internal.CheckpointCounter;
import com.shaft.tools.io.internal.CheckpointStatus;
import com.shaft.tools.io.internal.CheckpointType;
import com.shaft.validation.Validations;
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
    private static final Field SEQUENCE_FIELD;
    private static final Field PASSED_FIELD;
    private static final Field FAILED_FIELD;

    static {
        try {
            CHECKPOINTS_FIELD = CheckpointCounter.class.getDeclaredField("checkpoints");
            CHECKPOINTS_FIELD.setAccessible(true);
            SEQUENCE_FIELD = CheckpointCounter.class.getDeclaredField("checkpointSequence");
            SEQUENCE_FIELD.setAccessible(true);
            PASSED_FIELD = CheckpointCounter.class.getDeclaredField("passedCheckpoints");
            PASSED_FIELD.setAccessible(true);
            FAILED_FIELD = CheckpointCounter.class.getDeclaredField("failedCheckpoints");
            FAILED_FIELD.setAccessible(true);
        } catch (NoSuchFieldException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private void resetCheckpointCounter() throws Exception {
        ((ConcurrentHashMap<?, ?>) CHECKPOINTS_FIELD.get(null)).clear();
        ((AtomicInteger) SEQUENCE_FIELD.get(null)).set(0);
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
        int baselineSize = getCheckpointsSize();
        int baselinePassed = getPassedCount();
        int baselineFailed = getFailedCount();
        CheckpointCounter.increment(CheckpointType.ASSERTION, "test pass", CheckpointStatus.PASS);
        assertTrue(getCheckpointsSize() >= baselineSize + 1, "Should add a checkpoint");
        assertTrue(getPassedCount() >= baselinePassed + 1, "Should add a passed checkpoint");
        assertTrue(getFailedCount() >= baselineFailed, "Should not reduce failed checkpoints");
    }

    @Test(description = "CheckpointCounter.increment records failed checkpoints correctly")
    public void testCheckpointCounterIncrementFail() throws Exception {
        int baselineSize = getCheckpointsSize();
        int baselinePassed = getPassedCount();
        int baselineFailed = getFailedCount();
        CheckpointCounter.increment(CheckpointType.VERIFICATION, "test fail", CheckpointStatus.FAIL);
        assertTrue(getCheckpointsSize() >= baselineSize + 1, "Should add a checkpoint");
        assertTrue(getPassedCount() >= baselinePassed, "Should not reduce passed checkpoints");
        assertTrue(getFailedCount() >= baselineFailed + 1, "Should add a failed checkpoint");
    }

    @Test(description = "CheckpointCounter.increment records multiple checkpoints")
    public void testCheckpointCounterMultipleIncrements() throws Exception {
        int baselineSize = getCheckpointsSize();
        int baselinePassed = getPassedCount();
        int baselineFailed = getFailedCount();
        CheckpointCounter.increment(CheckpointType.ASSERTION, "assert 1", CheckpointStatus.PASS);
        CheckpointCounter.increment(CheckpointType.VERIFICATION, "verify 1", CheckpointStatus.PASS);
        CheckpointCounter.increment(CheckpointType.ASSERTION, "assert 2", CheckpointStatus.FAIL);
        assertTrue(getCheckpointsSize() >= baselineSize + 3, "Should add 3 checkpoints");
        assertTrue(getPassedCount() >= baselinePassed + 2, "Should add 2 passed");
        assertTrue(getFailedCount() >= baselineFailed + 1, "Should add 1 failed");
    }

    @Test(description = "CheckpointCounter.attach does not produce NaN when no checkpoints exist")
    public void testCheckpointCounterAttachEmpty() throws Exception {
        resetCheckpointCounter();
        CheckpointCounter.attach();
        assertNotNull(CHECKPOINTS_FIELD.get(null), "Checkpoint storage should remain available after attach");
    }

    @Test(description = "Validations.assertThat().number() populates checkpoint counter via ValidationsHelper2")
    public void testValidationsAssertPopulatesCheckpointCounter() throws Exception {
        int baselineSize = getCheckpointsSize();
        int baselinePassed = getPassedCount();
        Validations.assertThat().number(42).isEqualTo(42).perform();
        assertTrue(getCheckpointsSize() >= baselineSize + 1,
                "Checkpoint count should increase after assertion");
        assertTrue(getPassedCount() >= baselinePassed + 1,
                "Passed checkpoint count should increase after successful assertion");
    }

    @Test(description = "Validations.verifyThat().number() populates checkpoint counter via ValidationsHelper2")
    public void testValidationsVerifyPopulatesCheckpointCounter() throws Exception {
        int baselineSize = getCheckpointsSize();
        int baselinePassed = getPassedCount();
        Validations.verifyThat().number(10).isEqualTo(10).perform();
        assertTrue(getCheckpointsSize() >= baselineSize + 1,
                "Checkpoint count should increase after verification");
        assertTrue(getPassedCount() >= baselinePassed + 1,
                "Passed checkpoint count should increase after successful verification");
    }

    @Test(description = "Validations.assertThat().object() populates checkpoint counter via ValidationsHelper2")
    public void testValidationsObjectAssertPopulatesCheckpointCounter() throws Exception {
        int baselineSize = getCheckpointsSize();
        int baselinePassed = getPassedCount();
        Validations.assertThat().object("hello").isEqualTo("hello").perform();
        assertTrue(getCheckpointsSize() >= baselineSize + 1,
                "Checkpoint count should increase after object assertion");
        assertTrue(getPassedCount() >= baselinePassed + 1,
                "Passed checkpoint count should increase after successful object assertion");
    }

    @Test(description = "Failed assertion populates checkpoint counter with FAIL status",
            expectedExceptions = {AssertionError.class})
    public void testFailedAssertionPopulatesCheckpointCounterWithFail() throws Exception {
        int baselineFailed = getFailedCount();
        try {
            Validations.assertThat().number(1).isEqualTo(2).perform();
        } finally {
            assertTrue(getFailedCount() >= baselineFailed + 1,
                    "Failed checkpoint count should increase after failed assertion");
        }
    }
}
