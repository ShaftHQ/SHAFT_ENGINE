package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.Locator;
import io.qameta.allure.model.Status;
import io.qameta.allure.model.StepResult;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Map;
import java.util.Properties;
import java.util.function.Supplier;

/**
 * Lightweight performance budget for engine hot paths (no JMH, zero extra dependencies).
 * <p>
 * Each path runs {@value #BATCH} operations per sample; the median nanoseconds per operation is compared with
 * {@code src/test/resources/performance/budget-baseline.properties} and fails above {@value #THRESHOLD}x.
 * <p>
 * Baseline update procedure: run this test, copy the {@code measured} values printed in the failure or log line,
 * multiply by about 3 so slower CI runners stay green, and commit the new baseline in the same PR as the
 * intentional change, explaining why in the PR body.
 */
public class PerformanceBudgetTest {
    static final double THRESHOLD = 2.0;
    private static final int BATCH = 2_000;
    private static final int SAMPLES = 15;

    /**
     * Property proxy reads, Locator builder chains and report-step model creation stay inside the budget.
     */
    @Test
    public void hotPathsShouldStayWithinBudget() throws IOException {
        Properties baseline = baseline();
        Map<String, Supplier<Object>> paths = Map.of(
                "propertyRead", () -> SHAFT.Properties.flags.retryMaximumNumberOfAttempts(),
                "locatorBuild", () -> Locator.hasTagName("button").hasAttribute("type", "submit").containsText("Go").build(),
                "reportStep", () -> new StepResult().setName("Click \"Go\"").setStatus(Status.PASSED).setStart(System.currentTimeMillis()));
        StringBuilder measured = new StringBuilder();
        paths.forEach((name, op) -> {
            double nanos = medianNanosPerOp(op);
            measured.append(name).append('=').append(Math.round(nanos)).append(' ');
            double limit = Double.parseDouble(baseline.getProperty(name)) * THRESHOLD;
            Assert.assertFalse(exceedsBudget(nanos, Double.parseDouble(baseline.getProperty(name))),
                    name + " took " + nanos + " ns/op; budget " + limit + " ns/op. measured: " + measured);
        });
        System.out.println("PerformanceBudgetTest measured: " + measured);
    }

    /**
     * A deliberate 3x slowdown fails the comparator while normal jitter passes.
     */
    @Test
    public void comparatorShouldFailThreeTimesSlowdown() {
        Assert.assertTrue(exceedsBudget(300, 100));
        Assert.assertFalse(exceedsBudget(150, 100));
    }

    /**
     * Injected sleep that triples the cost of an operation is caught by the real measurement path.
     */
    @Test
    public void injectedSlowdownShouldFailBudget() {
        double fast = medianNanosPerOp(() -> spin(2_000));
        double slow = medianNanosPerOp(() -> spin(8_000));
        Assert.assertTrue(exceedsBudget(slow, fast), "fast=" + fast + " slow=" + slow);
    }

    static boolean exceedsBudget(double measuredNanos, double baselineNanos) {
        return measuredNanos > baselineNanos * THRESHOLD;
    }

    private static double medianNanosPerOp(Supplier<Object> op) {
        Object sink = null;
        for (int i = 0; i < BATCH; i++) {
            sink = op.get();
        }
        double[] samples = new double[SAMPLES];
        for (int s = 0; s < SAMPLES; s++) {
            long start = System.nanoTime();
            for (int i = 0; i < BATCH; i++) {
                sink = op.get();
            }
            samples[s] = (System.nanoTime() - start) / (double) BATCH;
        }
        Assert.assertNotNull(sink);
        Arrays.sort(samples);
        return samples[SAMPLES / 2];
    }

    private static Object spin(long nanos) {
        long end = System.nanoTime() + nanos;
        long n = 0;
        while (System.nanoTime() < end) {
            n++;
        }
        return n;
    }

    private static Properties baseline() throws IOException {
        Properties p = new Properties();
        try (InputStream in = PerformanceBudgetTest.class.getResourceAsStream("/performance/budget-baseline.properties")) {
            Assert.assertNotNull(in, "missing performance baseline");
            p.load(in);
        }
        return p;
    }
}
