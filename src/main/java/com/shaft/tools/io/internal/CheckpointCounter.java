package com.shaft.tools.io.internal;

import com.shaft.tools.internal.support.HTMLHelper;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Aggregates validation checkpoints and attaches a summarized HTML report to Allure.
 *
 * <p>Each call to {@link #increment(CheckpointType, String, CheckpointStatus)} stores one checkpoint
 * entry and updates pass/fail counters. Call {@link #attach()} once at the end of execution to
 * publish the generated report attachment.
 */
public class CheckpointCounter {
    private static final ConcurrentHashMap<Integer, ArrayList<?>> checkpoints = new ConcurrentHashMap<>();
    private static final AtomicInteger passedCheckpoints = new AtomicInteger(0);
    private static final AtomicInteger failedCheckpoints = new AtomicInteger(0);

    /**
     * Creates a new checkpoint entry and updates the pass/fail counters.
     *
     * @param type the checkpoint type (assertion or verification)
     * @param message the checkpoint message
     * @param status the final status of the checkpoint
     */
    public static void increment(CheckpointType type, String message, CheckpointStatus status) {
        ArrayList<String> entry = new ArrayList<>();
        entry.add(type.toString());
        entry.add(message);
        entry.add(status.toString());
        checkpoints.put(checkpoints.size() + 1, entry);

        if (status == CheckpointStatus.PASS) {
            passedCheckpoints.incrementAndGet();
        } else {
            failedCheckpoints.incrementAndGet();
        }
    }

    /**
     * Attaches the accumulated checkpoint summary HTML report to the current report context.
     *
     * <p>If no checkpoints were recorded, this method does nothing.
     */
    public static void attach() {
        if (checkpoints.isEmpty()) {
            return;
        }
        StringBuilder detailsBuilder = new StringBuilder();
        checkpoints.forEach((key, value) -> detailsBuilder.append(String.format(HTMLHelper.CHECKPOINT_DETAILS_FORMAT.getValue(), key, value.get(0), value.get(1), value.get(2))));

        ReportManagerHelper.attach("HTML",
                "Checkpoints Report",
                HTMLHelper.CHECKPOINT_COUNTER.getValue()
                        .replace("${CHECKPOINTS_PASSED_PERCENTAGE}", String.valueOf(passedCheckpoints.get() * 360d / checkpoints.size()))
                        .replace("${CHECKPOINTS_TOTAL}", String.valueOf(checkpoints.size()))
                        .replace("${CHECKPOINTS_PASSED}", String.valueOf(passedCheckpoints.get()))
                        .replace("${CHECKPOINTS_FAILED}", String.valueOf(failedCheckpoints.get()))
                        .replace("${CHECKPOINTS_DETAILS}", detailsBuilder));
    }
}
