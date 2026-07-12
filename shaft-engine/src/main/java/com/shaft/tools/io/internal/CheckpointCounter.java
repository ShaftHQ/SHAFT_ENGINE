package com.shaft.tools.io.internal;

import com.shaft.tools.internal.support.HTMLHelper;
import com.shaft.tools.internal.support.ReportHtmlTheme;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Aggregates validation checkpoints and attaches a summarized HTML report to Allure.
 *
 * <p>Each call to {@link #increment(CheckpointType, String, CheckpointStatus)} stores one checkpoint
 * entry and updates pass/fail counters. Call {@link #attach()} once at the end of execution to
 * publish the generated report attachment.
 */
public class CheckpointCounter {
    private static final List<ArrayList<String>> checkpoints = new CopyOnWriteArrayList<>();
    private static final AtomicInteger passedCheckpoints = new AtomicInteger(0);
    private static final AtomicInteger failedCheckpoints = new AtomicInteger(0);

    /**
     * Creates a new checkpoint counter instance.
     */
    public CheckpointCounter() {
        super();
    }

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
        checkpoints.add(entry);

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
        int checkpointId = 0;
        for (ArrayList<String> value : checkpoints) {
            detailsBuilder.append(String.format(
                    HTMLHelper.CHECKPOINT_DETAILS_FORMAT.getValue(),
                    ++checkpointId,
                    ReportHtmlTheme.escapeHtml(value.get(0)),
                    ReportHtmlTheme.escapeHtml(value.get(1)),
                    ReportHtmlTheme.statusClass(value.get(2)),
                    value.get(2)));
        }

        ReportManagerHelper.attach("HTML",
                "Checkpoints Report",
                HTMLHelper.CHECKPOINT_COUNTER.getValue()
                        .replace("${CHECKPOINTS_PASSED_PERCENTAGE_LABEL}", String.valueOf(Math.round(passedCheckpoints.get() * 100d / checkpoints.size())))
                        .replace("${CHECKPOINTS_PASSED_DEGREES}", String.valueOf(passedCheckpoints.get() * 360d / checkpoints.size()))
                        .replace("${CHECKPOINTS_TOTAL}", String.valueOf(checkpoints.size()))
                        .replace("${CHECKPOINTS_PASSED}", String.valueOf(passedCheckpoints.get()))
                        .replace("${CHECKPOINTS_FAILED}", String.valueOf(failedCheckpoints.get()))
                        .replace("${CHECKPOINTS_DETAILS}", detailsBuilder));
    }
}
