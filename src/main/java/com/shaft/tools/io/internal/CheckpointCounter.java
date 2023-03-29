package com.shaft.tools.io.internal;

import com.shaft.tools.internal.support.HTMLHelper;

import java.util.ArrayList;
import java.util.HashMap;

public class CheckpointCounter {
    private static final HashMap<Integer, ArrayList<?>> checkpoints = new HashMap<>();
    private static int passedCheckpoints = 0;
    private static int failedCheckpoints = 0;

    public static void increment(CheckpointType type, String message, CheckpointStatus status) {
        ArrayList<String> entry = new ArrayList<>();
        entry.add(type.toString());
        entry.add(message);
        entry.add(status.toString());
        checkpoints.put(checkpoints.size() + 1, entry);

        if (status == CheckpointStatus.PASS) {
            passedCheckpoints++;
        } else {
            failedCheckpoints++;
        }
    }

    public static void attach() {
        StringBuilder detailsBuilder = new StringBuilder();
        checkpoints.forEach((key, value) -> detailsBuilder.append(String.format(HTMLHelper.CHECKPOINT_DETAILS_FORMAT.getValue(), key, value.get(0), value.get(1), value.get(2))));

        ReportManagerHelper.attach("HTML",
                "Checkpoints Report",
                HTMLHelper.CHECKPOINT_COUNTER.getValue()
                        .replace("${CHECKPOINTS_PASSED_PERCENTAGE}", String.valueOf(passedCheckpoints * 360d / checkpoints.size()))
                        .replace("${CHECKPOINTS_TOTAL}", String.valueOf(checkpoints.size()))
                        .replace("${CHECKPOINTS_PASSED}", String.valueOf(passedCheckpoints))
                        .replace("${CHECKPOINTS_FAILED}", String.valueOf(failedCheckpoints))
                        .replace("${CHECKPOINTS_DETAILS}", detailsBuilder));
    }
}
