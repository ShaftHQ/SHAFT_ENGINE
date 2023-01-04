package io.github.shafthq.shaft.tools.io.helpers;

import io.github.shafthq.shaft.tools.support.HTMLHelper;

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
        checkpoints.entrySet()
                .forEach(entry -> detailsBuilder.append(String.format(HTMLHelper.CHECKPOINT_DETAILS_FORMAT.getValue(), entry.getKey(), entry.getValue().get(0), entry.getValue().get(1), entry.getValue().get(2))));

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
