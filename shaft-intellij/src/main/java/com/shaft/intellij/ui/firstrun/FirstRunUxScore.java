package com.shaft.intellij.ui.firstrun;

/** Counts the ten first-run inspection probes. */
public final class FirstRunUxScore {
    private FirstRunUxScore() {
    }

    public static int outOfTen(WizardInspection inspection) {
        int score = 0;
        for (boolean probe : inspection.probes()) {
            if (probe) {
                score++;
            }
        }
        return score;
    }
}
