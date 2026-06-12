package com.shaft.gui.internal.healing;

/**
 * Optional visual comparison contract used only when SHAFT Heal visual evidence
 * is explicitly enabled.
 */
public interface HealingVisualProvider {
    /**
     * @return stable provider identifier
     */
    String id();

    /**
     * Compares a previously approved element image with a current candidate.
     *
     * @param referenceImage previous element image
     * @param candidateImage current candidate image
     * @return normalized similarity from 0 to 1
     */
    double similarity(byte[] referenceImage, byte[] candidateImage);
}
