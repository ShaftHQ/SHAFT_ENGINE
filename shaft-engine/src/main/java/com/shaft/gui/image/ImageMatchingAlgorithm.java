package com.shaft.gui.image;

/** Identifies the algorithm that produced an {@link ImageMatch}. */
public enum ImageMatchingAlgorithm {
    TEMPLATE_COLOR,
    TEMPLATE_GRAYSCALE,
    TEMPLATE_CLAHE,
    TEMPLATE_EDGE,
    FEATURE_HOMOGRAPHY,
    APPIUM_IMAGES
}
