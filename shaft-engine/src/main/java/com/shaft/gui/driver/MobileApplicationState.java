package com.shaft.gui.driver;

/** Backend-neutral mobile application lifecycle states. */
public enum MobileApplicationState {
    NOT_INSTALLED,
    NOT_RUNNING,
    RUNNING_IN_BACKGROUND_SUSPENDED,
    RUNNING_IN_BACKGROUND,
    RUNNING_IN_FOREGROUND
}
