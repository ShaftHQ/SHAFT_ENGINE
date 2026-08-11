package com.shaft.gui.driver;

/** Immutable cross-platform battery reading. */
public record MobileBatteryInfo(double level, String state) {
    public MobileBatteryInfo {
        state = state == null ? "unknown" : state;
    }
}
