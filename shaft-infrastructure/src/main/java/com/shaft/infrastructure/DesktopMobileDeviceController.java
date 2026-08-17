package com.shaft.infrastructure;

import java.io.IOException;

/** Host device boundary used by desktop-mobile runtime start/stop. */
interface DesktopMobileDeviceController {
    enum SimulatorState { BOOTED, SHUTDOWN, MISSING }

    default SimulatorState simulatorState(String udid) throws IOException {
        throw new UnsupportedOperationException("This host does not own iOS Simulator lifecycle.");
    }

    default void bootSimulator(String udid) throws IOException {
        throw new UnsupportedOperationException("This host does not own iOS Simulator lifecycle.");
    }

    default void shutdownSimulator(String udid) throws IOException {
        throw new UnsupportedOperationException("This host does not own iOS Simulator lifecycle.");
    }
}
