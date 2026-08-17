package com.shaft.infrastructure;

import java.io.IOException;
import java.net.URI;
import java.time.Duration;

interface DesktopMobileRuntimeHealth {
    void awaitAppium(URI endpoint, Duration timeout) throws IOException;

    default void awaitSimulator(String udid, Duration timeout) throws IOException {
        throw new UnsupportedOperationException("This host does not own iOS Simulator readiness.");
    }
}
