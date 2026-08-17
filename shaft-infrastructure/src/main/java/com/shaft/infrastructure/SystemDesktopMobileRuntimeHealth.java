package com.shaft.infrastructure;

import java.io.IOException;
import java.net.URI;
import java.time.Duration;
import java.time.Instant;

final class SystemDesktopMobileRuntimeHealth implements DesktopMobileRuntimeHealth {
    private final SystemAndroidRuntimeHealth appium;
    private final DesktopMobileDeviceController devices;

    SystemDesktopMobileRuntimeHealth(ShaftCachePaths paths, SetupPlan plan,
                                     DesktopMobileDeviceController devices) {
        this(new SystemAndroidRuntimeHealth(paths, plan.platform(), plan.architecture()), devices);
    }

    SystemDesktopMobileRuntimeHealth(SystemAndroidRuntimeHealth appium, DesktopMobileDeviceController devices) {
        this.appium = java.util.Objects.requireNonNull(appium, "appium");
        this.devices = java.util.Objects.requireNonNull(devices, "devices");
    }

    @Override
    public void awaitAppium(URI endpoint, Duration timeout) throws IOException {
        appium.awaitAppium(endpoint, timeout);
    }

    @Override
    public void awaitSimulator(String udid, Duration timeout) throws IOException {
        Instant deadline = Instant.now().plus(timeout);
        IOException last = new IOException("iOS Simulator did not become ready.");
        while (Instant.now().isBefore(deadline)) {
            try {
                if (devices.simulatorState(udid) == DesktopMobileDeviceController.SimulatorState.BOOTED) return;
                last = new IOException("iOS Simulator " + udid + " is not Booted.");
            } catch (IOException notReady) {
                last = notReady;
            }
            long remaining = Duration.between(Instant.now(), deadline).toMillis();
            if (remaining <= 0) break;
            try {
                Thread.sleep(Math.min(200, remaining));
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                throw new IOException("Interrupted while waiting for iOS Simulator readiness.", interrupted);
            }
        }
        throw new IOException("iOS Simulator readiness timed out for " + udid + '.', last);
    }
}
