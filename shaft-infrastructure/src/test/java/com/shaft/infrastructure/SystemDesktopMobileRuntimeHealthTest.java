package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.time.Duration;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SystemDesktopMobileRuntimeHealthTest {

    @Test
    void awaitSimulatorReturnsWhenDeviceReportsBooted() {
        AtomicInteger polls = new AtomicInteger();
        DesktopMobileDeviceController devices = new DesktopMobileDeviceController() {
            @Override
            public SimulatorState simulatorState(String udid) {
                polls.incrementAndGet();
                return SimulatorState.BOOTED;
            }
        };
        SystemDesktopMobileRuntimeHealth health = new SystemDesktopMobileRuntimeHealth(
                unusedAppium(), devices);

        assertDoesNotThrow(() -> health.awaitSimulator("UDID-1", Duration.ofSeconds(1)));
        assertTrue(polls.get() >= 1);
    }

    @Test
    void awaitSimulatorTimesOutWhenNeverBooted() {
        DesktopMobileDeviceController devices = new DesktopMobileDeviceController() {
            @Override
            public SimulatorState simulatorState(String udid) {
                return SimulatorState.SHUTDOWN;
            }
        };
        SystemDesktopMobileRuntimeHealth health = new SystemDesktopMobileRuntimeHealth(
                unusedAppium(), devices);

        IOException failure = assertThrows(IOException.class,
                () -> health.awaitSimulator("UDID-2", Duration.ofMillis(50)));
        assertTrue(failure.getMessage().contains("timed out"));
        assertTrue(failure.getMessage().contains("UDID-2"));
    }

    @Test
    void awaitSimulatorSurfacesDeviceLookupFailuresUntilTimeout() {
        DesktopMobileDeviceController devices = new DesktopMobileDeviceController() {
            @Override
            public SimulatorState simulatorState(String udid) throws IOException {
                throw new IOException("simctl missing for " + udid);
            }
        };
        SystemDesktopMobileRuntimeHealth health = new SystemDesktopMobileRuntimeHealth(
                unusedAppium(), devices);

        IOException failure = assertThrows(IOException.class,
                () -> health.awaitSimulator("UDID-3", Duration.ofMillis(40)));
        assertTrue(failure.getMessage().contains("timed out"));
        assertTrue(failure.getCause().getMessage().contains("simctl missing"));
    }

    @Test
    void nullDependenciesAreRejected() {
        assertThrows(NullPointerException.class,
                () -> new SystemDesktopMobileRuntimeHealth(null, new DesktopMobileDeviceController() {
                }));
        assertThrows(NullPointerException.class,
                () -> new SystemDesktopMobileRuntimeHealth(unusedAppium(), null));
    }

    private static SystemAndroidRuntimeHealth unusedAppium() {
        return new SystemAndroidRuntimeHealth(
                (command, workingDirectory, environment, removed, input, log, timeout) -> {
                    throw new UnsupportedOperationException("unused in simulator tests");
                },
                java.net.http.HttpClient.newHttpClient());
    }
}
