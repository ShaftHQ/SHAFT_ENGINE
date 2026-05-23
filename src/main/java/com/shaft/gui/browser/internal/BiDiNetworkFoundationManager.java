package com.shaft.gui.browser.internal;

import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.RemoteWebDriver;

import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Internal manager for WebDriver BiDi network artifacts (interceptors/listeners).
 * Keeps per-thread and per-driver isolation and provides deterministic cleanup.
 */
public final class BiDiNetworkFoundationManager {
    private static final ThreadLocal<Map<String, DriverNetworkState>> DRIVER_STATE =
            ThreadLocal.withInitial(ConcurrentHashMap::new);

    private BiDiNetworkFoundationManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Registers a network interceptor handle for the active driver context.
     *
     * @param driver            the active WebDriver session
     * @param interceptorHandle the interceptor handle to manage
     * @return generated registration identifier
     */
    public static String registerInterceptor(WebDriver driver, AutoCloseable interceptorHandle) {
        Objects.requireNonNull(driver, "driver cannot be null");
        Objects.requireNonNull(interceptorHandle, "interceptorHandle cannot be null");
        String registrationId = UUID.randomUUID().toString();
        DriverNetworkState state = DRIVER_STATE.get().computeIfAbsent(resolveDriverKey(driver), key -> new DriverNetworkState());
        state.interceptors.put(registrationId, interceptorHandle);
        return registrationId;
    }

    /**
     * Removes a previously registered interceptor handle and closes it.
     *
     * @param driver         the active WebDriver session
     * @param registrationId the registration identifier returned at registration time
     * @return {@code true} when a handle was found and removed, otherwise {@code false}
     */
    public static boolean removeInterceptor(WebDriver driver, String registrationId) {
        return removeHandle(driver, registrationId, true);
    }

    /**
     * Registers a network listener handle for the active driver context.
     *
     * @param driver         the active WebDriver session
     * @param listenerHandle the listener handle to manage
     * @return generated registration identifier
     */
    public static String registerListener(WebDriver driver, AutoCloseable listenerHandle) {
        Objects.requireNonNull(driver, "driver cannot be null");
        Objects.requireNonNull(listenerHandle, "listenerHandle cannot be null");
        String registrationId = UUID.randomUUID().toString();
        DriverNetworkState state = DRIVER_STATE.get().computeIfAbsent(resolveDriverKey(driver), key -> new DriverNetworkState());
        state.listeners.put(registrationId, listenerHandle);
        return registrationId;
    }

    /**
     * Removes a previously registered listener handle and closes it.
     *
     * @param driver         the active WebDriver session
     * @param registrationId the registration identifier returned at registration time
     * @return {@code true} when a handle was found and removed, otherwise {@code false}
     */
    public static boolean removeListener(WebDriver driver, String registrationId) {
        return removeHandle(driver, registrationId, false);
    }

    /**
     * Closes and clears all registered network handles for the supplied driver context only.
     *
     * @param driver the active WebDriver session; ignored when {@code null}
     */
    public static void cleanupForDriver(WebDriver driver) {
        if (driver == null) {
            return;
        }
        DriverNetworkState state = DRIVER_STATE.get().remove(resolveDriverKey(driver));
        if (state == null) {
            return;
        }
        closeAll(state.interceptors);
        closeAll(state.listeners);
    }

    /**
     * Closes and clears all registered network handles tracked in the current thread.
     */
    public static void clearCurrentThread() {
        Map<String, DriverNetworkState> stateByDriver = DRIVER_STATE.get();
        stateByDriver.values().forEach(state -> {
            closeAll(state.interceptors);
            closeAll(state.listeners);
        });
        DRIVER_STATE.remove();
    }

    private static boolean removeHandle(WebDriver driver, String registrationId, boolean isInterceptor) {
        Objects.requireNonNull(driver, "driver cannot be null");
        Objects.requireNonNull(registrationId, "registrationId cannot be null");
        String driverKey = resolveDriverKey(driver);
        DriverNetworkState state = DRIVER_STATE.get().get(driverKey);
        if (state == null) {
            return false;
        }
        AutoCloseable handle = isInterceptor ? state.interceptors.remove(registrationId) : state.listeners.remove(registrationId);
        if (handle == null) {
            return false;
        }
        closeQuietly(handle);
        removeDriverStateIfEmpty(driverKey, state);
        return true;
    }

    private static void closeAll(Map<String, AutoCloseable> handles) {
        handles.values().forEach(BiDiNetworkFoundationManager::closeQuietly);
        handles.clear();
    }

    private static void closeQuietly(AutoCloseable handle) {
        try {
            handle.close();
        } catch (Exception e) {
            ReportManager.logDiscrete("BiDi network cleanup warning: " + e.getMessage());
        }
    }

    private static String resolveDriverKey(WebDriver driver) {
        if (driver instanceof RemoteWebDriver remoteWebDriver && remoteWebDriver.getSessionId() != null) {
            return remoteWebDriver.getSessionId().toString();
        }
        return String.valueOf(System.identityHashCode(driver));
    }

    private static void removeDriverStateIfEmpty(String driverKey, DriverNetworkState state) {
        if (state.isEmpty()) {
            DRIVER_STATE.get().remove(driverKey);
        }
    }

    private static class DriverNetworkState {
        private final Map<String, AutoCloseable> interceptors = new ConcurrentHashMap<>();
        private final Map<String, AutoCloseable> listeners = new ConcurrentHashMap<>();

        private boolean isEmpty() {
            return interceptors.isEmpty() && listeners.isEmpty();
        }
    }
}
