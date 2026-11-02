package com.shaft.gui.browser.internal;

import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.Command;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Applies browser network profiles through Selenium DevTools when the active driver supports them.
 */
public final class BrowserNetworkProfileManager {
    private BrowserNetworkProfileManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Enables offline browser networking.
     *
     * @param driver active WebDriver
     * @return {@code true} when the profile was applied
     */
    public static boolean goOffline(WebDriver driver) {
        DevTools devTools = devTools(driver, "offline network mode");
        if (devTools == null) {
            return false;
        }
        enableNetwork(devTools);
        emulateNetwork(devTools, true, 0, 0, 0);
        return true;
    }

    /**
     * Restores default browser networking and clears blocked URL patterns.
     *
     * @param driver active WebDriver
     * @return {@code true} when the profile was restored
     */
    public static boolean restore(WebDriver driver) {
        DevTools devTools = devTools(driver, "network profile restore");
        if (devTools == null) {
            return false;
        }
        enableNetwork(devTools);
        emulateNetwork(devTools, false, 0, -1, -1);
        devTools.send(new Command<>("Network.setBlockedURLs", Map.of("urls", List.of())));
        return true;
    }

    /**
     * Applies fixed latency and throughput limits.
     *
     * @param driver       active WebDriver
     * @param latencyMs    network latency in milliseconds
     * @param downloadKbps download throughput in kilobits per second
     * @param uploadKbps   upload throughput in kilobits per second
     * @return {@code true} when the profile was applied
     */
    public static boolean throttle(WebDriver driver, long latencyMs, long downloadKbps, long uploadKbps) {
        if (latencyMs < 0 || downloadKbps < 0 || uploadKbps < 0) {
            throw new IllegalArgumentException("Network throttle values must be greater than or equal to zero.");
        }
        DevTools devTools = devTools(driver, "network throttling");
        if (devTools == null) {
            return false;
        }
        enableNetwork(devTools);
        emulateNetwork(devTools, false, latencyMs, kilobitsToBytes(downloadKbps), kilobitsToBytes(uploadKbps));
        return true;
    }

    /**
     * Blocks browser requests whose URLs match the supplied DevTools patterns.
     *
     * @param driver      active WebDriver
     * @param urlPatterns DevTools URL patterns
     * @return {@code true} when the patterns were applied
     */
    public static boolean blockResources(WebDriver driver, String... urlPatterns) {
        DevTools devTools = devTools(driver, "resource blocking");
        if (devTools == null) {
            return false;
        }
        enableNetwork(devTools);
        List<String> patterns = urlPatterns == null ? List.of() : Arrays.stream(urlPatterns)
                .filter(pattern -> pattern != null && !pattern.isBlank())
                .toList();
        devTools.send(new Command<>("Network.setBlockedURLs", Map.of("urls", patterns)));
        return true;
    }

    private static DevTools devTools(WebDriver driver, String capability) {
        if (!(driver instanceof HasDevTools hasDevTools)) {
            String message = "Browser " + capability + " requires a DevTools-capable Selenium driver.";
            BrowserObservabilityRecorder.recordWarning("network", message);
            ReportManager.logDiscrete(message);
            return null;
        }
        DevTools devTools = hasDevTools.getDevTools();
        devTools.createSessionIfThereIsNotOne();
        return devTools;
    }

    private static void enableNetwork(DevTools devTools) {
        devTools.send(new Command<>("Network.enable", Map.of()));
    }

    private static void emulateNetwork(DevTools devTools, boolean offline, long latencyMs,
                                       long downloadBytesPerSecond, long uploadBytesPerSecond) {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("offline", offline);
        params.put("latency", latencyMs);
        params.put("downloadThroughput", downloadBytesPerSecond);
        params.put("uploadThroughput", uploadBytesPerSecond);
        devTools.send(new Command<>("Network.emulateNetworkConditions", params));
    }

    private static long kilobitsToBytes(long kilobitsPerSecond) {
        return (kilobitsPerSecond * 1024L) / 8L;
    }
}
