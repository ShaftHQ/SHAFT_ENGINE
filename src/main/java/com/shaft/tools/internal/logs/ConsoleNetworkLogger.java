package com.shaft.tools.internal.logs;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.v85.log.Log;
import org.openqa.selenium.devtools.v85.network.Network;
import org.openqa.selenium.devtools.v85.network.model.Request;
import org.openqa.selenium.devtools.v85.network.model.Response;
import org.openqa.selenium.logging.LogEntries;
import org.openqa.selenium.logging.LogEntry;
import org.openqa.selenium.logging.LogType;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;

/**
 * Utility class for capturing console and network logs during test execution,
 * particularly useful for retry scenarios to gather additional debugging information.
 */
public class ConsoleNetworkLogger {
    private static final ThreadLocal<List<String>> consoleLogs = new ThreadLocal<>();
    private static final ThreadLocal<List<String>> networkLogs = new ThreadLocal<>();
    private static final ThreadLocal<DevTools> devTools = new ThreadLocal<>();
    private static final ThreadLocal<Boolean> isLoggingActive = new ThreadLocal<>();
    private static final ConcurrentLinkedQueue<String> retryLogs = new ConcurrentLinkedQueue<>();

    private ConsoleNetworkLogger() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Starts collecting console and network logs for the given WebDriver instance.
     * This should be called when a test retry is initiated.
     *
     * @param driver the WebDriver instance to monitor
     */
    public static void startLoggingForRetry(WebDriver driver) {
        if (driver == null || Boolean.TRUE.equals(isLoggingActive.get())) {
            return;
        }

        try {
            consoleLogs.set(new ArrayList<>());
            networkLogs.set(new ArrayList<>());
            isLoggingActive.set(true);

            // If the driver supports DevTools (Chrome-based browsers)
            if (driver instanceof ChromeDriver chromeDriver) {
                DevTools devToolsInstance = chromeDriver.getDevTools();
                devTools.set(devToolsInstance);
                devToolsInstance.createSession();

                // Enable console logging
                devToolsInstance.send(Log.enable());
                devToolsInstance.addListener(Log.entryAdded(), entry -> {
                    String logMessage = String.format("[%s] %s: %s",
                            entry.getTimestamp().toString(),
                            entry.getLevel().toString(),
                            entry.getText());
                    consoleLogs.get().add(logMessage);
                    retryLogs.add("CONSOLE: " + logMessage);
                });

                // Enable network logging
                devToolsInstance.send(Network.enable(java.util.Optional.empty(), java.util.Optional.empty(), java.util.Optional.empty()));
                
                devToolsInstance.addListener(Network.requestWillBeSent(), request -> {
                    Request req = request.getRequest();
                    String networkLog = String.format("REQUEST: %s %s", req.getMethod(), req.getUrl());
                    networkLogs.get().add(networkLog);
                    retryLogs.add("NETWORK: " + networkLog);
                });

                devToolsInstance.addListener(Network.responseReceived(), response -> {
                    Response resp = response.getResponse();
                    String networkLog = String.format("RESPONSE: %d %s %s", 
                            resp.getStatus(), resp.getStatusText(), resp.getUrl());
                    networkLogs.get().add(networkLog);
                    retryLogs.add("NETWORK: " + networkLog);
                });

                ReportManager.logDiscrete("Started enhanced logging for retry scenario using DevTools");
            } else {
                // Fallback to standard WebDriver logging for non-Chrome browsers
                try {
                    LogEntries consoleEntries = driver.manage().logs().get(LogType.BROWSER);
                    for (LogEntry entry : consoleEntries) {
                        String logMessage = String.format("[%s] %s: %s",
                                entry.getTimestamp(),
                                entry.getLevel().toString(),
                                entry.getMessage());
                        consoleLogs.get().add(logMessage);
                        retryLogs.add("CONSOLE: " + logMessage);
                    }
                } catch (Exception e) {
                    ReportManager.logDiscrete("Standard console logging not available: " + e.getMessage());
                }
                
                ReportManager.logDiscrete("Started basic logging for retry scenario using standard WebDriver logs");
            }
        } catch (Exception e) {
            ReportManager.logDiscrete("Failed to start logging for retry: " + e.getMessage());
            isLoggingActive.set(false);
        }
    }

    /**
     * Stops logging and attaches the collected logs to the test report.
     * This should be called after a retry attempt is completed.
     *
     * @param testMethodName the name of the test method for attachment purposes
     * @param retryAttempt the retry attempt number
     */
    public static void stopLoggingAndAttach(String testMethodName, int retryAttempt) {
        if (!Boolean.TRUE.equals(isLoggingActive.get())) {
            return;
        }

        try {
            // Close DevTools session if active
            DevTools devToolsInstance = devTools.get();
            if (devToolsInstance != null) {
                try {
                    devToolsInstance.close();
                } catch (Exception e) {
                    ReportManager.logDiscrete("Error closing DevTools session: " + e.getMessage());
                }
                devTools.remove();
            }

            // Attach console logs
            List<String> consoleLogsList = consoleLogs.get();
            if (consoleLogsList != null && !consoleLogsList.isEmpty()) {
                String consoleLogsContent = String.join("\n", consoleLogsList);
                InputStream consoleStream = new ByteArrayInputStream(consoleLogsContent.getBytes());
                ReportManagerHelper.attach("Console Logs - Retry #" + retryAttempt, 
                        testMethodName + "_retry_" + retryAttempt + "_console", consoleStream);
                ReportManager.logDiscrete("Attached " + consoleLogsList.size() + " console log entries for retry #" + retryAttempt);
            }

            // Attach network logs
            List<String> networkLogsList = networkLogs.get();
            if (networkLogsList != null && !networkLogsList.isEmpty()) {
                String networkLogsContent = String.join("\n", networkLogsList);
                InputStream networkStream = new ByteArrayInputStream(networkLogsContent.getBytes());
                ReportManagerHelper.attach("Network Logs - Retry #" + retryAttempt, 
                        testMethodName + "_retry_" + retryAttempt + "_network", networkStream);
                ReportManager.logDiscrete("Attached " + networkLogsList.size() + " network log entries for retry #" + retryAttempt);
            }

            // Attach combined retry logs for easier analysis
            if (!retryLogs.isEmpty()) {
                List<String> allRetryLogs = new ArrayList<>(retryLogs);
                String combinedLogs = String.join("\n", allRetryLogs);
                InputStream combinedStream = new ByteArrayInputStream(combinedLogs.getBytes());
                ReportManagerHelper.attach("Combined Retry Evidence - Retry #" + retryAttempt, 
                        testMethodName + "_retry_" + retryAttempt + "_combined", combinedStream);
            }

        } catch (Exception e) {
            ReportManager.logDiscrete("Error attaching retry logs: " + e.getMessage());
        } finally {
            // Clean up ThreadLocal variables
            consoleLogs.remove();
            networkLogs.remove();
            isLoggingActive.remove();
            retryLogs.clear();
        }
    }

    /**
     * Checks if logging is currently active for this thread.
     *
     * @return true if logging is active, false otherwise
     */
    public static boolean isLoggingActive() {
        return Boolean.TRUE.equals(isLoggingActive.get());
    }

    /**
     * Gets the current console logs collected in this session.
     *
     * @return list of console log messages
     */
    public static List<String> getCurrentConsoleLogs() {
        List<String> logs = consoleLogs.get();
        return logs != null ? new ArrayList<>(logs) : new ArrayList<>();
    }

    /**
     * Gets the current network logs collected in this session.
     *
     * @return list of network log messages
     */
    public static List<String> getCurrentNetworkLogs() {
        List<String> logs = networkLogs.get();
        return logs != null ? new ArrayList<>(logs) : new ArrayList<>();
    }
}