package com.shaft.tools.internal;

import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;

/**
 * TelemetryService handles anonymous usage telemetry for SHAFT Engine.
 * This service is designed to be 100% anonymous and only tracks test run counts.
 * No personal data, machine identifiers, or sensitive information is collected.
 */
public class TelemetryService {
    private static final String TELEMETRY_ENDPOINT = "https://api.counterapi.dev/v2/shaft-engine/test-runs/up";
    private static final Duration REQUEST_TIMEOUT = Duration.ofSeconds(5);

    /**
     * Sends anonymous telemetry data if telemetry is enabled.
     * This method executes asynchronously and will not block test execution.
     * Only a simple counter increment is sent to track test run statistics.
     */
    public static void sendTelemetry() {
        if (!SHAFT.Properties.flags.telemetryEnabled()) {
            ReportManager.logDiscrete("Telemetry is disabled, skipping anonymous usage tracking.");
            return;
        }

        // Execute telemetry in a separate virtual thread to avoid blocking
        Thread.ofVirtual().start(() -> {
            try {
                ReportManager.logDiscrete("Sending anonymous telemetry data...");
                
                HttpClient client = HttpClient.newBuilder()
                        .connectTimeout(REQUEST_TIMEOUT)
                        .build();

                HttpRequest request = HttpRequest.newBuilder()
                        .uri(URI.create(TELEMETRY_ENDPOINT))
                        .timeout(REQUEST_TIMEOUT)
                        .header("User-Agent", "SHAFT-Engine/Anonymous")
                        .POST(HttpRequest.BodyPublishers.noBody())
                        .build();

                HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
                
                if (response.statusCode() >= 200 && response.statusCode() < 300) {
                    ReportManager.logDiscrete("Anonymous telemetry sent successfully.");
                } else {
                    ReportManager.logDiscrete("Failed to send telemetry (HTTP " + response.statusCode() + "). This will not affect test execution.");
                }
                
            } catch (Exception e) {
                // Silently catch all exceptions to ensure telemetry failures never impact test execution
                ReportManager.logDiscrete("Failed to send anonymous telemetry: " + e.getMessage() + ". This will not affect test execution.");
            }
        });
    }
}